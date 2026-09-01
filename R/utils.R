#########################
####    Utilities    ####
#########################

#---- get_ncol ----
# Helper for SS3 output converter
# Sourced from r4ss
get_ncol <- function(file, skip = 0) {
  nummax <- max(utils::count.fields(file,
    skip = skip, quote = "",
    comment.char = ""
  )) + 1
  nummax
}

#----------------------------------------------------------

# # Baseline units for models
# baseline_units <- function() {
#   labels <- c(
#     "spawning_biomass",
#     "biomass",
#     "recruitment",
#     "weight"
#   )
# }

#----------------------------------------------------------

# create notin operator
`%notin%` <- Negate(`%in%`)

#----------------------------------------------------------

# gt_split()
#' Split a table into a group of tables (a `gt_group`)
#'
#' @description
#'
#' With a **gt** table, you can split it into multiple tables and get that
#' collection in a `gt_group` object. This function is useful for those cases
#' where you want to section up a table in a specific way and print those
#' smaller tables across multiple pages (in RTF and Word outputs, primarily via
#' \link[gt]{gtsave}, or, with breaks between them when the output context is HTML.
#'
#' @param data *The gt table data object*
#'
#'   `obj:<gt_tbl>` // **required**
#'
#'   This is the **gt** table object that is commonly created through use of the
#'   [gt()] function.
#'
#' @param row_every_n *Split at every n rows*
#'
#'   `scalar<numeric|integer>` // *default:* `NULL` (`optional`)
#'
#'   A directive to split at every *n* number of rows. This argument expects a
#'   single numerical value.
#'
#' @param row_slice_i *Row-slicing indices*
#'
#'   `vector<numeric|integer>` // *default:* `NULL` (`optional`)
#'
#'   An argument for splitting at specific row indices. Here, we expect either a
#'   vector of index values or a function that evaluates to a numeric vector.
#'
#' @param col_slice_at *Column-slicing locations*
#'
#'   `<column-targeting expression>` // *default:* `NULL` (`optional`)
#'
#'   Any columns where vertical splitting across should occur. The splits occur
#'   to the right of the resolved column names. Can either be a series of column
#'   names provided in `c()`, a vector of column indices, or a select helper
#'   function (e.g. \link[gt]{starts_with}, \link[gt]{ends_with}, \link[gt]{contains}, \link[gt]{matches},
#'   \link[gt]{num_range}, and \link[gt]{everything}).
#'
#' @return An object of class `gt_group`.
#'
#' @note
#' This function is a temporary export of asar, but all development and rights
#' belong to `rstudio/gt`. This function provides a fix to the function
#' introduced by a bug in gt v1.3.0. Until this is corrected in the package, we
#' are using the function here. Once this bug is patched, we will deprecate
#' and remove this function from asar and direct users to use the gt package
#' version of this function.
#'
#' @section Examples:
#'
#' Use a subset of the [`gtcars`] dataset to create a **gt** table. Format the
#' `msrp` column to display numbers as currency values, set column widths with
#' \link[gt]{cols_width}, and split the table at every five rows with `gt_split()`.
#' This creates a `gt_group` object containing two tables. Printing this object
#' yields two tables separated by a line break.
#'
#' ```r
#' gtcars |>
#'   dplyr::slice_head(n = 10) |>
#'   dplyr::select(mfr, model, year, msrp) |>
#'   gt() |>
#'   fmt_currency(columns = msrp) |>
#'   cols_width(
#'     year ~ px(80),
#'     everything() ~ px(150)
#'   ) |>
#'   gt_split(row_every_n = 5)
#' ```
#'
#' Use a smaller subset of the [`gtcars`] dataset to create a **gt** table.
#' Format the `msrp` column to display numbers as currency values, set the table
#' width with [tab_options()] and split the table at the `model` column This
#' creates a `gt_group` object again containing two tables but this time we get
#' a vertical split. Printing this object yields two tables of the same width.
#'
#' ```r
#' gtcars |>
#'   dplyr::slice_head(n = 5) |>
#'   dplyr::select(mfr, model, year, msrp) |>
#'   gt() |>
#'   fmt_currency(columns = msrp) |>
#'   tab_options(table.width = px(400)) |>
#'   gt_split(col_slice_at = "model")
#' ```
#'
#' @family table group functions
#' @section Function ID:
#' 14-2
#'
#' @section Function Introduced:
#' `v0.9.0` (Mar 31, 2023)
#'
#' @export
gt_split <- function(
  data,
  row_every_n = NULL,
  row_slice_i = NULL,
  col_slice_at = NULL
) {
  # Perform input object validation
  gt:::stop_if_not_gt_tbl(data = data)

  # Resolution of columns as character vectors
  col_slice_at <-
    gt:::resolve_cols_c(
      expr = {{ col_slice_at }},
      data = data,
      null_means = "nothing"
    )

  gt_tbl_built <- gt:::build_data(data = data, context = "html")

  # Get row count for table (data rows)
  n_rows_data <- nrow(gt_tbl_built[["_stub_df"]])

  row_slice_vec <- rep.int(1L, n_rows_data)

  row_every_n_idx <- NULL
  if (!is.null(row_every_n)) {
    row_every_n_idx <- seq_len(n_rows_data)[seq(0, n_rows_data, row_every_n)]
  }

  row_slice_i_idx <- NULL
  if (!is.null(row_slice_i)) {
    row_slice_i_idx <- row_slice_i
  }

  row_idx <- sort(unique(c(row_every_n_idx, row_slice_i_idx)))

  group_i <- 0L

  for (i in seq_along(row_slice_vec)) {
    if (i %in% (row_idx + 1)) {
      group_i <- group_i + 1L
    }

    row_slice_vec[i] <- row_slice_vec[i] + group_i
  }

  row_range_list <-
    split(
      seq_len(n_rows_data),
      row_slice_vec
    )

  gt_tbl_main <- data

  gt_group <- gt::gt_group(.use_grp_opts = FALSE)

  for (i in seq_along(row_range_list)) {
    gt_tbl_i <- gt_tbl_main

    gt_tbl_i[["_data"]] <- gt_tbl_i[["_data"]][row_range_list[[i]], ]
    gt_tbl_i[["_stub_df"]] <- gt_tbl_i[["_stub_df"]][seq_along(row_range_list[[i]]), ]

    if (!is.null(col_slice_at)) {
      # Get all visible vars in their finalized order
      visible_col_vars <- gt:::dt_boxhead_get_vars_default(data = data)

      # Stop function if any of the columns to split at aren't visible columns
      if (!all(col_slice_at %in% visible_col_vars)) {
        cli::cli_abort(
          "All values provided in `col_slice_at` must correspond to visible columns."
        )
      }

      # Obtain all of the column indices for vertical splitting
      col_idx <- which(visible_col_vars %in% col_slice_at)

      col_slice_vec <- rep.int(1L, length(visible_col_vars))

      group_j <- 0L

      for (i in seq_along(col_slice_vec)) {
        if (i %in% (col_idx + 1)) {
          group_j <- group_j + 1L
        }

        col_slice_vec[i] <- col_slice_vec[i] + group_j
      }

      col_range_list <-
        split(
          seq_along(visible_col_vars),
          col_slice_vec
        )

      for (j in seq_along(col_range_list)) {
        gt_tbl_j <- gt_tbl_i

        gt_tbl_j[["_data"]] <-
          gt_tbl_j[["_data"]][, visible_col_vars[col_range_list[[j]]]]

        gt_tbl_j[["_boxhead"]] <-
          gt_tbl_j[["_boxhead"]][
            gt_tbl_j[["_boxhead"]]$var %in% visible_col_vars[col_range_list[[j]]],
          ]

        gt_group <- gt::grp_add(gt_group, gt_tbl_j)
      }
    } else {
      gt_group <- gt::grp_add(gt_group, gt_tbl_i)
    }
  }

  gt_group
}

#----Fix figures/tables docs with duplicate chunks----

fix_duplicate_chunks <- function(doc_path,
                                 doc_type) {
  new_figs_doc <- readLines(doc_path) |>
    suppressWarnings() |>
    as.list()

  label_line_nums <- grep("\\label", new_figs_doc)
  labels <- new_figs_doc[label_line_nums]
  names(labels) <- label_line_nums
  labels <- lapply(labels, function(x) {
    gsub("#\\| label: ", "", x)
  })

  repeated_labels <- labels[duplicated(labels)]
  repeated_labels <- as.vector(unlist(repeated_labels))

  if (length(repeated_labels) > 0) {
    cli::cli_alert_warning("{doc_type} doc contains chunks with identical labels: {repeated_labels}.")
    cli::cli_alert_warning("{doc_type} doc will not render if chunks have identical labels.")
    cli::cli_alert_info("The duplicate chunks will be commented out.")

    in_chunk <- FALSE
    current_chunk_start <- NA
    current_chunk_label <- NULL

    chunks_list <- list()

    for (i in seq_along(new_figs_doc)) {
      line <- new_figs_doc[i]

      # get code chunk start
      if (!in_chunk && grepl("^\\s*```\\s*\\{[a-zA-Z]", line)) {
        in_chunk <- TRUE
        current_chunk_start <- i
        current_chunk_label <- NULL

        # Handle inline label format: e.g., ```{r my-label}
        inline_match <- regmatches(line, regexec("^\\s*```\\s*\\{[a-zA-Z]+\\s+([^, }]+)", line))[[1]]
        if (length(inline_match) > 1) {
          current_chunk_label <- trimws(inline_match[2])
        }
      }
      # get label
      else if (in_chunk && is.null(current_chunk_label) && grepl("^\\s*#\\|\\s*label:", line)) {
        # extract everything after "label:"
        label_match <- regmatches(line, regexec("^\\s*#\\|\\s*label:\\s*(.*)", line))[[1]]
        if (length(label_match) > 1) {
          current_chunk_label <- trimws(label_match[2])
          # strip outer quotes
          current_chunk_label <- gsub("^['\"]|['\"]$", "", current_chunk_label)
        }
      }
      # find end of code chunk
      else if (in_chunk && grepl("^\\s*```\\s*$", line)) {
        in_chunk <- FALSE

        # get chunk label
        if (!is.null(current_chunk_label)) {
          chunks_list[[length(chunks_list) + 1]] <- tidyr::tibble(
            start = current_chunk_start,
            end = i,
            label = current_chunk_label
          )
        }
      }
    }

    # Combine list elements into a df
    chunks_df <- dplyr::bind_rows(chunks_list)

    # Find duplicate occurrences
    duplicates <- chunks_df |>
      dplyr::group_by(label) |>
      dplyr::mutate(occurrence = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::filter(occurrence > 1) |>
      dplyr::arrange(dplyr::desc(start))

    modified_doc <- new_figs_doc

    # Loop over duplicate chunks and comment out every line in range
    for (row_idx in seq_len(nrow(duplicates))) {
      label <- duplicates$label[row_idx]
      start <- duplicates$start[row_idx]
      end <- duplicates$end[row_idx]

      cli::cli_alert_info(sprintf("Commenting out duplicate chunk '%s' (Lines %d to %d)", label, start, end))

      # comment out each line
      modified_doc[start:end] <- paste0("<!-- ", modified_doc[start:end], " -->")
    }

    writeLines(as.character(unlist(modified_doc)), doc_path)
    cli::cli_alert_success(sprintf("Successfully resolved %d duplicate chunk label issues in {doc_type} doc.", nrow(duplicates)))
  }
}

#----------------------------------------------------------

#' Format author names for a citation
#'
#' Converts author names to the family-name-first format used in report
#' citations. Names containing given and family names are abbreviated using
#' their initials. A name with only one component, such as an initialism or a
#' mononym, is retained as the family name rather than causing name parsing to
#' fail.
#'
#' @param author_names A character vector containing one author name per
#'   element.
#'
#' @return A single character string containing the formatted author names,
#'   separated according to standard citation conventions.
#' @keywords internal
#' @noRd
format_citation_authors <- function(author_names) {
  author_names <- trimws(author_names)
  single_component <- !grepl("\\s", author_names)
  formatted_names <- character(length(author_names))
  
  # A single component cannot be reliably divided into given and family
  # names. Treating it as the family name preserves initials and mononyms.
  formatted_names[single_component] <- vapply(
    author_names[single_component],
    \(name) utils::toBibtex(utils::person(family = name)),
    character(1)
  )
  
  if (any(!single_component)) {
    formatted_names[!single_component] <- data.frame(
      input = author_names[!single_component]
    ) |>
      tidyr::separate_wider_regex(
        cols = input,
        # Caitlin Allen Akselrud is the only non-hyphenated dual last name
        # and needs to be included as its own pattern. The second pattern
        # allows for first initials rather than first names.
        patterns = c(first = "Caitlin |^[A-Z]. |.*[a-z] ", last = ".*$")
      ) |>
      tidyr::separate_wider_delim(
        cols = last,
        delim = ". ",
        names = c("mi", "last"),
        too_few = "align_end"
      ) |>
      dplyr::mutate(
        first = gsub(" ", "", first),
        mi = ifelse(is.na(mi), "", paste0(mi, ".")),
        first_initial = gsub("([A-Z])[a-z]+", "\\1.", first),
        bib = purrr::pmap_chr(
          list(x = first_initial, y = mi, z = last),
          \(x, y, z) {
            utils::toBibtex(
              utils::person(given = c(x, y), family = z)
            )
          }
        )
      ) |>
      dplyr::pull(bib)
  }
  
  formatted_names |>
    # `toBibtex()` adds a comma after a person with only a family name.
    # Remove it along with the trailing whitespace before joining authors.
    sub(pattern = ",?\\s*$", replacement = "") |>
    glue::glue_collapse(sep = ", ", last = ", and ") |>
    as.character()
}

#' Map legacy document names to current document names
#'
#' @return A list containing vector mappings for legacy and current
#'   tables and figures files.
get_doc_order <- function() {
  list(
    legacy_tables  = c("08_tables.qmd",  "05_tables.qmd",  "11_tables.qmd"),
    current_tables = c("09_tables.qmd",  "06_tables.qmd",  "12_tables.qmd"),
    legacy_figs    = c("09_figures.qmd", "06_figures.qmd", "12_figures.qmd"),
    current_figs   = c("08_figures.qmd", "05_figures.qmd", "11_figures.qmd")
  )
}

#' Detect, rename, and resolve legacy document paths
#'
#' @param subdir Directory where template files are located.
#' @param doc_type String. Document type.
#' 
#' Options: "figures", "tables"
#' 
#' @param rerender_skeleton Logical indicating if rerendering active.
#' 
#' Default: FALSE
#'
#' @return A list containing `using_legacy` (logical), `legacy_name`, `current_name`,
#'   and `resolved_name`.
migrate_legacy_docs <- function(subdir,
                                doc_type,
                                rerender_skeleton = FALSE) {
  mapping <- get_doc_order()
  
  # Determine target vectors based on type requested
  if (doc_type == "figures") {
    legacy_docs  <- mapping$legacy_figs
    current_docs <- mapping$current_figs
  } else {
    legacy_docs  <- mapping$legacy_tables
    current_docs <- mapping$current_tables
  }
  
  # Check for existing legacy files in subdir (both figs and tables exist in legacy format)
  legacy_match <- which(
    file.exists(fs::path(subdir, mapping$legacy_figs)) &
      file.exists(fs::path(subdir, mapping$legacy_tables)) &
      !file.exists(fs::path(subdir, mapping$current_figs)) &
      !file.exists(fs::path(subdir, mapping$current_tables))
  )
  
  # Fallback check when called independently per file type
  if (length(legacy_match) == 0) {
    legacy_match <- which(
      file.exists(fs::path(subdir, legacy_docs)) &
        !file.exists(fs::path(subdir, current_docs))
    )
  }
  
  using_legacy <- (rerender_skeleton || doc_type != "skeleton") && length(legacy_match) > 0
  
  if (using_legacy) {
    idx <- legacy_match[1]
    legacy_name  <- legacy_docs[idx]
    current_name <- current_docs[idx]
    
    return(list(
      using_legacy  = TRUE,
      legacy_name   = legacy_name,
      current_name  = current_name,
      resolved_name = current_name
    ))
  }
  
  # Fallback for standard non-legacy paths
  existing_current <- current_docs[file.exists(fs::path(subdir, current_docs))]
  resolved_name <- if (length(existing_current) > 0) {
    existing_current[1]
  } else {
    current_docs[1]
  }
  
  list(
    using_legacy  = FALSE,
    legacy_name   = NULL,
    current_name  = current_docs[1],
    resolved_name = resolved_name
  )
}