#' Create Quarto Document of Tables
#'
#' Only tables in an rda format (e.g., my_table.rda) will be imported. Tables in
#' other formats (e.g., .jpg, .png) are not supported; they lack text recognition.
#' See [the `asar` custom figures and tables vignette](https://nmfs-ost.github.io/asar/articles/custom-figs-tabs.html#make-rdas)
#' for more information about making .rda files with custom tables.
#'
#' If your table is too wide to print on a portrait-oriented page,
#' the page will be rotated to landscape view. If if is too wide to print in
#' landscape view, it will be split into multiple tables. In this case, a new rda
#' will be created and is identifiable by the phrase "split" in the filename (e.g.,
#' indices.abundance_table.rda will generate a new indices.abundance_table_split.rda
#' file), and column 1 will be repeated across split tables. These tables will
#' share the same caption. To specify a different repeated column(s), use
#' asar::export_split_tbls with your preferred essential_columns value.
#'
#' @inheritParams create_figures_doc
#' @param tables_dir The location of the "tables" folder, which contains tables
#' files.
#'
#' @return Create a quarto document as part of a stock assessment outline with
#' pre-loaded R chunks that add stock assessment tables from the nmfs-ost/stockplotr
#' R package, or other tables in the same rda format.
#' @export
#'
#' @examples
#' \dontrun{
#' create_tables_doc(
#'   subdir = getwd(),
#'   tables_dir = here::here()
#' )
#' }
create_tables_doc <- function(subdir = getwd(),
                              tables_dir = getwd()) {
  # NOTE: essential_columns = 1 for all tables split using export_split_tbls() in
  # the code below.
  # To customize essential_columns, the user must run export_split_tbls() manually
  # and specify essential_columns. Then, the split table will be imported into
  # the tables doc as is.
  # Upon adding more tables to stockplotr, the code may need to be altered to
  # specify essential_columns for stockplotr-created tables.

  # set portrait page width (in)
  portrait_pg_width <- 5

  # set landscape page width (in)
  landscape_pg_width <- 8

  empty_doc_text <- "Please refer to the `stockplotr` package downloaded from remotes::install_github('nmfs-ost/stockplotr') to add premade tables."

  tab_header <- "# Tables {#sec-tables}\n \n"

  # append table-producing code to non-empty tables doc, if it exists, vs. overwriting it
  append <- FALSE
  if (length(file.path(subdir, list.files(subdir, pattern = "tables.qmd"))) == 1) {
    existing_tables_doc <- file.path(subdir, list.files(subdir, pattern = "tables.qmd"))
    table_content <- readLines(existing_tables_doc) |>
      suppressWarnings()

    if ("# Tables {#sec-tables}" %in% table_content) {
      append <- TRUE
      cli::cli_alert_info("Tables doc will be appended to include tables in `tables_dir`.")
    }
  }

  # add header
  tables_doc_header <- ifelse(append,
    "",
    tab_header
  )

  # add chunk that creates object as the directory of all rdas
  tables_doc_setup <- paste0(
    add_chunk(
      glue::glue(
        "library(flextable)
          tables_dir <- fs::path('{tables_dir}', 'tables')"
      ),
      label = "set-rda-dir-tbls",
      # add_option = TRUE,
      chunk_option = c(
        "echo: false",
        "warning: false",
        "include: false"
      )
    ),
    "\n"
  )

  tables_doc <- ""

  # list all files in tables
  file_list <- list.files(file.path(tables_dir, "tables"))

  # create sublist of only rda table files
  rda_tab_list <- file_list[grepl(".rda", file_list)]

  # remove rda table files that have an associated "split" version
  # remove "_split" from filenames
  remove_split_names <- gsub("_split", "", rda_tab_list)
  # identify duplicates in remove_split_names
  dup_tab <- remove_split_names[duplicated(remove_split_names) | duplicated(remove_split_names, fromLast = TRUE)]
  # remove duplicates in remove_split_names to create final list
  final_rda_tab_list <- rda_tab_list[!(remove_split_names %in% dup_tab & !grepl("_split", rda_tab_list))]

  # create sublist of only non-rda table files
  # non.rda_tab_list <- file_list[!grepl(".rda", file_list)]

  # create two-chunk system to plot each rda table
  create_tab_chunks <- function(tab = NA,
                                tables_dir = getwd()) {
    # test whether table has been split
    split <- grepl("split", tab)

    tab_shortname <- ifelse(split,
      stringr::str_remove(tab, "_table_split.rda"),
      stringr::str_remove(tab, "_table.rda")
    )

    # identify table orientation
    # split tables will always be extra_wide
    tbl_orient <- ifelse(split,
      "extra_wide",
      ID_tbl_width_class(
        plot_name = tab_shortname,
        tables_dir = tables_dir,
        portrait_pg_width = portrait_pg_width
      )
    )

    ## import table, caption
    ## do this for all tables
    tables_doc_plot_setup1 <- paste0(
      add_chunk(
        paste0(
          "# load rda
load(file.path(tables_dir, '", stringr::str_remove(tab, "_split"), "'))\n
# save rda with table-specific name\n",
          tab_shortname, "_table_rda <- rda\n
# save table and caption as separate objects\n",
          tab_shortname, "_table <- ", tab_shortname, "_table_rda$table\n",
          tab_shortname, "_cap <- ", tab_shortname, "_table_rda$cap"
        ),
        label = glue::glue("tab-{tab_shortname}-setup")
      ),
      "\n"
    )

    ## add table if it only requires one chunk
    if (tbl_orient == "regular") {
      tables_doc_plot_setup2 <- paste0(
        add_chunk(
          glue::glue("{tab_shortname}_table"),
          label = glue::glue("tbl-{tab_shortname}"),
          # add_option = TRUE,
          chunk_option = c(
            "echo: false",
            "warnings: false",
            glue::glue(
              "tbl-cap: !expr {tab_shortname}_cap"
            )
          )
        ),
        "\n"
      )
    }

    if (tbl_orient == "wide") {
      tables_doc_plot_setup2 <- paste0(
        # add landscape braces before R chunk
        "::: {.landscape}\n\n",
        add_chunk(
          glue::glue(
            "{tab_shortname}_table |>
                flextable::fit_to_width(max_width = 8)"
          ),
          label = glue::glue("tbl-{tab_shortname}"),
          # add_option = TRUE,
          chunk_option = c(
            "echo: false",
            "warnings: false",
            glue::glue(
              "tbl-cap: !expr {tab_shortname}_cap"
            )
          )
        ),
        "\n",
        # add landscape braces after R chunk
        ":::\n"
      )
    }

    if (tbl_orient == "extra_wide") {
      if (split) {
        # identify number of split tables
        load(fs::path(tables_dir, "tables", tab))
        split_tables <- length(table_list)
      } else {
        # split extra_wide tables into smaller tables and export AND
        # identify number of split tables IF not already split
        split_tables <- export_split_tbls(
          tables_dir = tables_dir,
          plot_name = tab,
          essential_columns = 1
        )
      }

      # add a chunk to import split tables
      tables_doc_plot_setup2_import <- paste0(
        add_chunk(
          paste0(
            "load(file.path(tables_dir, '", tab, "'))\n
# save rda with plot-specific name\n",
            tab_shortname, "_table_split_rda <- table_list\n
# extract table caption specifiers\n",
            tab_shortname, "_cap_split <- names(", tab_shortname, "_table_split_rda)"
          ),
          label = glue::glue("tbl-{tab_shortname}-labels"),
          # add_option = TRUE,
          chunk_option = c(
            "echo: false",
            "warnings: false",
            glue::glue("include: false")
          )
        ),
        "\n"
      )
      # prepare text for chunk that will display split tables
      tables_doc_plot_setup2_display <- ""
      for (i in 1:as.numeric(split_tables)) {
        # add a chunk for each table
        tables_doc_plot_setup2_display <- paste0(
          tables_doc_plot_setup2_display,
          # add landscape braces before R chunk
          "::: {.landscape}\n\n",
          add_chunk(
            paste0(
              "# plot split table ", i, "\n",
              tab_shortname, "_table_split_rda[[", i, "]] |> flextable::fit_to_width(max_width = 8)\n"
            ),
            label = glue::glue("tbl-{tab_shortname}", i),
            add_option = TRUE,
            chunk_option = c(
              "echo: false",
              glue::glue(
                "tbl-cap: !expr paste0({tab_shortname}_cap, '(', {tab_shortname}_cap_split[[", i, "]], ')')"
              )
            )
          ),
          "\n",
          # add landscape braces after R chunk
          ":::\n"
        )
      }

      tables_doc_plot_setup2 <- paste0(
        tables_doc_plot_setup2_import,
        tables_doc_plot_setup2_display
      )
    }

    paste0(
      tables_doc_plot_setup1,
      tables_doc_plot_setup2
    )
  }

  if (length(rda_tab_list) == 0) {
    cli::cli_alert_warning("Found zero tables in an rda format (i.e., .rda) in {fs::path(tables_dir, 'tables')}.",
      wrap = TRUE
    )
    cli::cli_alert_info("For `create_tables_doc` to run properly, there must be:",
      wrap = TRUE
    )
    cli::cli_ol(c(
      "a 'tables' folder in {fs::path(tables_dir)}",
      ".rda files in the 'tables' folder"
    ))
    tables_doc <- paste0(
      tables_doc_header,
      empty_doc_text
    )
  } else {
    cli::cli_alert_success("Found {length(final_rda_tab_list)} table{?s} in an rda format (i.e., .rda) in {fs::path(tables_dir, 'tables')}.",
      wrap = TRUE
    )
    # paste rda table code chunks into one object
    if (length(final_rda_tab_list) > 0) {
      rda_tables_doc <- ""
      for (i in seq_along(final_rda_tab_list)) {
        tab_chunk <- create_tab_chunks(
          tab = final_rda_tab_list[i],
          tables_dir = tables_dir
        )

        rda_tables_doc <- paste0(rda_tables_doc, tab_chunk)
      }
    }
    # if (length(non.rda_tab_list) > 0){
    #   non.rda_tables_doc <- ""
    #   for (i in seq_along(non.rda_tab_list)){
    #     # remove file extension
    #     tab_name <- stringr::str_extract(non.rda_tab_list[i],
    #                                      "^[^.]+")
    #     # remove "_table", if present
    #     tab_name <- sub("_table", "", tab_name)
    #     tab_chunk <- paste0(
    #       "![Your caption here](", fs::path("tables",
    #                                         non.rda_tab_list[i]),
    #       "){#tab-",
    #       tab_name,
    #       "}\n\n"
    #     )
    #
    #     non.rda_tables_doc <- paste0(non.rda_tables_doc, tab_chunk)
    #   }
    # } else {
    #   message(paste0("Note: No table files in a non-rda format (e.g., .jpg, .png) were found in '",  fs::path(tables_dir, "tables") , "'."))
    # }

    # combine tables_doc setup with table chunks
    tables_doc <- paste0(
      tables_doc_header,
      tables_doc_setup,
      ifelse(exists("rda_tables_doc"),
        rda_tables_doc,
        ""
      ) # ,
      # ifelse(exists("non.rda_tables_doc"),
      #        non.rda_tables_doc,
      #        "")
    )
  }

  # Save tables doc to template folder
  utils::capture.output(cat(tables_doc),
    file = paste0(
      subdir, "/",
      ifelse(
        any(grepl("_tables.qmd$", list.files(subdir))),
        list.files(subdir)[grep("_tables.qmd", list.files(subdir))],
        "08_tables.qmd"
      )
    ),
    append = append
  )

  # Read through tables doc and warn about identical labels
  new_tables_doc <- readLines(
    ifelse(
      any(grepl("_tables.qmd$", list.files(subdir))),
      fs::path(subdir, list.files(subdir)[grep("_tables.qmd", list.files(subdir))]),
      fs::path(subdir, "08_tables.qmd")
    )
  ) |>
    suppressWarnings() |>
    as.list()

  label_line_nums <- grep("\\label", new_tables_doc)
  labels <- new_tables_doc[label_line_nums]
  names(labels) <- label_line_nums
  labels <- lapply(labels, function(x) {
    gsub("#\\| label: ", "", x)
  })

  repeated_labels <- labels[duplicated(labels)]
  repeated_labels <- as.vector(unlist(repeated_labels))

  if (length(repeated_labels) > 0) {
    cli::cli_alert_danger("Tables doc contains chunks with identical labels: {repeated_labels}.")
    cli::cli_alert_info("Open tables doc and check for:")
    cli::cli_bullets(c(
      "*" = "Identical, repeated tables",
      "*" = "Different tables with identical labels"
    ))
    cli::cli_alert_warning("Tables doc will not render if chunks have identical labels.")
  }
}
