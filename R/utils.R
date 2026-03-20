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

#---- SS3_extract_df ----
# Helper for SS3 output converter
# Function to extract rows, identify the dfs, and clean them up
# SS3_extract_df <- function(dat, label) {
#   # Locate the row containing the specified value from the df
#   value_row <- which(apply(dat, 1, function(row) any(row == label)))[2]

#   # If the parameter value is not found, return NA
#   if (is.na(value_row)) {
#     cli::cli_alert_warning("Label not found in data frame.")
#     return(NA)
#   }
#   # Search for the next blank row after the value
#   next_blank <- which(apply(dat, 1, function(row) all(is.na(row) | row == "" | row == "-" | row == "#")) & (seq_len(nrow(dat)) > value_row))[1]
#   if (is.na(next_blank)) {
#     next_blank <- nrow(dat)
#   }
#   # Combine the rows surrounding the selected metric from the output table
#   rows <- c(value_row, next_blank)

#   # Extract the metric using the rows from above as a guide and clean up empty columns
#   clean_df <- dat[rows[1]:(rows[2] - 1), ] |>
#     naniar::replace_with_na_all(condition = ~ .x == "")
#   clean_df <- Filter(function(x) !all(is.na(x)), clean_df)

#   clean_df
# }

SS3_extract_df <- function(dat, label) {
  # Convert to data.table if not already
  if (!inherits(dat, "data.table")) {
    dat <- data.table::as.data.table(dat)
  }

  # Identify all values to be treated as NA
  na_values <- c("", "-", "#")

  # Find the row indices using a vectorized approach
  start_row <- which(apply(dat, 1, function(row) any(row == label)))[2]

  # If the label is not found or is not the second instance, return NA
  if (is.na(start_row)) {
    cli::cli_alert_warning("Label not found in data frame or less than two instances were found.")
    return(NA)
  }

  # Find the next blank row after the starting row
  next_blank_rows <- which(apply(dat, 1, function(row) all(is.na(row) | row %in% na_values)))

  # Find the first blank row that appears after the start_row
  end_row <- next_blank_rows[which(next_blank_rows > start_row)[1]]
  if (is.na(end_row) || length(end_row) == 0) {
    end_row <- nrow(dat)
  }

  # Extract the subset
  clean_dt <- data.table::as.data.table(dat[start_row:(end_row - 1), ])

  # Efficiently replace specified values with NA
  for (j in names(clean_dt)) {
    data.table::set(clean_dt, i = which(clean_dt[[j]] %in% na_values), j = j, value = NA)
  }

  # Identify columns to remove and subset the data.table
  # This is the key optimization for removing NA columns
  cols_to_keep <- which(sapply(clean_dt, function(x) !all(is.na(x))))
  clean_dt <- clean_dt |> dplyr::select(dplyr::all_of(c(names(cols_to_keep))))

  # Return data
  as.data.frame(clean_dt)
}

#---- SS3_extract_fleet ----
SS3_extract_fleet <- function(dat, vers) {
  # Determine where fleet names are located base on model version
  # TODO: test other SS3 models and/or write converter based on r4ss::ss_output
  vers <- as.numeric(stringr::str_replace(vers, "3.30.", ""))
  if (vers < 20.00) {
    i <- 1
  } else {
    i <- 2
  }
  # Locate the row containing the specified value from the df
  value_row <- switch(i,
    "1" = which(apply(dat, 1, function(row) any(row == "Fleet")))[1],
    "2" = which(apply(dat, 1, function(row) any(row == "Fleet_name")))[1]
  )

  # If the parameter value is not found, return NA
  if (is.na(value_row)) {
    cli::cli_alert_warning("Label not found in data frame.")
    return(NA)
  }
  # Search for the next blank row after the value
  next_blank <- which(apply(dat, 1, function(row) all(is.na(row) | row == "" | row == "-" | row == "#")) & (seq_len(nrow(dat)) > value_row))[1]
  if (is.na(next_blank)) {
    next_blank <- nrow(dat)
  }
  # Combine the rows surrounding the selected metric from the output table
  rows <- c(value_row, next_blank)

  # Extract the metric using the rows from above as a guide and clean up empty columns
  clean_df <- dat[rows[1]:(rows[2] - 1), ] |>
    naniar::replace_with_na_all(condition = ~ .x == "")
  fleets <- switch(i,
    "1" = {
      fleet_names <- Filter(function(x) !all(is.na(x)), clean_df)[-1, 9]$X9
      fleet_length <- seq_along(fleet_names)
      names(fleet_names) <- fleet_length
      fleet_length
    },
    {
      clean_df <- dat[rows[1]:(rows[2] - 1), ] |>
        naniar::replace_with_na_all(condition = ~ .x == "")
      fleet_info <- Filter(function(x) !all(is.na(x)), clean_df)[-1, ]
      stats::setNames(fleet_info[[ncol(fleet_info)]], fleet_info[[1]])
    }
  )

  if (any(grepl("[0-9]+", fleets))) {
    # Set fleets to another object to use as names of new vector
    # This follows the same naming convention of i = 2
    fleet_name <- fleets
    fleets <- paste0("fleet_", fleets)
    names(fleets) <- fleet_name
  }
  fleets
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
#' version of this function.rom
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

