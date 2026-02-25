#' Split an extra-wide table into multiple tables
#'
#' @param report_gt The extra-wide gt table.
#' @param essential_columns The columns that will be retained between the split
#' tables, formatted as a sequence (e.g., 1:2 for columns 1-2, or 1 for a single
#' column). Example: for the indices table, this could be the year column.
#' @param plot_name Name of the .rda file containing the table
#' @inheritParams create_tables_doc
#'
#' @return A list of the split tables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' render_lg_table(
#'   report_gt = indices_table,
#'   essential_columns = 1,
#'   tables_dir = here::here(),
#'   plot_name = "indices.abundance_table.rda"
#' )
#'
#' render_lg_table(
#'   report_gt = important_table,
#'   essential_columns = 1:3,
#'   tables_dir = "data",
#'   plot_name = "bnc_table.rda"
#' )
#' }
render_lg_table <- function(report_gt,
                            essential_columns,
                            tables_dir,
                            plot_name) {
  
  # Clean up essential_columns
  is_empty_essentials <- is.null(essential_columns) || 
    length(essential_columns) == 0 || 
    (length(essential_columns) == 1 && essential_columns == "0")
  
  if (is_empty_essentials) {
    essential_cols_cleaned <- character(0)
  } else {
    # If numeric (like 1:2), convert to actual column names to prevent index shifting errors
    if (is.numeric(essential_columns)) {
      essential_cols_cleaned <- colnames(report_gt[["_data"]])[essential_columns]
    } else {
      essential_cols_cleaned <- essential_columns
    }
  }
  
  col_inches <- 1.5
  
  # calculate key numbers
  # total columns, width of table in inches
  total_cols <- ncol(report_gt[["_data"]])
  total_width <- total_cols * col_inches
  
  # goal width of each table split from report_gt, in inches
  goal_width <- 7.5 # max is 8"
  
  # goal number of columns per table split from report_gt
  goal_cols_per_table <- ceiling(goal_width / col_inches)
  
  all_cols <- colnames(report_gt[["_data"]])
  moving_cols <- setdiff(all_cols, essential_cols_cleaned)
  
  total_slots <- floor(goal_width / col_inches)
  
  # Slots for non-essential columns
  non_ess_slots <- max(1, total_slots - length(essential_cols_cleaned))
  
  # Chunk the moving columns
  col_chunks <- split(moving_cols,
                      ceiling(seq_along(moving_cols) / non_ess_slots))
  
  # Loop through chunks and build tables
  table_list <- list()
  for (i in seq_along(col_chunks)) {
    
    current_keep <- c(essential_cols_cleaned, col_chunks[[i]])
    
    # Create the gt objects
    split_table <- report_gt |>
      gt::cols_hide(columns = -all_of(current_keep)) |>
      gt::tab_options(table.width = px(goal_width * 96)) |> 
      gt::tab_style(
        style = gt::cell_borders(sides = "bottom", 
                                 color = "#666666",
                                 weight = px(1.5)),
        locations = gt::cells_column_labels()
      ) |>
      gt::tab_style(
        style = gt::cell_text(v_align = "middle"),
        locations = gt::cells_column_labels()
      )
    
    # Store table in list
    table_list[[i]] <- split_table
    
    # Get labels for visible columns only
    all_vals <- split_table[["_boxhead"]]
    
    if (!is.null(all_vals)) {
      display_names <- all_vals |>
        dplyr::filter(type != "hidden") |>
        dplyr::pull(column_label) |>
        as.character() |>
        (\(x) x[!is.na(x) & x != ""])() |> 
        unique() |>
        stringr::str_squish() |>
        paste(collapse = ", ")
      
      # Assign name to list index
      if (nchar(display_names) > 0) {
        names(table_list)[i] <- display_names
      } else {
        names(table_list)[i] <- paste0("Table_Part_", i)
      }
    }
  }

  # save table_list as rda
  save(table_list,
    file = fs::path(
      tables_dir,
      "tables",
      paste0(stringr::str_remove(plot_name, ".rda"), "_split.rda")
    )
  )

  length(table_list)
}
