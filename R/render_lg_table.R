#' Split an extra-wide table into multiple tables
#'
#' @param report_gt The extra-wide gt table.
#' @param essential_columns The columns that will be retained between the split
#' tables, formatted as a sequence (e.g., 1:2 for columns 1-2, or 1 for a single
#' column. Example: for the indices table, this could be the year column.
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

  # Set each col to 1.5"
  col_inches <- 1.5
  
  # calculate key numbers
  # total columns, width of table in inches
  total_cols <- ncol(report_gt[["_data"]])
  total_width <- total_cols * col_inches

  # goal width of each table split from report_gt, in inches
  goal_width <- 7.5 # max is 8"

  # goal number of columns per table split from report_gt
  goal_cols_per_table <- ceiling(goal_width / col_inches)

  # split tables needed
  num_tables <- ceiling(total_cols / goal_cols_per_table)

  # set up main df organizing split table information
  table_cols <- matrix(NA,
    nrow = num_tables,
    ncol = 4
  ) |>
    as.data.frame() |>
    dplyr::rename(
      "table" = 1,
      "cols_to_keep" = 2,
      "start_col" = 3,
      "end_col" = 4
    )

  i <- 1
  # TODO: add error improved error message
  essential_cols <- essential_columns
  for (i in 1:num_tables) {
    # set table number to i
    table_num <- i

    # add table number to df
    table_cols$table[i] <- i

    # get first and last columns of new table
    if (i == 1) {
      init_col <- 1
      end_col <- init_col + goal_cols_per_table
    } else if ((i < num_tables) & (init_col < total_cols)) {
      # subtracting essential_cols so the first cols will be the essential ones
      # and the total cols will still = goal_cols_per_table
      end_col <- init_col + goal_cols_per_table - length(essential_cols)
    } else {
      end_col <- total_cols
    }

    table_cols$cols_to_keep[i] <- paste0(init_col, ":", end_col)

    table_cols$start_col[i] <- init_col
    table_cols$end_col[i] <- end_col

    # add goal_cols_per_table for next table
    init_col <- end_col + 1

    # add 1 for next table
    i <- i + 1
  }

  # add col with essential cols
  table_cols <- table_cols |>
    dplyr::mutate(
      essential_cols = paste(essential_cols, collapse = ", "),

      # find cols to delete
      cols_to_del = apply(table_cols, 1, function(row) {
        curr_range <- row["start_col"]:row["end_col"]
        miss_nums <- setdiff(1:total_cols, curr_range)
        paste(miss_nums, collapse = ", ")
      }),
      # make cols_to_delete and essential_cols into sequences
      cols_to_del_seq = lapply(cols_to_del, function(x) as.numeric(unlist(strsplit(x, ",")))),
      essential_cols_seq = lapply(essential_cols, function(x) as.numeric(unlist(strsplit(x, ",")))),
      # find the final columns to delete by removing essential_cols from
      # cols_to_delete
      final_cols_to_del = mapply(function(seq1, seq2) {
        paste(setdiff(seq1, seq2), collapse = ", ")
      }, cols_to_del_seq, essential_cols_seq)
    )

 
  # save all tables to a list
  table_list <- list()
  for (i in 1:num_tables) {
    split_table <- report_gt |>
      gt::cols_hide(
        columns = all_of(
          as.numeric(
            unlist(
              strsplit(
                table_cols[i, "final_cols_to_del"], ","
                )
              )
            )
          )
       ) |>
      gt::tab_options(table.width = px(goal_width * 96)) |> # in pixels
      gt::tab_style(
        style = cell_borders(
          sides = "bottom",
          color = "#666666",
          weight = px(1.5)
        ),
        locations = cells_column_labels()
      ) |>
      gt::tab_style(
        style = cell_text(v_align = "middle"),
        locations = cells_column_labels()
      )

    table_list[[i]] <- split_table

    # get rownames of split table
    all_vals <- split_table[["_boxhead"]]
    split_tbl_vals <- all_vals |>
      dplyr::filter(type != "hidden") |>
      # Pull the display labels (this handles cases where you renamed columns)
      dplyr::pull(column_label) |>
      as.character() |>
      # Replaces subset() - filter keeps only the values that match
      (\(x) x[!is.na(x) & x != ""])() |> 
      unique() |>
      stringr::str_squish() |>
      paste(collapse = ", ")

    # add rownames to table_list
    names(table_list)[[i]] <- split_tbl_vals
    
  }
  # save table_list as rda
  save(table_list,
    file = fs::path(
      tables_dir,
      "tables",
      paste0(stringr::str_remove(plot_name, ".rda"), "_split.rda")
    )
  )

  num_tables
}
