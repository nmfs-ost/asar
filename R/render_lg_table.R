#' Split an extra-wide table into multiple tables
#'
#' @param report_flextable The extra-wide flextable.
#' @param essential_columns The columns that will be retained between the split
#' tables, formatted as a sequence (e.g., 1:2 for columns 1-2, or 1 for a single
#' column. Example: for the indices table, this could be the year column.
#' @param rda_dir Folder where rda files containing alternative text is located
#' @param plot_name Name of the .rda file containing the table
#'
#' @return A list of the split tables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' render_lg_table(
#' report_flextable = indices_table,
#' essential_columns = 1,
#' rda_dir = here::here(),
#' plot_name = "indices.abundance_table.rda")
#'
#' render_lg_table(
#' report_flextable = important_table,
#' essential_columns = 1:3,
#' rda_dir = "data",
#' plot_name = "bnc_table.rda")
#' }
render_lg_table <- function(report_flextable = NULL,
                            essential_columns = NULL,
                            rda_dir = NULL,
                            plot_name = NULL
                            ) {
  # calculate key numbers

  # total columns, width of table
  total_cols <- flextable::ncol_keys(report_flextable)
  total_width <- flextable::flextable_dim(report_flextable)[["widths"]]

  # goal width of each table split from report_flextable, in inches
  goal_width <- 7.5 # max is 8"

  # approx width of each column in report_flextable
  approx_col_width <- total_width / total_cols

  # goal number of columns per table split from report_flextable
  goal_cols_per_table <- ceiling(goal_width / approx_col_width)

  # split tables needed
  num_tables <- ceiling(total_cols / goal_cols_per_table)

  # set up main df organizing split table information
  table_cols <- matrix(NA,
                       nrow = num_tables,
                       ncol = 4) |>
    as.data.frame() |>
    dplyr::rename("table" = 1,
                  "cols_to_keep" = 2,
                  "start_col" = 3,
                  "end_col" = 4)

  i = 1
  essential_cols = essential_columns
  for (i in 1:num_tables){

    # set table number to i
    table_num <- i

    # add table number to df
    table_cols$table[i] <- i

    # get first and last columns of new table
    if(i == 1) {

      init_col <- 1
      end_col <- init_col + goal_cols_per_table

    } else if ((i < num_tables) & (init_col < total_cols)){

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
    i = i + 1
  }

  # add col with essential cols
  table_cols <- table_cols |>
    dplyr::mutate(essential_cols = paste(essential_cols, collapse = ", "),

                  # find cols to delete
                  cols_to_del = apply(table_cols, 1, function(row){
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

  # print all split tables by removing final_cols_to_del from report_flextable
  # for (i in 1:num_tables) {
  #   report_flextable |>
  #     flextable::delete_columns(j = c(
  #       as.numeric(
  #         unlist(
  #           strsplit(
  #             table_cols[i,"final_cols_to_del"], ",")
  #           )
  #         )
  #       )
  #     ) |>
  #       print()
  # }

  # save all tables to a list
  table_list <- list()
  for (i in 1:num_tables) {
    split_table <- report_flextable |>
      flextable::delete_columns(j = c(
        as.numeric(
          unlist(
            strsplit(
              table_cols[i,"final_cols_to_del"], ",")
          )
        )
      )
      ) |>
      flextable::fit_to_width(max_width = goal_width) |>
      flextable::hline(part = "header",
                      # i = 1,
                       border = officer::fp_border(width = 1.5,
                                                   color = "#666666")) |>
      flextable::valign(valign = "center", part = "header")

      table_list[[i]] <- split_table

    # get rownames of split table
    all_vals <- split_table$header$dataset
    shown_vals <- c(split_table[["header"]][["content"]][["keys"]])
    split_tbl_vals <- all_vals |>
      dplyr::select(dplyr::intersect(names(all_vals),
                                     shown_vals)) |>
      dplyr::slice(1) |>
      dplyr::select_if(~!(all(is.na(.)) | all(. == ""))) |>
      as.character() |>
      unique() |>
      stringr::str_squish() |>
      paste(collapse=', ')

    # add rownames to table_list
    names(table_list)[[i]] <- split_tbl_vals


    # if(i == num_tables){
    #   single_tab <- table_list[[returned_tab]]
    #   return(single_tab)
    # }
  }
  # save table_list as rda
  save(table_list,
       file = fs::path(rda_dir,
                       "rda_files",
                       paste0(stringr::str_remove(plot_name, ".rda"), "_split.rda")))

  return(num_tables)
}

