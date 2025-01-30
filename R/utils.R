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
  return(nummax)
}

#---- SS3_extract_df ----
# Helper for SS3 output converter
# Function to extract rows, identify the dfs, and clean them up
SS3_extract_df <- function(dat, label) {
  # Locate the row containing the specified value from the df
  value_row <- which(apply(dat, 1, function(row) any(row == label)))[2]

  # If the parameter value is not found, return NA
  if (is.na(value_row)) {
    message("Label not found in data frame.")
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
  clean_df <- Filter(function(x) !all(is.na(x)), clean_df)

  return(clean_df)
}

# create notin operator
`%notin%` <- Negate(`%in%`)

#---- Render really wide tables ----
# TODO: Put this code into a fxn, then insert into create_tables_doc
render_lg_table <- function(report_flextable) {
  total_cols <- flextable::ncol_keys(report_flextable)
  total_width <- flextable::flextable_dim(report_flextable)[["widths"]]
  goal_width <- 8
  approx_col_width <- total_width / total_cols
  goal_cols_per_table <- ceiling(goal_width / approx_col_width)
  num_tables <- ceiling(total_cols / goal_cols_per_table)


  table_cols <- matrix(NA,
                       nrow = num_tables,
                       ncol = 4) |>
    as.data.frame() |>
    dplyr::rename("table" = 1,
                  "cols_to_keep" = 2,
                  "start_col" = 3,
                  "end_col" = 4)

  i = 1
  essential_cols = 1:2
  for (i in 1:num_tables){

    # set table number to i
    table_num <- i

    # add table number to df
    table_cols$table[i] <- i

    # get first and last columns of new table
    if(i == 1) {

      init_col <- 1
      end_col <- init_col + goal_cols_per_table

    } else if ((i > 1) &
               i < num_tables &
               ((init_col + goal_cols_per_table) <= total_cols)){

      # subtracting essential_cols so the first cols will be the essential ones
      # and the total cols will still = goal_cols_per_table
      end_col <- init_col + goal_cols_per_table - length(essential_cols)

    } else if (i == num_tables){

      end_col <- total_cols

    } else {

      print("Error")

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
    dplyr::mutate(essential_cols = paste(essential_cols, collapse = ", "))

  # find cols to delete
  table_cols$cols_to_del <- apply(table_cols, 1, function(row){
    curr_range <- row["start_col"]:row["end_col"]
    miss_nums <- setdiff(1:total_cols, curr_range)
    paste(miss_nums, collapse = ", ")
  })

  table_cols <- table_cols |>
    dplyr::mutate(
      ctd_num = lapply(cols_to_del, function(x) as.numeric(unlist(strsplit(x, ",")))),
      ec_num = lapply(essential_cols, function(x) as.numeric(unlist(strsplit(x, ",")))),

      final_cols_to_del = mapply(function(seq1, seq2) {
        result <- setdiff(seq1, seq2)
        paste(result, collapse = ", ")
      }, ctd_num, ec_num)
    )

  for (i in 1:num_tables) {
    report_flextable |>
      flextable::delete_columns(j = c(
        as.numeric(
          unlist(
            strsplit(
              table_cols[i,"final_cols_to_del"], ",")
            )
          )
        )
      ) |>
        print()
  }
}

render_lg_table(indices_table)
