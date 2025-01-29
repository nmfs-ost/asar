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
total_cols <- ncol_keys(indices_table)
total_width <- flextable::flextable_dim(indices_table)[["widths"]]
goal_width <- 9
approx_col_width <- total_width / total_cols
goal_cols_per_table <- ceiling(goal_width / approx_col_width)
num_tables <- ceiling(total_cols / goal_cols_per_table)


table_cols <- matrix(NA,
                     nrow = num_tables,
                     ncol = 2) |>
  as.data.frame() |>
  dplyr::rename("table" = 1,
                "rows" = 2)

i = 1
essential_cols = 1
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
    end_col <- init_col + goal_cols_per_table - essential_cols

  } else if (i == num_tables){

    end_col <- total_cols

  } else {

    print("Error")

  }

  table_cols$rows[i] <- paste0(init_col, ":", end_col)

  # add goal_cols_per_table for next table
  init_col <- end_col + 1

  # add 1 for next table
  i = i + 1

}

essential_cols_full <- paste0("1:",
                              essential_cols)

if (essential_cols_full == "1:1"){
  essential_cols_full <- 1
}

table_cols <- table_cols |>
  dplyr::mutate(final_cols = paste0("c(",
                                    essential_cols_full,
                                    ",",
                                    rows,
                                    ")"))





indices_table[[table_cols[[1,"final_cols"]]]]
indices_table |>
  flextable::delete_columns(j = c(1,1:8))

indices_table |>
  flextable::delete_columns(j = c(1:10, 20:ncol_keys(indices_table)))
