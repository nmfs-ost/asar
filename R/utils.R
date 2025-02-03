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

#---- Identify table width class ----
ID_tbl_width_class <- function(
    plot_name, # e.g., "/bnc_table.rda"
    rda_dir,
    portrait_pg_width) {
  rda_path <- file.path(paste0(rda_dir, "/rda_files", plot_name))

  if (file.exists(rda_path)) {
    load(rda_path)
    table_rda <- rda
    rm(rda)
    table_width <- flextable::flextable_dim(table_rda$table)[["widths"]] |>
      as.numeric()

    # determine table width class
    if (table_width > portrait_pg_width){
      if (table_width > 12) {
        width_class <- "extra-wide"
      } else {
        width_class <- "wide"
      }
    } else {
      width_class <- "regular"
    }

  } else {
    width_class <- "regular"
  }

  return(width_class)
}

#---- Identify table width class ----
ID_split_tbls <- function(
    rda_dir,
    plot_name # e.g., "/bnc_table.rda"
    ) {
  rda_path <- file.path(paste0(rda_dir, "/rda_files", plot_name))

  load(rda_path)
  table_rda <- rda
  rm(rda)
  # table_width <- flextable::flextable_dim(table_rda$table)[["widths"]] |>
  #   as.numeric()

  # find number of split tables
  split_tables <- render_lg_table(table_rda$table) |>
    length()

  return(split_tables)
}
