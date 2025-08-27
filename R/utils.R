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
# Function to extract rows then identify and clean up the dfs
SS3_extract_df <- function(dat, label) {
  # Locate the row containing the specified value from the df
  value_row <- which(apply(dat, 1, function(row) any(row == label)))[2]

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
  clean_df <- Filter(function(x) !all(is.na(x)), clean_df)

  clean_df
}

#---- SS3_extract_fleet ----
SS3_extract_fleet <- function(dat, vers) {
  # Determine where fleet names are located base on model version
  # TODO: test other SS3 models and/or write converter based on r4ss::ss_output
  vers <- as.numeric(stringr::str_replace(vers, "3.30.", ""))
  if (vers < 20.00) {
    i = 1
  } else {
    i = 2
  }
  # Locate the row containing the specified value from the df
  value_row <- which(apply(dat, 1, function(row) any(row == "Fleet")))[i]

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
  fleets <- switch(
    i,
    "1" = {
      Filter(function(x) !all(is.na(x)), clean_df)[-1,1]$X1
    },
    {
      clean_df <- dat[rows[1]:(rows[2] - 1), ] |>
        naniar::replace_with_na_all(condition = ~ .x == "")
      fleet_info <- Filter(function(x) !all(is.na(x)), clean_df)[-1,]
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

# create notin operator
`%notin%` <- Negate(`%in%`)
