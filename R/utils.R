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
  fleets <- switch(i,
    "1" = {
      Filter(function(x) !all(is.na(x)), clean_df)[-1, 1]$X1
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
