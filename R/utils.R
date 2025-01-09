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


#---- consolidate and organize acronyms ----
# TODO: make this into a function
ac_dir <- fs::path("inst", "resources", "acronyms")

# import all acronyms, meanings, and definitions
# (later, the definitions may go in a glossary)
all_entries <- ac_dir |>
  fs::dir_ls(regexp = "\\.csv$") |>
  purrr::map_dfr(read.csv, .id = "source") |>
  dplyr::mutate(source = gsub(".*acronyms/","",source),
                source = gsub("_.*","",source))


acronyms <- all_entries |>
  dplyr::select(-c(All, X)) |>
  dplyr::filter(!is.na(Acronym),
                Acronym != "") |>
  dplyr::mutate(Shared_ac = duplicated(Acronym),
                Shared_mean = duplicated(Meaning),
                Definition = stringr::str_to_sentence(Definition))









