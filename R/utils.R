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
    plot_name, # e.g., "bnc_table.rda"
    rda_dir,
    portrait_pg_width) {
  rda_path <- file.path(paste0(rda_dir, "/rda_files/", plot_name))

  if (file.exists(rda_path)) {
    load(rda_path)
    table_rda <- rda
    rm(rda)
    table_width <- flextable::flextable_dim(table_rda$table)[["widths"]] |>
      as.numeric()

    # determine table width class
    if (table_width > portrait_pg_width) {
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

#---- # split extra-wide tables into smaller tables and export ----
# the function returns the number of split tables
export_split_tbls <- function(
    rda_dir,
    plot_name, # e.g., "bnc_table.rda"
    essential_columns) {
  rda_path <- file.path(paste0(rda_dir, "/rda_files/", plot_name))

  load(rda_path)
  table_rda <- rda
  rm(rda)

  # split tables and export
  render_lg_table(
    report_flextable = table_rda$table,
    essential_columns = essential_columns,
    rda_dir = rda_dir,
    plot_name = plot_name
  )

  # return(split_tables)
}

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
  dplyr::mutate_all(dplyr::na_if,"") |>
  dplyr::mutate_all(trimws) |>
  dplyr::mutate(Meaning = gsub("<92>", "'", Meaning),
                Meaning = gsub("<f1>", "ñ", Meaning),
                meaning_lower = tolower(Meaning),
                Shared_ac = duplicated(Acronym),
                Shared_mean = duplicated(meaning_lower)
                )

# min length of consolidated acronyms: ~818
# length(unique(acronyms$Acronym))

# for a given acronym: if there is a row with an identical acronym,
# and there is a row with an identical meaning_lower, and at least one
# row has a definition, then keep the row with the definition
has_definitions_unique <- acronyms |>
  dplyr::group_by(Acronym, meaning_lower) |>
  dplyr::filter(dplyr::if_any(Definition, ~!is.na(.))) |>
  dplyr::slice_max(order_by = !is.na(Definition), n = 1) |>
  dplyr::ungroup()

# anti-join above with main list (grouped by acronym) to find acronyms
# that have no definitions
no_definitions_unique <- acronyms |>
  dplyr::group_by(Acronym, meaning_lower) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::anti_join(has_definitions_unique) |>
  dplyr::left_join(acronyms[c(2:3, 5)]) |>
  dplyr::distinct(Acronym, meaning_lower, n, .keep_all = T)

# join the two dfs
unique_all <- dplyr::full_join(has_definitions_unique,
                               no_definitions_unique) |>
  dplyr::arrange(Acronym) |>
  dplyr::select(source:meaning_lower) |>

# manually clean rows to make some extremely similar rows identical
  dplyr::filter(Acronym != "?",
                # ACE is already used for multiple acronyms; will use ACOE
                !(Acronym == "ACE" & Meaning == "Army Corps of Engineers"),
                # Longer duplicate of AFSC
                !Meaning %in% "Alaska Fisheries Science Center (National Marine Fisheries Service)",
                # Duplicate without a definition
                !Meaning %in% "Administrative Procedure Act") |>
  dplyr::mutate(Meaning = stringr::str_replace_all(Meaning,
                                          "Administrative Procedures Act",
                                          "Administrative Procedure Act")) |>
  # another duplicate without a definition
  dplyr::filter(!Meaning %in% "Biomass (in either weight or other appropriate unit)")

# then, filter out duplicates again

