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
  dplyr::mutate(across(.cols = everything(), trimws),
                Meaning = gsub("Nino", "Niño", Meaning),
                meaning_lower = tolower(Meaning),
                Shared_ac = duplicated(Acronym),
                Shared_mean = duplicated(meaning_lower)
                )

# min length of consolidated acronyms: ~815
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
                                          "Administrative Procedure Act"),
                Acronym = stringr::str_replace_all(Acronym,
                                                   "BRDs",
                                                   "BRD"),
                Meaning = stringr::str_replace_all(Meaning,
                                                   "Bycatch reduction devices",
                                                   "Bycatch reduction device"),
                Acronym = stringr::str_replace_all(Acronym,
                                                   "Bi Op",
                                                   "BiOp")) |>
  # duplicates without a definition or with nearly identical definitions
  dplyr::filter(!Meaning %in% c("Biomass (in either weight or other appropriate unit)",
                                "Biomass at maximum sustainable yield",
                                "Biomass at MSY"),
                !Acronym %in% "BMSY (B sub MSY)") |>
  # remove ACOE ac without definition, and make entry with def ("COE" --> ACOE)
  dplyr::filter(Acronym != "ACOE") |>
  dplyr::mutate(Acronym = stringr::str_replace(Acronym,
                                                   "COE",
                                                   "ACOE")) |>
  # near-duplicate
  dplyr::filter(
    !Meaning %in% c(
      # duplicates without definitions, near-duplicates, or duplicates that are too specific
      "Catcher-processor",
      "Catch per Unit Effort",
      "Catch Per Unit Effort, sometimes C/E",
      "Code of Federal Regulations 1",
      "Department of Fisheries and Oceans, Canadian",
      "El Niño-Southern Oscillation Index",
      "Evolutionary Significant Unit",
      "Ecosystem-based fishery management",
      "East Pacific Ocean",
      "Division of Fish and Wildlife, Northern Mariana Islands",
      "A measure of the instantaneous rate of fishing mortality",
      "instantaneous rate of fishing mortality",
      "Mortality due to fishing",
      "Food and Agriculture Organization (United Nations)",
      "Fisheries Ecosystem Plan",
      "Fishing Mortality Rate Yielding MSY",
      "Fishing mortality rate to result in the Maximum Sustainable Yield",
      "Finding of No Significant Impact ,",
      "Fish and Wildlife Service",
      "U.S. Fish and Wildlife Service",
      "habitat area of particular concern",
      "(instantaneous) natural mortality rate",
      "Mid-Atlantic Fisheries Management Council",
      "Marine protected areas",
      "Marine Resources Education Program",
      "Marine Recreational Fisheries Survey and Statistics",
      "Marine Recreational Fisheries Statistics Survey",
      "Magnuson-Steven Fishery Conservation and Management Act (Magnuson-Stevens Act)",
      "Magnuson-Stevens Fishery Conservation and Management Act",
      "management strategy evaluations",
      "Magnuson-Stevens Fishery Conservation and Management Reauthorization Act of 2006",
      "New England Fisheries Management Council",
      "Northeast Fisheries Science Centre",
      "Nongovernmental organization",
      "National Marine Fisheries Service, National Oceanic and Atmospheric Department of Commerce. Also, NOAA Fisheries",
      "National Marine Fisheries Service (also known as NOAA Fisheries)",
      "National Oceanic and Atmospheric Administration, U.S. Department of Commerce",
      "National Oceanographic and Atmospheric Administration",
      "National Resource Defense Council",
      "Over Fishing Limit",
      "Overfishing Level",
      "Office of Law Enforcement (NOAA Fisheries)",
      "Office of Law Enforcement, NOAA",
      "Puerto Rico Department of Natural and Environmental",
      "Pacific States Marines Fisheries Commission",
      "quality assurance and quality control",
      "Regional Administrator",
      "reasonably foreseeable future actions",
      "Regional Fishery Management Organizations",
      "Reasonable and prudent alternatives",
      "Stock Assessment and Fishery Evaluation [report]",
      "Stock Assessment and Fishery Evaluation Reports",
      "Small Business Association",
      "standardized bycatch reporting methodology",
      "Southeast Data Assessment and Review",
      "Southeast Data Assessment Review (Stock Assessment)",
      "Sustainable Fisheries Act of 1996",
      "spawning stock biomass consistent with maximum sustainable yield",
      "Spawning Stock Biomass at Maximum Sustainable Yield",
      "Secretary of Commerce",
      "Term of Reference",
      "Caribbean Islands of Puerto Rico, St. Thomas, St. John, and St. Croix",
      "United States Coast Guard",
      "United States Fish and Wildlife Service, Department of Interior",
      "US Fish and Wildlife Service",
      "United States Virgin Islands",
      "Valued Ecosystem Component",
      "Western Pacific Fishery Management Council",
      "yield-per-recruit",
      "kilograms",
      "Meter or meters",
      "Nautical miles",
      "Atlantic States Marine Fisheries Commission (ASMFC)",
      "Distinct Population Segment",
      "Socioeconomic Panel (of the Scientific and Statistical Committee)"),
    !Acronym %in% c("E.O.",
                    "FAG",
                    "NS #",
                    "NS1",
                    "NS2",
                    "NS8",
                    "NSGs",
                    "BO",
                    "DAR",
                    "ITA",
                    "NS#"),
    !(is.na(Definition) & Meaning == "Marine Recreational Fishing Statistical Survey"),
    !(is.na(Definition) & Acronym == "P*"),
    !(source == "GMFMC" & Acronym == "P*"),
    !(is.na(Definition) & Acronym == "RFA")
    ) |> # correct meaning
  dplyr::mutate(Meaning = stringr::str_replace(Meaning,
                                               "Marine Recreational Fisheries Statistical Survey",
                                               "Marine Recreational Fisheries Statistics Survey"),
                Meaning = stringr::str_replace(Meaning,
                                               "National Standards Guidelines",
                                               "National Standard Guidelines")) |>
  dplyr::filter(!Definition %in% "An advisory committee of the PFMC made up of scientists and economists. The Magnuson-Stevens Act requires that each council maintain an SSC to assist in gathering and analyzing statistical, biological, ecological, economic, social, and other scientific information that is relevant to the management of Council fisheries.") |>
  # recreate meaning_lower after updates
  dplyr::mutate(meaning_lower = tolower(Meaning))

dup_acs <- unique_all |>
  dplyr::count(Acronym) |>
  dplyr::filter(n > 1) |>
  dplyr::mutate(duplicated_ac = "Y")

dup_means <- unique_all |>
  dplyr::count(meaning_lower) |>
  dplyr::filter(n > 1) |>
  dplyr::mutate(duplicated_mean = "Y")

unique_all <- unique_all |>
  dplyr::left_join(dup_acs) |>
  dplyr::left_join(dup_means)

redundants <- unique_all |>
  dplyr::filter(!dplyr::if_all(c(duplicated_ac, duplicated_mean), is.na))

# export to csv
# all acronyms
# write.csv(unique_all,
#           file = fs::path("inst/resources/acronyms/intermediary_files/acronyms_partial_cleaned.csv"))
# # acronyms with identical acronyms and/or meanings
# write.csv(redundants |>
#             dplyr::select(-meaning_lower),
#           file = fs::path("inst/resources/acronyms/intermediary_files/acronyms_duplicates.csv"))


# keep cleaning once we collectively decide which duplicated acronym to use,
# and resolve other questions
# also, keep cleaning by:
# -standardizing U.S. vs. US
# -remove "council"; ensure each council's full ac is present
# -
