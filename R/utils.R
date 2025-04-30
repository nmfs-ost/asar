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
create_acronym_table <- function(){

  # set up fxn that capitalizes first letter of string
  first_capitalize <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }

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
                      "F=0",
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

  # remove duplicated acronyms as per discussions with Sam, Steve
  unique_all_cleaned <- unique_all |>
    dplyr::filter(
      !Meaning %in% c(
        "Assistant Administrator",
        "Accumulated Cyclone Energy",
        "annual catch limit",
        "Catch-per-unit-effort",
        "Catch-Per-Unit-Effort",
        "Catcher vessel",
        "Enforcement Consultants, or ecosystem component.",
        "Instantaneous Fishing Mortality Rate",
        "Florida Administrative Code",
        "Fish Aggregating Device",
        "Fishery Management Plan",
        "Individual transferable quota",
        "instantaneous natural mortality rate",
        "Natural mortality rate",
        "Massachusetts Division of Marine Fisheries",
        "DMR Maine Department of Marine Resources",
        "McCracken Estimates",
        "Northwest Region",
        "operational assessment",
        "Pacific Salmon Commission",
        "Subarea as specified by NAFO",
        "Saipan Fishermen's Association",
        "Social impact analysis",
        "Spawning biomass per recruit",
        "Spatial Working Group",
        "United States"
        ),
      !Acronym %in% c(
        "Board",
        "B-CURR",
        "CA",
        "CCA",
        "Council",
        "F-30% SPR",
        "F-CURR",
        "F-MSY",
        "F-OY",
        "LCN",
        "Mid-Atlantic Council",
        "Observer Program",
        "PRD",
        "RF",
        "SAS",
        "SBA",
        "Secretary",
        "SSB",
        "STT"
      ),
      !(source == "PFMC" & Acronym == "ABC"),
      !(source == "SAFMC" & Acronym == "ALS"),
      !(source == "ASMFC" & Meaning == "Accountability Measures"),
      !(source == "ASMFC" & Meaning == "Advisory Panel"),
      !(source == "GMFMC" & Acronym == "ASMFC"),
      !(Acronym == "B" & Meaning == "Biomass"),
      !(source == "ASMFC" & Acronym == "BRD"),
      !(source == "GMFMC" & Acronym == "BRP"),
      !(source == "PFMC" & Acronym == "BiOp"),
      !(Acronym == "C" & Meaning == "Recent Average Catch"),
      !(source == "GMFMC" & Acronym == "CFR"),
      !(source %in% c("PFMC", "WPFMC") & Acronym == "EEZ"),
      !(source == "WPFMC" & Acronym == "ESA"),
      !(source == "PFMC" & Acronym == "F"),
      !(source == "PFMC" & Acronym == "FMSY"),
      !(is.na(source) & Acronym == "FOY"),
      !(is.na(source) & Acronym == "FR"),
      !(Meaning == "Identification" & Acronym == "ID"),
      !(Acronym == "ISC" & Meaning == "International Scientific Committee for Tuna and Tuna-like Species in the North Pacific Ocean"),
      !(Acronym == "MRIP" & source == "ASMFC"),
      !(is.na(source) & Acronym == "MSST"),
      !(source == "PFMC" & Acronym == "MSY"),
      !(source == "ASMFC" & Acronym == "NMFS"),
      !(source == "PFMC" & Acronym == "NOAA"),
      !(source == "ASMFC" & Acronym == "OY"),
      !(source == "GMFMC" & Acronym == "SEDAR"),
      !(source == "ASMFC" & Acronym == "SEFSC"),
      !(source %in% c("PFMC", "WPFMC", "ASMFC") & Acronym == "SPR"),
      !(source %in% c("WPFMC", "GMFMC") & Acronym == "SSC"),
      !(source %in% c("PFMC", "GMFMC") & Acronym == "TAC"),
      !(source == "PFMC" & Acronym == "VMS"),
      !(source == "GMFMC" & Acronym == "VPA"),
      !(source == "PFMC" & Acronym == "mt"),
      !(source == "Spawning Stock Biomass at MSY" & Acronym == "SBMSY")
    ) |>
    dplyr::mutate(Definition = stringr::str_replace_all(Definition,
                                                        "An annual catch level recommended by a Council's SSC. The Council's ACL for a stock may not exceed the ABC recommendation of the SSC for that stock. The SSC's ABC recommendation should incorporate consideration of the stock's life history and reproductive potential, vulnerability to overfishing, and the degree of uncertainty in the science upon which the ABC recommendation is based.",
                                                        "A scientific calculation of the annual catch level recommended by a Council's SSC and is used to set the upper limit of the annual total allowable catch. It is calculated by applying the estimated (or proxy) harvest rate that produces maximum sustainable yield to the estimated exploitable stock biomass (the portion of the fish population that can be harvested)."),
                  Definition = stringr::str_replace_all(Definition,
                                                        "The level of annual catch of a stock or stock complex that serves as the basis for invoking [accountability measures]. ACL cannot exceed the ABC, but may be divided into sector-ACLs.",
                                                        "The level of annual catch, set equal to or below the OFL, of a stock or stock complex that serves as the basis for invoking accountability measures. ACL cannot exceed the ABC, but may be divided into sector-ACLs."),
                  Meaning = stringr::str_replace(Meaning,
                                                 "\\(commercial fishing statistics\\)",
                                                 ""),
                  Definition = stringr::str_replace_all(Definition,
                                                        "panel of members made up of individuals with knowledge and first-hand experience of harvesting Gulf of Mexico managed species and are interested in the conservation and best practices for management of these fishery resources",
                                                        "A group of stakeholders with experience and knowledge of the regional fisheries who provide input into the management process."),
                  Meaning = stringr::str_replace(Meaning, "stock biomass level", "Biomass"),
                  Definition = stringr::str_replace(Definition,
                                                    "unintended capture of marine mammals",
                                                    "unintended capture of non-target species"),
                  Definition = stringr::str_replace(Definition,
                                                    "issued by various NOAA regional offices",
                                                    "A scientific assessment issued by various NOAA regional offices"),
                  Meaning = stringr::str_replace(Meaning, "Average Catch", "Catch"),
                  Meaning = stringr::str_replace(Meaning, "ecosystem component species", "Ecosystem component"),
                  Definition = stringr::str_replace(Definition,
                                                    "U.S. federal waters that extend from 3 to 200 miles from shore. The U.S. has sole management authority of the natural resources found therein.",
                                                    "An area of the ocean, generally extending 200 nautical miles (230 miles) beyond a nation's territorial sea, within which a coastal nation has jurisdiction over both living and nonliving resources."),
                  Definition = stringr::str_replace(Definition,
                                                    "The instantaneous rate at which fish in a stock die because of fishing. Typically includes measured bycatch, if data are available.",
                                                    "The rate at which fish die due to fishing activities."),
                  Meaning = stringr::str_replace(Meaning, "FMSY", "Fishing Mortality at MSY"),
                  Meaning = stringr::str_replace(Meaning, "FOY", "Fishing Mortality Rate Yielding OY"),
                  Definition = stringr::str_replace(Definition,
                                                    "fishing mortality rate corresponding to an equilibrium yield at optimum",
                                                    "fishing mortality rate corresponding to an equilibrium yield that balances ecological, economic, and social goals."),
                  Definition = ifelse(Acronym == "ID",
                                      "Process defining the spatial and temporal extent of a fish population that will be assessed.",
                                      Definition),
                  Definition = ifelse(Acronym == "MSST",
                                      "A threshold biomass used to determine if a stock is overfished.",
                                      Definition),
                  Definition = ifelse(Acronym == "NMFS",
                                      "A division of the U.S. Department of Commerce, National Oceanic and Atmospheric Administration (NOAA). NMFS is responsible for conservation and management of offshore fisheries (and inland salmon) and ecosystems. The NMFS Regional Director is a voting member of the Council.",
                                      Definition),
                  Definition = ifelse(Acronym == "NOAA",
                                      "A federal agency within the Department of Commerce focused on the condition of the oceans and the atmosphere.",
                                      Definition),
                  Definition = ifelse(Acronym == "NWR",
                                      "A network of protected areas in the United States, managed by the U.S. Fish and Wildlife Service, dedicated to the conservation, management, and restoration of fish, wildlife, and plant resources and their habitats.",
                                      Definition),
                  Definition = ifelse(Acronym == "OA",
                                      "A fishery for which entry is not controlled by a limited entry permitting program.",
                                      Definition),
                  Definition = ifelse(Acronym == "PSC",
                                      "The incidental capture of species that must be returned to the sea by law, and cannot be retained for sale or personal use.",
                                      Definition),
                  Definition = ifelse(Acronym == "SA",
                                      "The number of mature fish contributing to the estimate of recruitment.",
                                      Definition),
                  Definition = ifelse(Acronym == "SEDAR",
                                      "The cooperative peer-reviewed process by which stock assessment projects are conducted in NOAA Fisheries’ Southeast Region.",
                                      Definition),
                  Definition = ifelse(Acronym == "SEFSC",
                                      "The center that provides the scientific advice and data needed to effectively manage the living resources of the Southeast Region and Atlantic high seas.",
                                      Definition),
                  Definition = ifelse(Acronym == "SOI",
                                      NA,
                                      Definition),
                  Meaning = ifelse(Acronym == "SOI",
                                      "Statistics of income",
                                   Meaning),
                  Definition = ifelse(Acronym == "SPR",
                                      "The ratio of the number of eggs that could be produced by a fish over its lifetime that has recruited to a fishery, over the number of eggs that could be produced by an average fish in a stock that is unfished. It can be used to measure the effects of fishing pressure on a stock by expressing the spawning potential of the fished biomass as a percentage of the unfished virgin spawning biomass.",
                                      Definition),
                  Definition = ifelse(Acronym == "SB",
                                      "The total weight of the mature females, or mature females and males, depending on the species, that are reproducing in a given season (sometimes measured in egg production).",
                                      Definition),
                  Definition = ifelse(Acronym == "SSC",
                                      "An advisory committee of a regional fishery management council composed of scientists, economists, and other technical experts that peer review statistical, biological, ecological, economic, social, and other scientific information that is relevant to the management of council fisheries, and provides preliminary policy language to the full council for consideration.",
                                      Definition),
                  Meaning = ifelse(Acronym == "mt", "Metric ton", Meaning),
                  Acronym = ifelse(Acronym == "SS", "SS3", Acronym),
                  Definition = ifelse(Acronym == "VPA",
                                      "A fisheries stock assessment cohort modeling method that reconstructs historical fish population structure by analyzing catch data and mortality rates to estimate past population sizes and fishing mortality rates.",
                                      Definition),
                  Acronym = ifelse(Acronym == "Bo (B sub zero)", "B0", Acronym),
                  Acronym = ifelse(Acronym == "BCURRENT", "Bcurrent", Acronym),
                  Acronym = ifelse(Acronym == "BFLAG", "Bflag", Acronym),
                  Acronym = ifelse(Acronym == "B MAX", "Bmax", Acronym),
                  Acronym = ifelse(Acronym == "B MSY", "Bmsy", Acronym),
                  Acronym = ifelse(Acronym == "F MAX", "Fmax", Acronym),
                  Acronym = ifelse(Acronym == "FCURR", "Fcurrent", Acronym),
                  Acronym = ifelse(Acronym == "FMSY", "Fmsy", Acronym),
                  Acronym = ifelse(Acronym == "L MAX", "Lmax", Acronym),
                  Acronym = ifelse(Acronym == "SBTarget", "SBtarget", Acronym),
                  Acronym = ifelse(Acronym == "SBThreshold", "SBthreshold", Acronym),
                  Acronym = ifelse(Acronym == "SSBTarget", "SSBtarget", Acronym),
                  Acronym = ifelse(Acronym == "SSBThreshold", "SSBthreshold", Acronym),
                  Acronym = ifelse(Acronym == "TMAX", "Tmax", Acronym),
                  Acronym = ifelse(Acronym == "TMIN", "Tmin", Acronym),
                  Acronym = ifelse(Acronym == "TTARGET", "Ttarget", Acronym),
                  Meaning = ifelse(Meaning == "#VALUE!", NA, Meaning),
                  Definition = ifelse(Definition == "#VALUE!", NA, Definition)

    ) |>
    # add periods to end of Definition
    dplyr::mutate(
      Definition = ifelse(
        stringr::str_sub(Definition, -1) == ".",
        Definition,
        paste0(Definition, ".")
      )
    ) |>
    dplyr::mutate(Meaning = first_capitalize(Meaning),
                  Definition = first_capitalize(Definition)) |>
    dplyr::mutate_if(is.character, ~stringr::str_replace(., "U.S.", "US")) |>
    # instance in LEC won't update unless done twice
    dplyr::mutate_if(is.character, ~stringr::str_replace(., "U.S.", "US")) |>
    dplyr::mutate_if(is.character, ~stringr::str_replace(., "SBMSY", "SBmsy")) |>
    dplyr::select(2:4)

  # take rows where SSB is in the acronym, change it to SB, and add to main df
  ssb_rows <- unique_all_cleaned |>
    dplyr::filter(grepl('SSB', Acronym)) |>
    dplyr::mutate(Acronym = stringr::str_replace_all(Acronym, "SSB", "SB"))

  unique_all_cleaned <- unique_all_cleaned |>
    dplyr::full_join(ssb_rows) |>
    # add periods between acronym letters for acronyms that could be recognized as words
    dplyr::mutate(
      Acronym = ifelse(Acronym == "ACE", "A.C.E.", Acronym),
      Acronym = ifelse(Acronym == "ACT", "A.C.T.", Acronym),
      Acronym = ifelse(Acronym == "AM", "A.M.", Acronym),
      Acronym = ifelse(Acronym == "ARM", "A.R.M.", Acronym),
      Acronym = ifelse(Acronym == "AS", "A.S.", Acronym),
      Acronym = ifelse(Acronym == "BEG", "B.E.G.", Acronym),
      Acronym = ifelse(Acronym == "BET", "B.E.T.", Acronym),
      Acronym = ifelse(Acronym == "CITES", "C.I.T.E.S.", Acronym),
      Acronym = ifelse(Acronym == "COP", "C.O.P.", Acronym),
      Acronym = ifelse(Acronym == "FATE", "F.A.T.E.", Acronym),
      Acronym = ifelse(Acronym == "GOES", "G.O.E.S.", Acronym),
      Acronym = ifelse(Acronym == "HOT", "H.O.T.", Acronym),
      Acronym = ifelse(Acronym == "ID", "I.D.", Acronym),
      Acronym = ifelse(Acronym == "JAM", "J.A.M.", Acronym),
      Acronym = ifelse(Acronym == "LAMP", "L.A.M.P.", Acronym),
      Acronym = ifelse(Acronym == "LEAP", "L.E.A.P.", Acronym),
      Acronym = ifelse(Acronym == "ME", "M.E.", Acronym),
      Acronym = ifelse(Acronym == "MEW", "M.E.W.", Acronym),
      Acronym = ifelse(Acronym == "NOVA", "N.O.V.A.", Acronym),
      Acronym = ifelse(Acronym == "OLE", "O.L.E.", Acronym),
      Acronym = ifelse(Acronym == "OR", "O.R.", Acronym),
      Acronym = ifelse(Acronym == "POP", "P.O.P.", Acronym),
      Acronym = ifelse(Acronym == "SAFE", "S.A.F.E.", Acronym),
      Acronym = ifelse(Acronym == "SAP", "S.A.P.", Acronym),
      Acronym = ifelse(Acronym == "SAW", "S.A.W.", Acronym),
      Acronym = ifelse(Acronym == "SET", "S.E.T.", Acronym),
      Acronym = ifelse(Acronym == "SPLASH", "S.P.L.A.S.H.", Acronym),
      Acronym = ifelse(Acronym == "STAR", "S.T.A.R.", Acronym),
      Acronym = ifelse(Acronym == "US", "U.S.", Acronym),
      Acronym = ifelse(Acronym == "WHAM", "W.H.A.M.", Acronym)
    )

  # keep cleaning by:
  # -adding new definitions

  # Export to csv
  # write.csv(unique_all_cleaned |>
  # dplyr::select(Acronym, Meaning, Definition),
  #           file = paste0(ac_dir, "/", "final_files", "/", "cleaned_acronyms.csv"))

  # Convert df into .tex file format
  # sink(paste0(ac_dir, "/", "final_files", "/", "report_glossary.tex"))
  # tex_acs <- purrr::map_df(unique_all_cleaned, ~ gsub("%", "\\%", .x, fixed = TRUE))
  # for(i in 1:dim(tex_acs)[1]) {
  #   cat(
  #     paste0(
  #       "\\newacronym{",
  #       tolower(tex_acs[[1]][[i]]),
  #       "}{",
  #       tex_acs[[1]][[i]],
  #       "}{",
  #       tex_acs[[2]][[i]],
  #       "}{",
  #       tex_acs[[3]][[i]],
  #       "}"
  #     )
  #   )
  #   cat("\n")
  #   }
  # sink()

  }

# create_acronym_table()

# acronyms that need definitions written
# need_defs <- unique_all_cleaned |>
#   dplyr::filter(is.na(duplicated_ac),
#                 is.na(duplicated_mean),
#                 is.na(Definition)) |>
#   dplyr::select(Acronym, Meaning, Definition)
#
# write.csv(need_defs,
#           "acronyms_need_definitions.csv")
