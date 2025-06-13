#' Create and export glossary
#'
export_glossary <- function() {

  # set up fxn that capitalizes first letter of string
  first_capitalize <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }

  ac_dir <- fs::path("inst", "glossary", "formatted_acronym_lists")

  # import all acronyms, meanings, and definitions
  # (later, the definitions may go in a glossary)
  all_entries <- ac_dir |>
    fs::dir_ls(regexp = "\\.csv$") |>
    purrr::map_dfr(read.csv, .id = "source") |>
    dplyr::mutate(source = gsub(".*acronym_lists/","",source),
                  source = gsub("_.*","",source))

  acronyms <- all_entries |>
    dplyr::select(-c(All, X)) |>
    dplyr::filter(!is.na(Acronym),
                  Acronym != "") |>
    dplyr::mutate_all(dplyr::na_if,"") |>
    # add additional entries
    dplyr::add_row(source = NA,
                   Acronym = "JABBA",
                   Meaning = "Just Another Bayesian Biomass Assessment",
                   Definition = NA) |>
    dplyr::add_row(source = NA,
                   Acronym = "AMAK",
                   Meaning = "Assessment model for Alaska",
                   Definition = NA) |>
    dplyr::add_row(source = NA,
                   Acronym = "CEATTLE",
                   Meaning = "Climate enhanced Age-based model with Temperature specific Trophic linkages and Energetics",
                   Definition = NA) |>
    dplyr::mutate(dplyr::across(.cols = tidyr::everything(), trimws),
                  Meaning = gsub("Nino", "Niño", Meaning),
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

  # redundants <- unique_all |>
  #   dplyr::filter(!dplyr::if_all(c(duplicated_ac, duplicated_mean), is.na))

  # export to csv
  # all acronyms
  # dir.create(fs::path("inst/glossary/partially_cleaned_glossary/"))
  # write.csv(unique_all,
  #           file = fs::path("inst/glossary/partially_cleaned_glossary/acronyms_partial_cleaned.csv"))
  # # acronyms with identical acronyms and/or meanings
  # write.csv(redundants |>
  #             dplyr::select(-meaning_lower),
  #           file = fs::path("inst/glossary/partially_cleaned_glossary/acronyms_duplicates.csv"))

  # remove duplicated acronyms as per discussions with Sam, Steve
  unique_all_cleaning <- unique_all |>
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
        "Gulf",
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
                  Meaning = stringr::str_replace(Meaning, "Annual Catch Limits", "Annual Catch Limit"),
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
                  Acronym = ifelse(Acronym == "TnS/TNS", "TNS", Acronym),
                  Meaning = ifelse(Meaning == "Climate and Commununities Initiative", "Climate and Communities Initiative", Meaning),
                  Meaning = ifelse(Acronym == "DMIS", "Data Matching Imputation System", Meaning),
                  Definition = ifelse(Definition == "#VALUE!", NA, Definition),
                  Definition = ifelse(Definition == "(Charter boat)", "Charter boat", Definition),
                  Meaning = ifelse(Meaning == "Fishing Mortality at MSY", "fishing mortality at MSY", Meaning),
                  Meaning = ifelse(Meaning == "Fishing Mortality Rate Yielding OY", "fishing mortality rate yielding OY", Meaning),
                  Meaning = ifelse(Meaning == "General Additive Models", "General Additive Model", Meaning),
                  Meaning = ifelse(Meaning == "Northwest Atlantic Fisheries Organization viii", "Northwest Atlantic Fisheries Organization", Meaning),
                  Meaning = ifelse(Meaning == "Optimum sustainable production, Oregon State Police", "optimum sustainable production", Meaning),
                  Meaning = ifelse(Meaning == "Spawning Abundance at MSY", "spawning abundance at MSY", Meaning),
                  Meaning = ifelse(Meaning == "Spawning Stock Biomass at MSY", "spawning stock biomass at MSY", Meaning),
                  Meaning = ifelse(Acronym == "R", "Programming environment for statistical processing and presentation", Meaning)
                   ) |>
    # add periods to end of Definition
    dplyr::mutate(
      Definition = ifelse(
        stringr::str_sub(Definition, -1) == ".",
        Definition,
        paste0(Definition, ".")
      )
    ) |>
    dplyr::mutate(Definition = first_capitalize(Definition)) |>
    dplyr::mutate_if(is.character, ~stringr::str_replace(., "U.S.", "US")) |>
    # instance in LEC won't update unless done twice
    dplyr::mutate_if(is.character, ~stringr::str_replace(., "U.S.", "US")) |>
    dplyr::mutate_if(is.character, ~stringr::str_replace(., "SBMSY", "SBmsy")) |>
    dplyr::select(2:4)

    # take rows where SSB is in the acronym, change it to SB
  ssb_rows <- unique_all_cleaning |>
    dplyr::filter(grepl('SSB', Acronym)) |>
    dplyr::mutate(Acronym = stringr::str_replace_all(Acronym, "SSB", "SB"))

  unique_all_cleaning2 <- rbind(unique_all_cleaning,
                                         ssb_rows) |>
    # make label column
    dplyr::mutate(Label = tolower(Acronym)) |>
    dplyr::relocate(Label, .after = Acronym) |>
    # keep certain labels uppercase to differentiate from labels with same lowercase acronym
    dplyr::mutate(Label = ifelse(Acronym == "CM", "CM", Label),
                  Label = ifelse(Acronym == "M", "M", Label),
                  Label = ifelse(Acronym == "PPT", "PPT", Label)) |>
    dplyr::arrange(Label) |>
    as.data.frame()

  # rows with meanings that should be all lowercase, labelled by label
  rows_to_lower <- c("M",
                     "aa", "abc", "abm", "ace", "acl", "adp", "aeq", "ais", "alj", "aop", "ap", "ar", "arm", "asc", "atm",
                     "b", "b-oy", "b1", "b2", "ba", "bb", "bc", "bcurrent", "beg", "bet", "bflag", "bkc", "bmsy", "brp", "bts",
                     "c", "cams", "cas", "cdq", "cea", "cfa", "cfs", "cm", "cml", "cmm", "co2", "cp", "cpdf", "cs", "cvoa", "cy",
                     "das", "dea", "deis", "dgn", "dic", "dps", "dtl", "dts", "dwfn",
                     "eam", "ebm", "ebfm", "ec", "ed", "edr", "eej", "ef", "efh", "efh-hapc", "efhca", "eir", "eis", "elaps", "em", "eo", "epr", "esd", "esp", "ewg",
                     "f", "f/v", "fad", "fcurrent", "feis", "fep", "fes", "fis", "fl", "fll", "fm", "fmc", "fmp", "fmu", "fonsi", "fpr", "frfa",
                     "gac", "gam", "gdp", "gf", "ghl", "gis", "gkc", "gm", "gni", "gnp", "goes", "grt",
                     "haccp", "hapc", "hbs", "hc", "hcr", "hg", "hms", "hp",
                     "iba", "ibq", "ica", "id", "iea", "int", "ipa", "ipq", "ipt", "iq", "iqf", "irfa", "iriu", "itq", "iuu",
                     "jai", "jam",
                     "laa", "lc", "le", "lk", "llp", "lng", "loa", "loc", "lof",
                     "m", "m&si", "mc", "mca", "mcd", "mfmt", "mhhw", "mm", "moa", "mou", "mpcc", "mra", "ms", "mse", "msst", "msvpa", "msy", "mt", "mus", "mw",
                     "ne", "ngo", "nlaa",
                     "oa", "oeg", "ofl", "oswag", "otec", "oy",
                     "p*", "pbf", "pbr", "pce", "pdt", "peec", "pid", "pie rule", "ppa", "ppm", "prt", "psc", "pse", "pt",
                     "qa/qc", "qp", "qs",
                     "r/v", "rer", "rffa", "rfma", "rfmo", "rhl", "rir", "rkc", "rkm", "roa", "rod", "rofr", "rov", "rpa", "rpb", "rqe", "rsw", "rta", "rvc",
                     "s-r", "sa", "sap", "sar", "sac", "saw", "sb", "sb", "sbrm", "sbtarget", "sca", "scs", "seg", "sep", "set", "sfm", "sg", "sia", "sir", "smp", "snp", "sofi", "soi", "sopp", "srd", "srkw", "srt", "ssbtarget", "ssc", "ssl", "sst", "star", "star panel", "std", "stf", "swac", "swo",
                     "ta", "tac", "tal", "tc", "tk", "tla", "tlas", "tmct", "total catch oy",
                     "u/a",
                     "vpa", "vtr",
                     "wets", "wpue",
                     "xbt",
                     "yfs", "ypr",
                     "z"
                     )

  unique_all_cleaning3 <- unique_all_cleaning2 |>
    dplyr::mutate(Meaning = ifelse(Label %in% rows_to_lower,
                  tolower(Meaning),
                  Meaning))

  # keep cleaning by:
  # -adding new definitions

  # NOTE: The above definitions are not finalized.

  # Export to csv
  # all acronyms
  # write.csv(unique_all_cleaning3 |>
  # dplyr::select(Acronym, Meaning, Definition),
  #           file = fs::path("inst/glossary/cleaned_acronyms.csv"))

  # acronyms that need definitions written
  # need_defs <- unique_all_cleaning3 |>
  #   dplyr::filter(is.na(duplicated_ac),
  #                 is.na(duplicated_mean),
  #                 is.na(Definition)) |>
  #   dplyr::select(Acronym, Meaning, Definition)
  #
  # write.csv(need_defs,
  #           file = fs::path("inst/glossary/partially_cleaned_glossary/acronyms_need_definitions.csv"))

  # Convert df into .tex file format and remove definitions
  sink(fs::path("inst/glossary/report_glossary.tex"))
  tex_acs <- unique_all_cleaning3 |>
    dplyr::select(-Definition) |>
    # remove rows causing issues for now
    dplyr::filter(!Acronym %in% c("B25%", "B40%", "F30% SPR")) |>
   # purrr::map_df(~ gsub("%", "\\%", .x, fixed = TRUE)) |>
    dplyr::filter(!is.na(Meaning)) |>
    # properly format terms with sub/superscripts
    dplyr::mutate(
      Acronym = stringr::str_replace_all(Acronym, stringr::regex("msy", ignore_case = TRUE), "_{MSY}"),
      Acronym = ifelse(grepl("\\{MSY\\}", Acronym),
                       paste0("$", Acronym, "$"),
                       Acronym)
    ) |>
    dplyr::mutate(
      Acronym = stringr::str_replace_all(Acronym, stringr::regex("current", ignore_case = TRUE), "_{current}"),
      Acronym = ifelse(grepl("\\{current\\}", Acronym),
                       paste0("$", Acronym, "$"),
                       Acronym)
    ) |>
    dplyr::mutate(
      Acronym = stringr::str_replace_all(Acronym, stringr::regex("target", ignore_case = TRUE), "_{target}"),
      Acronym = ifelse(grepl("\\{target\\}", Acronym),
                       paste0("$", Acronym, "$"),
                       Acronym)
    ) |>
    dplyr::mutate(
      Acronym = stringr::str_replace_all(Acronym, stringr::regex("proxy", ignore_case = TRUE), "_{proxy}"),
      Acronym = ifelse(grepl("\\{proxy\\}", Acronym),
                       paste0("$", Acronym, "$"),
                       Acronym)
    ) |>
    dplyr::mutate(
      Acronym = stringr::str_replace_all(Acronym, stringr::regex("threshold", ignore_case = TRUE), "_{threshold}"),
      Acronym = ifelse(grepl("\\{threshold\\}", Acronym),
                       paste0("$", Acronym, "$"),
                       Acronym)
    ) |>
    dplyr::mutate(
      Acronym = stringr::str_replace_all(Acronym, stringr::regex("max", ignore_case = TRUE), "_{max}"),
      Acronym = ifelse(grepl("\\{max\\}", Acronym),
                       paste0("$", Acronym, "$"),
                       Acronym)
    ) |>
    dplyr::mutate(Acronym = ifelse(Acronym == "$$SSB_{MSY} _{proxy}$$",
                                   "$SSB_{MSY proxy}$",
                                   Acronym),
                  Acronym = ifelse(Acronym == "$$SB_{MSY} _{proxy}$$",
                                   "$SB_{MSY proxy}$",
                                   Acronym),
                  Acronym = ifelse(Acronym == "SSBR",
                                   "$SSB_{R}$",
                                   Acronym),
                  Acronym = ifelse(Acronym == "CO2",
                                   "$CO_{2}$",
                                   Acronym),
                  Meaning = ifelse(Meaning == "C. opilio Bycatch Limitation Zone",
                                   "\\textit{C. opilio} Bycatch Limitation Zone",
                                   Meaning),
                  Acronym = ifelse(Acronym == "$_{MSY}$",
                                   "$MSY$",
                                   Acronym)
                  ) |>
    dplyr::arrange(tolower(Label))


  for(i in 1:dim(tex_acs)[1]) {
    cat(
      paste0(
        "\\newacronym{",
        tex_acs[[2]][[i]],
        "}{",
        tex_acs[[1]][[i]],
        "}{",
        tex_acs[[3]][[i]],
        "}"
      )
    )
    cat("\n")
  }
  sink()

}
