#' Export key quantities to SIS via Google Drive
#'
#' Submit completed stock assessment data to the Species Information System (SIS)
#' via a Google Drive folder.
#' 
#' @param sis_data_dir Location of the sis_assmt_template.csv and sis_ts_template.csv files,
#' both of which are required.
#' 
#' Default: The working directory.
#'  
#' @details This function requires `sis_assmt_template.csv` and `sis_ts_template.csv` to be
#' present in `sis_data_dir`. You can generate these templates using either:
#' * `asar::create_blank_sis()` for blank templates, or
#' * `stockplotr::extract_sis_data()` for partially populated templates (see the
#'   [Automating data extraction for SIS](https://nmfs-ost.github.io/stockplotr/articles/extract-sis-data.html)
#'   vignette for details).
#'
#' After you populate the templates according to the requirements in their metadata,
#' `export_to_sis()` performs the following steps:
#' 1. Reads in the CSV files and validates required fields, character limits, and
#'    `Primary` time series designations.
#' 2. Formats and exports the combined data as a JSON file named using the
#'    assessment ID, entity ID, and optional model identifier.
#' 3. Prompts you to confirm before uploading the JSON file to the designated
#'    Google Drive folder for SIS administrators.
#'     
#' @export
#'
#' @examples
#' \dontrun{
#' stockplotr::plot_recruitment(
#'   example_data,
#'   make_rda = T
#'   )
#' 
#' stockplotr::table_total_catch(
#'   example_data,
#'   make_rda = T
#'   )
#' 
#' export_to_sis(
#'   sis_data_dir = getwd()
#' )
#' }
#'
export_to_sis <- function(
    sis_data_dir = getwd()
){
  # Read in, clean, check, format assessment data
  sis_assmt_data <- read.csv(file.path(sis_data_dir, "sis_assmt_template.csv"), stringsAsFactors = FALSE) |>
    dplyr::select(-Explanation, -Default)
  
  # check data for required fields and character limits
  if (!is.na(sis_assmt_data$Value[sis_assmt_data$Variable == "AS_B_COMMENT"]) && nchar(sis_assmt_data$Value[sis_assmt_data$Variable == "AS_B_COMMENT"]) > 1000){
    stop("AS_B_COMMENT exceeds 1,000 character limit")
  }

  if (!is.na(sis_assmt_data$Value[sis_assmt_data$Variable == "AS_F_COMMENT"]) && nchar(sis_assmt_data$Value[sis_assmt_data$Variable == "AS_F_COMMENT"]) > 1000){
    stop("AS_F_COMMENT exceeds 1,000 character limit")
  }

  required_missing <- sis_assmt_data |>
    dplyr::filter(Optional == "NO") |>
    dplyr::filter(is.na(Value) | Value == "")
  
  missing_vars <- length(required_missing$Variable)
  if (missing_vars > 0){
    cli::cli_bullets(c(
      "x" = "Missing {missing_vars} required field{?s}:",
      stats::setNames(as.character(required_missing$Variable), rep("*", missing_vars))
    ))
    stop("Please provide values for the missing required fields in sis_data.csv, then rerun the function.")
  }
  
  # add empty fields for SIS to fill in
  null_fields <- data.frame(
    Variable = c(
      "DATE_CREATED",
      "CREATED_BY",
      "DATE_MODIFIED",
      "MODIFIED_BY",
      "AS_LOCKED_FLAG",
      "AS_LEAD_LAB",
      "AS_TIMESERIES_LOCKED_FLAG",
      "AS_SURVEY_LINK_LOCKED_FLAG",
      "AS_FISCAL_YEAR",
      "AS_LOCKED_FLAG_BY",
      "AS_LOCKED_FLAG_DATE",
      "AS_TIMESERIES_LOCKED_FLAG_BY",
      "AS_TIMESERIES_LOCKED_FLAG_DATE",
      "AS_SURVEY_LINK_LOCKED_FLAG_BY",
      "AS_SURVEY_LINK_LOCKED_FLAG_DATE",
      "PLANNED_ASSESSMENT_ID",
      "ENT_ID",
      "ENT_NAME",
      "ATS_CNT",
      "ASL_CNT"
    ),
    Value = c(rep("", 20)))
  
  sis_assmt_data <- sis_assmt_data |>
    dplyr::select(-Optional) |>
    dplyr::mutate(Value = as.character(Value)) |>
    dplyr::full_join(null_fields, by = c("Variable", "Value")) |>
    dplyr::mutate(Value = ifelse(is.na(Value), "", Value))
  
  
  # read in, clean, check, format time series data
  sis_ts_data <- read.csv(file.path(sis_data_dir, "sis_ts_template.csv"), stringsAsFactors = FALSE)
  
  if ("Year" %in% colnames(sis_ts_data) == FALSE){
    cli::cli_abort("sis_ts_template.csv is missing the 'Year' column. This column is mandatory.")
  }
  
  primary_type <- sis_ts_data |>
    dplyr::filter(Primary == "Y") |>
    dplyr::distinct(Category, Description, Unit) |>
    dplyr::pull(Category)
  
  valid_primary_cats <- c("Fmort", "Recruitment", "Catch", "Spawners", "Biomass")
  sb_cats <- c("Spawners", "Biomass")
  
  if (length(primary_type) == 0) {
    cli::cli_abort(
      "Zero categories are marked as 'Y' in the 'Primary' column. At least one primary time series must be designated from: 'Fmort', 'Recruitment', 'Catch', and either 'Spawners' or 'Biomass'."
    )
  } else if (any(primary_type %notin% valid_primary_cats)) {
    invalid <- unique(primary_type[primary_type %notin% valid_primary_cats])
    cli::cli_abort(
      "Invalid Primary category detected ({.val {invalid}}). Only 'Fmort', 'Recruitment', 'Catch', and either 'Spawners' or 'Biomass' can be marked as 'Y' ('Index' must always be 'N')."
    )
  } else if (any(table(primary_type) > 1)) {
    dups <- names(which(table(primary_type) > 1))
    cli::cli_abort(
      "Multiple time series are marked as 'Y' within the same category ({.val {dups}}). Only one time series per category can be designated as Primary."
    )
  } else if (sum(primary_type %in% sb_cats) > 1) {
    cli::cli_abort(
      "Both 'Spawners' and 'Biomass' are marked as 'Y' in the 'Primary' column. Only one time series combined across 'Spawners' and 'Biomass' can be designated as Primary."
    )
  }

  # name json file
  assess_ID <- sis_assmt_data |>
    dplyr::filter(Variable == "ASSESSMENT_ID") |>
    dplyr::pull(Value)
  
  entity_ID <- sis_assmt_data |>
    dplyr::filter(Variable == "ENTITY_ID") |>
    dplyr::pull(Value)
  
  model_identifier <- sis_assmt_data |>
    dplyr::filter(Variable == "model_identifier") |>
    dplyr::pull(Value)
  
  if (length(model_identifier) == 0 || is.na(model_identifier) || model_identifier == "") {
    model_identifier <- NA
  }
  
  # final filename
  filename <- paste0(assess_ID, "_", entity_ID, ifelse(is.na(model_identifier), "", paste0("_", model_identifier))) |>
    # Replace non-alphanumeric/hyphen/underscore characters with "_"
    stringr::str_replace_all("[^a-zA-Z0-9_-]", "_") |>
    # Collapse consecutive underscores into one
    stringr::str_replace_all("_+", "_") |>
    # Trim underscores from the beginning and end
    stringr::str_remove("^_+|_+$") |>
    paste0(".json")
  
  
  # prep datasets for conversion into json
  sis_list <- setNames(as.list(sis_assmt_data$Value), sis_assmt_data$Variable)
  
  # create data matrix
  categories <- unique(unlist(sis_ts_data["Category"]))
  
  # get unnamed vector of categories' units
  units <- sis_ts_data |>
    dplyr::select(Category, Unit) |>
    dplyr::distinct() |>
    dplyr::arrange(match(Category, categories)) |>
    dplyr::pull(Unit)
  
  primary_cat <- sis_ts_data |>
    dplyr::filter(Primary == "Y") |>
    dplyr::pull(Category) |>
    unique()

  primary_flags <- ifelse(categories %in% primary_cat, "Y", "N")
  
  ts_matrix <- sis_ts_data |>
    dplyr::mutate(Value = ifelse(is.na(Value), "(blank)", Value)) |>
    dplyr::select(Year, Category, Value) |>
    tidyr::pivot_wider(names_from = Category,
                       values_from = Value) |>
    dplyr::arrange(Year) |>
    as.matrix() |>
    unname()
    
  # the lengths of categories, primary_flags, and units should all be the same
  if (length(categories) != length(primary_flags) || length(categories) != length(units) || anyNA(units) || any(trimws(units) == "")){
    cli::cli_abort("At least one category does not have units. Please check the sis_ts_template.csv file for inconsistencies.")
  }
  
  nested_list <- list(
    SUMMARY = sis_list,
    TIMESERIES = list(
      PARAMETERS = list(
        TSC_NAME = categories,
        TSP_PRIMARY_FLAG = primary_flags,
        TSP_DESC = sis_ts_data$Description[match(categories, sis_ts_data$Category)],
        TSP_UNIT = units
      ),
      DATA = ts_matrix
    )
  )
 
  
  # write json file
  jsonlite::write_json(
    x = nested_list, 
    path = fs::path(getwd(), filename),
    na = "string",
    pretty = TRUE,       # Formats the JSON with clean indentation
    auto_unbox = TRUE    # Ensures single values don't convert to JSON arrays ([13879])
  )
  
  # Let author decide if json file is accurate and ready to upload to Google Drive
  cli::cli_alert_success("json file exported to {fs::path(getwd(), filename)}.")
  ready_q <- readline("Do you want to transmit it to Google Drive? (Y/N)")
  
  if (!interactive()) {ready_q <- "y"}
  if (!(tolower(trimws(ready_q)) %in% c("y", "yes"))) {
    cli::cli_abort("Transmission to Google Drive aborted.")
  }

  #TODO: create pipeline to upload to Google Drive via API once created
  googledrive::drive_upload(
    media = fs::path(getwd(), filename),
    # TODO: replace drive folder link with NMFS agency wide folder for depositing json files for Jeff
    path = "https://drive.google.com/drive/folders/1eZ_2Y38ftFffr2-EhC30ia1N_4Igacvg?usp=drive_link/",
    name = filename,
    overwrite = TRUE # Do we want to overwrite?
  )
}
