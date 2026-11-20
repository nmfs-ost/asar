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
  
  # Identify each distinct time series and assign a unique ID
  series_distinct <- sis_ts_data |>
    dplyr::distinct(Category, Description, Unit, Primary) |>
    dplyr::mutate(series_id = paste0("series_", dplyr::row_number()))
  
  # Abort if two series share the same Category, Description, and Year
  if (any(duplicated(sis_ts_data[, c("Category", "Description", "Year")]))) {
    cli::cli_abort(
      "Duplicate time series detected with identical 'Category', 'Description', and 'Year'. Give each time series within a category a unique 'Description'."
    )
  }
  
  primary_series <- series_distinct |>
    dplyr::filter(Primary == "Y") |>
    unique()
  
  primary_type <- primary_series$Category
  
  valid_primary_cats <- c("Fmort", "Recruitment", "Catch", "Spawners", "Biomass")
  required_primary_cats <- c("Fmort", "Recruitment", "Catch")
  sb_cats <- c("Spawners", "Biomass")
  present_required <- intersect(unique(series_distinct$Category), required_primary_cats)
  
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
  } else if (!all(present_required %in% primary_type)) {
    missing_prim <- setdiff(present_required, primary_type)
    cli::cli_abort(
      "Missing Primary = 'Y' designation for required categor{?y/ies}: {.val {missing_prim}}."
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
  
  # validate units across all distinct series
  if (anyNA(sis_ts_data$Unit) || any(trimws(sis_ts_data$Unit) == "")) {
    cli::cli_abort("At least one time series does not have units. Please check the sis_ts_template.csv file for inconsistencies.")
  }
  
  # create data matrix keyed by series_id so multiple series per category stay separate
  ts_matrix <- sis_ts_data |>
    dplyr::left_join(series_distinct, by = c("Category", "Description", "Unit", "Primary")) |>
    dplyr::mutate(Value = ifelse(is.na(Value), "(blank)", as.character(Value))) |>
    dplyr::select(Year, series_id, Value) |>
    tidyr::pivot_wider(
      names_from = series_id,
      values_from = Value,
      values_fill = "(blank)"
    ) |>
    dplyr::select(Year, dplyr::all_of(series_distinct$series_id)) |>
    dplyr::arrange(Year) |>
    as.matrix() |>
    unname()
  
  nested_list <- list(
    SUMMARY = sis_list,
    TIMESERIES = list(
      PARAMETERS = list(
        TSC_NAME = series_distinct$Category,
        TSP_PRIMARY_FLAG = ifelse(series_distinct$Primary == "Y", "Y", "N"),
        TSP_DESC = series_distinct$Description,
        TSP_UNIT = series_distinct$Unit
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
