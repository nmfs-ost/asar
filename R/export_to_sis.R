#' TODO: add title
#'
#' TODO: add further description
#'
#' @param ASSESSMENT_ID Unique numeric identifier assigned to all 
#'  stock assessment records. Assigned automatically by SIS.
#' @param AS_YEAR Year the assessment was completed. Assigned
#'  automatically by SIS.
#' @param AS_MONTH Month the assessment was completed. Assigned
#' automatically by SIS.
#' @param AS_REVIEW_TYPE Final status of the assessment. This is 
#'  constrained to a set of values found in the SIS manual:
#'   \itemize{
#'     \item Not Reviewed
#'     \item Accept Previous Approach, Remand New Attempt
#'     \item Full Acceptance
#'     \item Partial Acceptance, Fishing Mortality Estimates
#'     \item Partial Acceptance, Biomass Estimates
#'     \item Partial Acceptance, Status Determinations Only
#'     \item Reject, Data Insufficient for Assessment
#'     \item Reject, Results Too Uncertain To Be Considered Accurate
#'     \item Remand
#'     }
#'     
#' @param AS_LAST_DATA_YEAR Year of the "latest" data 
#'   used in the assessment. *Source: Automate extraction from model file.*
#' @param AS_B_BASIS The basis of the biomass unit. 
#'   *Source: Automate extraction from model file.* param should align
#'  with one of the following values:
#'   \itemize{
#'     \item Spawning Stock Biomass
#'     \item Total Stock Biomass
#'     \item Survey-Estimated Biomass
#'     \item Escapement
#'     \item Stock Reproductive Output
#'     \item Survey Index
#'     \item Total Stock Abundance
#'     }
#' @param AS_F_BASIS Character. The basis of the Fishing Mortality unit. 
#'   *Source: Automate extraction from model file.* Field should align with one 
#'   of the following values:
#'   \itemize{
#'     \item Max F @ Age
#'     \item F for Fully-Selected Fish
#'     \item Catch / Biomass
#'     \item Catch / Exploitable Biomass
#'     \item Catch
#'     \item Fishing Intensity
#'     \item True F
#'   }
#' @param AS_B_BMSY_RATIO Numeric. The ratio of B / Bmsy. **OPTIONAL FIELD.** #'   *Source: Automatically calculated inside database by default, or provided by model file.*
#' @param AS_FMSY Numeric. The estimated and/or calculated value of Fishing Mortality at MSY. 
#'   *Source: Automate extraction from model file.*
#' @param AS_F_FLIMIT_RATIO Numeric. The ratio of F / Flimit. **OPTIONAL FIELD.** #'   *Source: Automatically calculated inside database by default, or provided by model file.*
#' @param AS_F_BEST Numeric. The best estimate of Fishing Mortality. 
#'   *Source: Automate extraction from model file.*
#' @param AS_FLIMIT_BASIS Character. Basis for the recommended fishing mortality limit - 
#'   calculated or directly estimated. *Source: Automate extraction from model file, *
#'   *though user input may be required.*
#' @param AS_B_YEAR Numeric/Character. The year of the Biomass estimate for the stock. 
#'   *Source: Automate extraction from model file.*
#' @param AS_B_MAX Numeric. The maximum estimated value within the approved confidence interval 
#'   of the Biomass estimate. Equivalent to the value of Best B Confidence Interval Upper estimate. 
#'   *Source: Automate extraction from model file.*
#' @param AS_BMSY Numeric. Estimated stock size that would, on average, produce the maximum 
#'   sustainable yield when fished at a level equal to FMSY. *Source: Automate extraction from model file.*
#' @param AS_STOCK_LEVEL_BMSY Character. Displays whether the stock is ABOVE, NEAR 
#'   (between 80% and 99%), or BELOW (<80%) Bmsy based upon the value provided in the 
#'   AS_B_BMSY_RATIO field. *Source: Calculated within SIS Automatically.*
#' @param AS_B_MIN Numeric. The minimum estimated value within the approved confidence interval 
#'   of the Biomass estimate. Equivalent to the value of Best B Confidence Interval Lower estimate. 
#'   *Source: Automate extraction from model file.*
#' @param AS_B_BEST Numeric. The best estimate of Biomass. 
#'   *Source: Automate extraction from model file.*
#' @param AS_BMSY_BASIS Character. Basis for the estimated BMSY value. 
#'   *Source: Automate extraction from model file, though user input may be required.*
#' @param AS_FMSY_BASIS Character. The estimated fishing mortality rate that, on average, 
#'   would produce the maximum sustainable yield from a stock at BMSY. 
#'   *Source: Automate extraction from model file, though user input may be required.*
#' @param AS_F_FMSY_RATIO Numeric. The ratio of F / Fmsy. **OPTIONAL FIELD.** #'   *Source: Automatically calculated inside database by default, or provided by model file.*
#' @param AS_FLIMIT Numeric. The recommended fishing mortality limit from the assessment, 
#'   above which the stock would be considered to be experiencing overfishing. 
#'   *Source: Automate extraction from model file.*
#' @param AS_F_YEAR Numeric/Character. The year of the Fishing Mortality estimate for the stock. 
#'   *Source: Automate extraction from model file.*
#' @param ENTITY_ID Character/Numeric. Entity unique identifier value assigned by SIS. 
#'   *Source: Automatically provided by SIS.*
#' @return TODO add
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_to_sis(
#'  TODO: add example
#' )
#' }
#'
# TODO: import key quantities in place of args
export_to_sis <- function(
    # SIS summary
  ASSESSMENT_ID,
  ENT_NAME = "Alaska plaice - Bering Sea / Aleutian Islands",
  model_identifier = "base", # optional; can distinguish between base and new, updated model sent in subsequent submission
  AS_YEAR,
  AS_MONTH,
  AS_REVIEW_TYPE = "Full acceptance",
  AS_LAST_DATA_YEAR = 2024,
  AS_B_BASIS = "Spawning Stock Biomass",
  AS_F_BASIS = "F for Fully-Selected Fish",
  AS_B_BMSY_RATIO = 1.422,
  AS_FMSY = 0.17,
  AS_F_FLIMIT_RATIO = 0.356,
  AS_F_BEST = 0.074,
  AS_FLIMIT_BASIS = "F from 2024 asmt corresponding to 2023 OFL",
  AS_B_YEAR = 2024,
  AS_B_MAX = 160559,
  AS_BMSY = 103743,
  AS_STOCK_LEVEL_BMSY = "ABOVE",
  AS_B_MIN = 134463,
  AS_B_BEST = 147511,
  AS_BMSY_BASIS = "B35%",
  AS_FMSY_BASIS = "F35% as proxy",
  AS_F_FMSY_RATIO = 0.435,
  AS_FLIMIT = 0.208,
  AS_F_YEAR = 2023,
  ENTITY_ID = 10026,
  DATE_CREATED = "2025-07-25 08:35:39.0",
  CREATED_BY = "SYSTEM",
  DATE_MODIFIED = "2025-07-29 09:16:49.0",
  MODIFIED_BY = "SIS_USER",
  AS_LOCKED_FLAG = "Y",
  AS_F_UNIT = "Fully-selected F",
  AS_B_UNIT = "Metric Tons",
  AS_MODEL = "SS",
  AS_MODEL_VERSION = "3.30.22",
  AS_LEAD_LAB = "AFSC",
  AS_POINT_OF_CONTACT, # email address
  AS_TIMESERIES_LOCKED_FLAG = "Y",
  AS_SURVEY_LINK_LOCKED_FLAG = "Y",
  AS_CATCH_DATA = 5,
  AS_ABUNDANCE_DATA = 5,
  AS_BIOLOGICAL_DATA = 4,
  AS_ECOSYSTEM_DATA = 2,
  AS_COMP_DATA = 4,
  AS_MODEL_CAT = 6,
  AS_TYPE = "Research & Operational",
  AS_ENSEMBLE_FLAG = "N",
  AS_FISCAL_YEAR = 2025,
  AS_F_TRANSFORM = "N",
  AS_B_RANGE_BASIS = "Credible",
  AS_B_RANGE = 95,
  AS_B_TRANSFORM = "N",
  AS_LOCKED_FLAG_BY, # email address
  AS_LOCKED_FLAG_DATE = "2025-07-29 09:16:49.0",
  AS_TIMESERIES_LOCKED_FLAG_BY, # email address
  AS_TIMESERIES_LOCKED_FLAG_DATE = "2025-07-29 09:14:38.0",
  AS_SURVEY_LINK_LOCKED_FLAG_BY, # email address
  AS_SURVEY_LINK_LOCKED_FLAG_DATE = "2025-07-29 09:14:42.0",
  PLANNED_ASSESSMENT_ID = 471,
  ENT_ID = 10026,
  ATS_CNT = 4,
  ASL_CNT = 1,
  
  # SIS time series
  # this is formatted as a string to be imported as JSON into SIS, but could be formatted as a df and converted to JSON in the function
  # colnames: Year; Catch (Metric Tons); Spawners (Metric Tons); Recruitment (Recruits - Age 1);	Fmort (Fully-selected F)
  TIME_SERIES,
  
  # SIS surveys
  ASSESSMENT_SURVEY_LINK_ID = 13859,
  SURVEY_ID = 13,
  ASL_INFLUENCE_DEGREE = "Primary",
  S_NAME = "GAP-SAP Eastern Bering Sea Bottom Trawl_Summer"
){
  
  summary <- paste0(
    '{"ASSESSMENT_ID":"', ASSESSMENT_ID,
    '","AS_YEAR":"', AS_YEAR,
    '","AS_MONTH":"', AS_MONTH,
    '","AS_REVIEW_TYPE":"', AS_REVIEW_TYPE,
    '","AS_LAST_DATA_YEAR":"', AS_LAST_DATA_YEAR,
    '","AS_B_BASIS":"', AS_B_BASIS,
    '","AS_F_BASIS":"', AS_F_BASIS,
    '","AS_B_BMSY_RATIO":"', AS_B_BMSY_RATIO,
    '","AS_FMSY":"', AS_FMSY,
    '","AS_F_FLIMIT_RATIO":"', AS_F_FLIMIT_RATIO,
    '","AS_F_BEST":"', AS_F_BEST,
    '","AS_FLIMIT_BASIS":"', AS_FLIMIT_BASIS,
    '","AS_B_YEAR":"', AS_B_YEAR,
    '","AS_B_MAX":"', AS_B_MAX,
    '","AS_BMSY":"', AS_BMSY,
    '","AS_STOCK_LEVEL_BMSY":"', AS_STOCK_LEVEL_BMSY,
    '","AS_B_MIN":"', AS_B_MIN,
    '","AS_B_BEST":"', AS_B_BEST,
    '","AS_BMSY_BASIS":"', AS_BMSY_BASIS,
    '","AS_FMSY_BASIS":"', AS_FMSY_BASIS,
    '","AS_F_FMSY_RATIO":"', AS_F_FMSY_RATIO,
    '","AS_FLIMIT":"', AS_FLIMIT,
    '","AS_F_YEAR":"', AS_F_YEAR,
    '","ENTITY_ID":"', ENTITY_ID,
    '","DATE_CREATED":"', DATE_CREATED,
    '","CREATED_BY":"', CREATED_BY,
    '","DATE_MODIFIED":"', DATE_MODIFIED,
    '","MODIFIED_BY":"', MODIFIED_BY,
    '","AS_LOCKED_FLAG":"', AS_LOCKED_FLAG,
    '","AS_F_UNIT":"', AS_F_UNIT,
    '","AS_B_UNIT":"', AS_B_UNIT,
    '","AS_MODEL":"', AS_MODEL,
    '","AS_MODEL_VERSION":"', AS_MODEL_VERSION,
    '","AS_LEAD_LAB":"', AS_LEAD_LAB,
    '","AS_POINT_OF_CONTACT":"', AS_POINT_OF_CONTACT,
    '","AS_TIMESERIES_LOCKED_FLAG":"', AS_TIMESERIES_LOCKED_FLAG,
    '","AS_SURVEY_LINK_LOCKED_FLAG":"', AS_SURVEY_LINK_LOCKED_FLAG,
    '","AS_CATCH_DATA":"', AS_CATCH_DATA,
    '","AS_ABUNDANCE_DATA":"', AS_ABUNDANCE_DATA,
    '","AS_BIOLOGICAL_DATA":"', AS_BIOLOGICAL_DATA,
    '","AS_ECOSYSTEM_DATA":"', AS_ECOSYSTEM_DATA,
    '","AS_COMP_DATA":"', AS_COMP_DATA,
    '","AS_MODEL_CAT":"', AS_MODEL_CAT,
    '","AS_TYPE":"', AS_TYPE,
    '","AS_ENSEMBLE_FLAG":"', AS_ENSEMBLE_FLAG,
    '","AS_FISCAL_YEAR":"', AS_FISCAL_YEAR,
    '"," AS_F_TRANSFORM":"', AS_F_TRANSFORM,
    '"," AS_B_RANGE_BASIS":"', AS_B_RANGE_BASIS,
    '"," AS_B_RANGE":"', AS_B_RANGE,
    '"," AS_B_TRANSFORM":"', AS_B_TRANSFORM,
    '"," AS_LOCKED_FLAG_BY":"', AS_LOCKED_FLAG_BY,
    '"," AS_LOCKED_FLAG_DATE":"', AS_LOCKED_FLAG_DATE,
    '"," AS_TIMESERIES_LOCKED_FLAG_BY ":"', AS_TIMESERIES_LOCKED_FLAG_BY,
    '"," AS_TIMESERIES_LOCKED_FLAG_DATE ":"', AS_TIMESERIES_LOCKED_FLAG_DATE,
    '"," AS_SURVEY_LINK_LOCKED_FLAG_BY ":"', AS_SURVEY_LINK_LOCKED_FLAG_BY,
    '"," AS_SURVEY_LINK_LOCKED_FLAG_DATE ":"', AS_SURVEY_LINK_LOCKED_FLAG_DATE,
    '"," PLANNED_ASSESSMENT_ID ":"', PLANNED_ASSESSMENT_ID,
    '"," ENT_ID ":"', ENT_ID,
    '"," ENT_NAME ":"', ENT_NAME,
    '"," ATS_CNT ":"', ATS_CNT,
    '"," ASL_CNT ":"', ASL_CNT,
    '"," model_identifier ":"', model_identifier, '"}'
  )
  
  # left off here
  TIME_SERIES <- data.frame(
    Year = c(2000, 2001, 2002),
    Catch_Metric_Tons = c(1000, 1100, 1200),
    Spawners_Metric_Tons = c(5000, 5500, 6000),
    Recruitment_Recruits_Age_1 = c(10000, 11000, 12000),
    Fmort_Fully_selected_F = c(0.1, 0.15, 0.2)
  )
  
  time_series <- TIME_SERIES |>
    tidyr::pivot_longer(cols = -Year, names_to = "Metric", values_to = "Value") |>
    tidyr::unite("Metric_Unit", Metric, sep = "_") |>
    tidyr::pivot_wider(names_from = Metric_Unit, values_from = Value)
  
  surveys <- paste0(
    '{"ASSESSMENT_SURVEY_LINK_ID":"', ASSESSMENT_SURVEY_LINK_ID,
    '","ASSESSMENT_ID":"', ASSESSMENT_ID,
    '","SURVEY_ID":"', SURVEY_ID,
    '","ASL_INFLUENCE_DEGREE":"', ASL_INFLUENCE_DEGREE,
    '","S_NAME":"', S_NAME, '"}'
  )
  
  # final filename
  filename <- paste0(ASSESSMENT_ID, "_", ENT_NAME, ".json")  |>
    # Replace non-alphanumeric/hyphen/underscore characters with "_"
    stringr::str_replace_all("[^a-zA-Z0-9_-]", "_") |>
    # Collapse consecutive underscores into one
    stringr::str_replace_all("_+", "_") |>
    # Trim underscores from the beginning and end
    stringr::str_remove("^_+|_+$")
  
}
