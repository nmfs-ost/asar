#' Export key quantities to SIS via Google Drive
#'
#' Submit completed stock assessment data to SIS via a Google Drive folder.
#' 
#' @param key_quantities_dir Location of the key_quantities.csv
#' file created when any `stockplotr` figure or table is exported.
#' 
#' Default: The working directory.
#' 
#' @param model_identifier Argument used to distinguish between base model
#' and a new, updated model sent in subsequent submission. Optional.
#' 
#' Default: "base"
#' 
#' Options: "base", "updated_model_1", "updated_model_2", etc.
#' 
#' @param AS_POINT_OF_CONTACT The lead/corresponding author for a stock 
#' assessment, formatted as an email address.
#' 
#' @param AS_CATCH_DATA Categorical classification describing
#' the availability of catch data for use in the stock assessment. This level 
#' should be based on the data that was actually used in the final version of 
#' the assessment model.
#' 
#' Options: 0, 1, 2, 3, 4, 5. Descriptions of each level are as follows:
#'   \itemize{
#'     \item (0) - No quantitative catch data available
#'     \item (1) - Some catch data, but major gaps for some fishery sectors or for historical periods such that their use in assessments is not supported
#'     \item (2) - Enough catch data establish magnitude of catch and trends in catch for a major fishery sector in order to apply a data-limited assessment method. This includes fisheries that are closed and it is known that negligible catch is occurring
#'     \item (3) - Catch data is generally available for all fishery sectors to support quantitative stock assessment, but some gaps exist such as low observer coverage, high levels of self-reported catch, or weak information on discard mortality
#'     \item (4) - No data gaps substantially impede assessment, but catch is not without uncertainty (e.g. recreational catches estimated from surveys)
#'     \item (5) - Very complete knowledge of total catch
#'   }
#'   
#' @param AS_ABUNDANCE_DATA Categorical classification describing
#'  the availability of abundance data for use in the stock assessment. This 
#'  level should be based on the data that was actually used in the final version
#'  of the assessment model.
#'  
#'  Options: 0, 1, 2, 3, 4, 5. Descriptions of each level are as follows:
#'   \itemize{
#'     \item (0) - No indicator of stock abundance or trend in stock abundance over time
#'     \item (1) - Fishery-dependent catch rates (CPUE) are available, but high uncertainty about their standardization over time; or expert opinion on degree of stock depletion over time
#'     \item (2) - Fishery-dependent catch rates (CPUE) are sufficiently standardized to enable their use in full assessments; data from fishery-independent sources are not available or sufficient to estimate abundance trends
#'     \item (3) - Limited fishery-independent survey(s) provide estimates of relative abundance; however, the temporal or spatial coverage of the stock is limited or the sampling variability is high
#'     \item (4) - Complete fishery-independent survey(s) provide estimates of relative abundance, and the survey(s) cover a large proportion of the spatial extent of the stock with several years of tracking at a level of precision that supports assessments
#'     \item (5) - Calibrated fishery-independent survey(s) or tag-recapture provide estimates of absolute abundance
#'   }
#'   
#' @param AS_BIOLOGICAL_DATA Categorical classification describing the availability of 
#'   biological/life history data for use in the stock assessment. This level should be based on the data 
#'   that was actually used in the final version of the assessment model.
#'   
#'   Options: 0, 1, 2, 3, 4, 5. Descriptions of each level are as follows:
#'   \itemize{
#'     \item (0) - No life history data
#'     \item (1) - Estimates of most life history factors not based on empirical data; instead derived using proxies, meta-analyses, borrowed from other species, or without scientific basis
#'     \item (2) - Estimates of some life history factors based on stock-specific empirical data, but at least one derived using life history proxies, meta-analyses, borrowed from other species, or without scientific basis. Generally supports data-poor assessments that use life history information
#'     \item (3) - Estimates of most life history factors based on stock-specific empirical data
#'     \item (4) - Data are sufficient to track changes over time in at least growth
#'     \item (5) - No major gaps in life history knowledge, including detailed stock structure, spatial and temporal patterns in natural mortality, growth, and reproductive biology
#'   }
#'   
#' @param AS_ECOSYSTEM_DATA Categorical classification describing the usage of ecosystem 
#'   linkage data in the stock assessment. This level should be based on the data that was actually used in the final version of the assessment model.
#'  
#'   Options: 0, 1, 2, 3, 4, 5. Descriptions of each level are as follows:
#'   \itemize{
#'     \item (0) - No linkage to ecosystem dynamic or consideration of ecosystem properties (environment, climate, habitat, predator-prey, etc.) in configuring the assessment (i.e. equilibrium conditions assumed for ecosystem)
#'     \item (1) - Ecosystem-based hypotheses inform the assessment model structure (e.g. defining the stock boundaries and/or spatial or temporal features) and/or are used for processing assessment inputs (e.g. abundance index), but no explicit linkage to any ecosystem drivers (environment, climate, habitat, predator-prey, etc.)
#'     \item (2) - The assessment includes some form of variability or effect to explicitly account for unidentified ecosystem dynamic(s) (e.g. time/space “regimes”, random variation, or other approaches to changing features without direct inclusion of ecosystem data)
#'     \item (3) - One or more assessment features is linked to a dynamic (i.e. data) from at least one of the following categories: environment, climate, habitat, predator-prey data (e.g. covariate)
#'     \item (4) - The assessment model is linked to at least one ecosystem dynamic, and one or more process studies directly support the manner in which environmental, climate, habitat, and/or predator-prey dynamics are incorporated (e.g. consumption rates measured and covariate informed by results)
#'     \item (5) - The assessment approach is configured to be coupled or linked with an ecosystem process (e.g. multispecies, coupled biophysical, climate-linked models)
#'   }
#'   
#' @param AS_COMP_DATA Categorical classification describing the availability of size/age 
#'   composition data for use in the stock assessment. This level should be based on the data that was 
#'   actually used in the final version of the assessment model.
#'   
#'   Options: 0, 1, 2, 3, 4, 5. Descriptions of each level are as follows:
#'   \itemize{
#'     \item (0) - No composition data collected
#'     \item (1) - Some size or age composition data has been collected, but major gaps in coverage, and not used in stock assessment
#'     \item (2) - Enough size or age composition data has been collected to enable data-limited assessment approaches
#'     \item (3) - Enough size or age composition data is collected over a sufficient time series to be informative in age/size structured assessment models
#'     \item (4) - Enough age composition data has been collected over a sufficient time series to enable assessment methods that need age composition data from the fishery
#'     \item (5) - Very complete age and size composition data, including, as needed on stock-specific basis, knowledge of ageing precision, spatial patterns or other issues
#'   }
#'   
#' @param AS_MODEL_CAT Category of model used to complete the stock assessment; categories 
#'   are defined in Implementing a Next Generation Stock Assessment Enterprise (see Table 5.1; NOAA, 2018). 
#'   Categories range from 1-6 and focus on the population dynamics structure, data requirements, and types 
#'   of management advice provided. If an ensemble approach was used, select the highest category describing 
#'   one or more models included in the ensemble.
#'   
#'   Options: 0, 1, 2, 3, 4, 5, 6. Descriptions of each level are as follows:
#'   \itemize{
#'     \item (1) - Data-limited
#'     \item (2) - Index-based
#'     \item (3) - Aggregate Biomass Dynamics
#'     \item (4) - Virtual Population Analysis
#'     \item (5) - Statistical Catch-at-Length
#'     \item (6) - Statistical Catch-at-Age
#'   }
#'   
#' @param AS_TYPE Type of stock assessment, with regards to approach, technique, 
#'   effort level, and complexity (NOAA, 2018). Assigned automatically by SIS.
#'   
#'   Options: "Research Stock Assessment", "Research/Operational Stock Assessment", "Operational Assessment", "Stock Monitoring Update". Descriptions of each type are as follows:
#'   \itemize{
#'     \item Research Stock Assessment: Development or revision of a stock assessment data type or method, typically subjected to the regional assessment review process.
#'     \item Research/Operational Stock Assessment: Applied to provide management advice while also producing a substantial revision to the assessment method.
#'     \item Operational Assessment: Analyses conducted to provide scientific advice to fishery managers with particular focus on determining stock status and recommending catch limits.
#'     \item Stock Monitoring Update: Methods used to provide stock-level advice to fishery managers between stock assessments (e.g. catch-only updates) containing no changes to methods or data series.
#'   }
#'   
#' @param AS_REVIEW_TYPE Final status of the assessment, chosen from a set of 
#' values found in the SIS manual.
#'  
#'  Options: 1, 2, 3, 4, 5, 6, 7, 8, 9. Descriptions of each level are as follows:
#'   \itemize{
#'     \item (1) Not Reviewed
#'     \item (2) Accept Previous Approach, Remand New Attempt
#'     \item (3) Full Acceptance
#'     \item (4) Partial Acceptance, Fishing Mortality Estimates
#'     \item (5) Partial Acceptance, Biomass Estimates
#'     \item (6) Partial Acceptance, Status Determinations Only
#'     \item (7) Reject, Data Insufficient for Assessment
#'     \item (8) Reject, Results Too Uncertain To Be Considered Accurate
#'     \item (9) Remand
#'     }
#'     
#' @param ASSESSMENT_ID Unique numeric identifier assigned to all 
#'  stock assessment records. Assigned automatically by SIS.
#'  
#' @param ENTITY_ID Entity unique identifier value. Assigned
#' automatically by SIS.
#' 
#' @param AS_YEAR Year the assessment was completed. Assigned
#'  automatically by SIS.
#'  
#' @param AS_MONTH Month the assessment was completed. Assigned
#' automatically by SIS.
#'   
#' @param AS_LAST_DATA_YEAR Year of the "latest" data used in the assessment. 
#'   
#'   Default: value extracted as the "landings.end.year" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_B_BASIS The basis of the biomass unit.
#'   
#'   Options:
#'   \itemize{
#'     \item Spawning Stock Biomass
#'     \item Total Stock Biomass
#'     \item Survey-Estimated Biomass
#'     \item Escapement
#'     \item Stock Reproductive Output
#'     \item Survey Index
#'     \item Total Stock Abundance
#'     }
#'     
#' @param AS_F_BASIS The basis of the Fishing Mortality unit. 
#'   
#'   Options: 1, 2, 3, 4, 5, 6, 7. Descriptions of each level are as follows:
#'   \itemize{
#'     \item (1) Max F at Age
#'     \item (2) F for Fully-Selected Fish
#'     \item (3) Catch / Biomass
#'     \item (4) Catch / Exploitable Biomass
#'     \item (5) Catch
#'     \item (6) Fishing Intensity
#'     \item (7) True F
#'   }
#'   
#' @param AS_FMSY Estimated and/or calculated value of Fishing Mortality at MSY. 
#'  
#'   Default: value extracted as the "F.MSY.terminal" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_F_BEST Best estimate of Fishing Mortality. Typically,
#' Best F = Terminal F for the stock assessment unless the estimation
#' has undergone some transformation (e.g., averaging or retrospective
#' adjustment).
#'  
#'   Default: value extracted as the "F.terminal.est" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_FLIMIT_BASIS Basis for the recommended fishing mortality limit,
#' calculated or directly estimated. Only utilized in Alaska as assessments
#' utilize catch projections in the current year, so overfishing stock
#' status is always reviewed against the last "complete" year of fishing
#' activity. Most stocks utilize Flimit = Fmsy. Example: "F from 2024 asmt
#' corresponding to 2023 OFL". Optional.
#' 
#' Default: NULL
#'   
#' @param AS_B_YEAR Year of the Biomass estimate for the stock. 
#'  
#'   Default: value extracted as the "B.terminal.year" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_B_MAX Maximum estimated value within the approved confidence interval 
#'   of the Biomass estimate. Equivalent to the value of Best B Confidence 
#'   Interval Upper estimate. 
#'   
#'   Default: value extracted as the "B.terminal.max" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_BMSY Estimated stock size that would, on average, produce the maximum 
#'   sustainable yield when fished at a level equal to FMSY.
#'  
#'   Default: value extracted as the "B.msy" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'
#' @param AS_B_BMSY_RATIO Ratio of B / Bmsy.
#' Automatically calculated by SIS. Optional.
#' 
#' Default: NULL
#'
#' @param AS_STOCK_LEVEL_BMSY Whether the stock is
#' above, near, or below Bmsy based upon the value
#' provided in the AS_B_BMSY_RATIO field. Optional.
#' 
#' Options: "Above", "Near" (between 80% and 99%), "Below" (<80%)
#' 
#' Default: NULL
#'   
#' @param AS_B_MIN Minimum estimated value within the approved confidence interval 
#'   of the Biomass estimate. Equivalent to the value of Best B Confidence 
#'   Interval Lower estimate. 
#'  
#'   Default: value extracted as the "B.terminal.max" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_B_BEST Best estimate of Biomass. Typically, Best B = Terminal 
#' B for the stock assessment unless the estimation has undergone some
#' transformation (e.g., averaging or retrospective adjustment).
#'  
#'  Default: value extracted as the "B.terminal.est" key quantity
#'  within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_BMSY_BASIS Basis for the estimated BMSY value. Example: "B35%"
#'   
#' @param AS_FMSY_BASIS Estimated fishing mortality rate that, on average, 
#'   would produce the maximum sustainable yield from a stock at BMSY. Example:
#'   "F35% as proxy"
#'   
#' @param AS_FLIMIT Recommended fishing mortality limit from the assessment, 
#'   above which the stock would be considered to be experiencing overfishing. 
#'  
#'   Default: value extracted as the "F.limit" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_F_YEAR Terminal year estimate of stock Fishing Mortality.
#' Always corresponds to the year of the Best estimate of Fishing Mortality
#' (AS_F_BEST).
#'  
#'   Default: value extracted as the "F.terminal.year" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_F_UNIT Unit of measure corresponding to the fishing mortality estimate. 
#'   Linked to F Basis selections.
#'   
#'   Options: 1, 2, 3, 4, 5, 6, 7. Descriptions of each level are as follows:
#'   \itemize{
#'     \item (1) Apical F = Max F at Age
#'     \item (2) Fully-selected F = F for Fully-Selected Fish
#'     \item (3) Exploitation Rate = Catch / Biomass
#'     \item (4) Relative F = Catch / Exploitable Biomass
#'     \item (5) Metric Tons = Catch
#'     \item (6) 1 - SPR = Fishing Intensity
#'     \item (7) F = Z - M = True F
#'   }
#'   
#' @param AS_B_UNIT Unit of measure corresponding to the biomass estimate.
#'  Linked to B Basis selections.
#'  
#'  Options: 1, 2, 3, 4, 5, 6, 7, 8. Descriptions of each level are as follows:
#'   \itemize{
#'     \item (1) Metric Tons = Spawning Stock Biomass / Total Stock Biomass / Survey-Estimated Biomass
#'     \item (2) Thousand Metric Tons = Spawning Stock Biomass / Total Stock Biomass / Survey-Estimated Biomass
#'     \item (3) Adult spawners - Natural & Hatchery - Escapement
#'     \item (4) Adult spawners - Hatchery - Escapement
#'     \item (5) Adult spawners - Natural - Escapement
#'     \item (6) Number of Eggs - Stock Reproductive Output
#'     \item (7) kg / tow - Survey Index
#'     \item (8) Number of Fish - Total Stock Abundance
#'   }
#'   
#' @param AS_MODEL Model software package used to complete the final version of the assessment. Example: "SS"
#'   
#' @param AS_MODEL_VERSION Version of the software package used to complete the final stock assessment. Example: "3.30.22"
#'  
#'   
#' @param AS_ENSEMBLE_FLAG Whether the assessment was completed using an ensemble
#'  or multimodeling approach. 
#'  
#'   Options: "Y" (yes), "N" (no)
#'   
#' @param AS_F_TRANSFORM Indicator identifying Fishing Mortality best estimates 
#' that include terminal year transformations (e.g., retrospective corrections 
#' or multi-year averaging).
#' 
#'  Options: "Y" (yes), "N" (no)
#'   
#' @param AS_B_RANGE_BASIS Approach used to calculate the confidence 
#'   intervals provided for the stock assessment. Optional.
#'  
#'  Default: NULL
#'  
#'  Options: "Asymptotic", "Credible", "Bootstrapped", user-specified
#'   
#' @param AS_B_RANGE Percentile range of the confidence intervals 
#'   provided for the stock assessment. Optional. 
#'  
#'   Default: 95
#'   
#' @param AS_B_TRANSFORM Indicator identifying Biomass best estimates that 
#' include terminal year transformations (e.g., retrospective corrections or 
#' multi-year averaging).
#'   
#'   Options: "Y" (yes), "N" (no)
#' 
#' 
#' 
#' 
#' 
#' @param AS_F_MAX Maximum estimated value within the approved confidence interval 
#'   of the Fishing Mortality estimate. This field should be equivalent to the 
#'   value of Best F Confidence Interval Upper estimate.
#'   
#'   Default: value extracted as the "F.terminal.max" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_F_MIN Minimum estimated value within the approved confidence interval 
#'   of the Fishing Mortality estimate. This field should be equivalent to the 
#'   value of Best F Confidence Interval Lower estimate.
#'   
#'   Default: value extracted as the "F.terminal.min" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'      
#' @param AS_F_RANGE_BASIS Approach used to calculate the confidence intervals 
#'   provided for the stock assessment. Optional.
#'   
#'  Default: NULL
#'  
#'  Options: "Asymptotic", "Credible", "Bootstrapped", user-specified
#'   
#' @param AS_F_RANGE Percentile range of the confidence intervals provided for 
#'   the stock assessment. Optional.
#'   
#'   Default: 95
#'   
#' @param AS_FMSY_MAX Maximum estimated value within the approved confidence interval 
#'   of the Fishing Mortality estimate. This field should be equivalent to the 
#'   value of Fmsy Confidence Interval Upper estimate.
#'   
#'   Default: value extracted as the "F.MSY.terminal.max" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_FMSY_MIN Minimum estimated value within the approved confidence interval 
#'   of the Fishing Mortality estimate. This field should be equivalent to the 
#'   value of Fmsy Confidence Interval Lower estimate.
#'   
#'   Default: value extracted as the "F.MSY.terminal.min" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'      
#' @param AS_FMSY_RANGE_BASIS Approach used to calculate the confidence intervals 
#'   provided for the stock assessment. Optional.
#'   
#'  Default: NULL
#'  
#'  Options: "Asymptotic", "Credible", "Bootstrapped", user-specified
#'  
#' @param AS_FMSY_RANGE Percentile range of the confidence intervals provided for 
#'   the stock assessment. Optional.
#'   
#'   Default: 95
#'   
#' @param AS_FTARGET Value of the Ftarget estimate produced by a stock assessment. 
#'   This is often used for stocks in a rebuilding plan.
#'   
#'   Default: value extracted as the "F.target" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_FTARGET_BASIS Approach used to calculate the Ftarget estimate produced 
#'   by a stock assessment.
#'   
#' @param AS_MSY Value of the MSY estimated by the assessment.
#'   
#' @param AS_MSY_UNIT Unit associated with the MSY value.
#'   
#'   Options:
#'   \itemize{
#'     \item Metric tons
#'     \item Thousand metric tons
#'     \item lbs
#'     \item Thousand lbs
#'     \item Number of fish
#'   }
#'   
#' @param AS_MSY_MAX Maximum estimated value within the approved confidence interval 
#'   of the Fishing Mortality estimate. This field should be equivalent to the 
#'   value of MSY Confidence Interval Upper estimate.
#'     
#' @param AS_MSY_MIN Minimum estimated value within the approved confidence interval 
#'   of the Fishing Mortality estimate. This field should be equivalent to the 
#'   value of MSY Confidence Interval Lower estimate.
#'      
#' @param AS_MSY_RANGE_BASIS Approach used to calculate the confidence intervals 
#'   provided for the stock assessment. Optional.
#'   
#'  Default: NULL
#'  
#'  Options: "Asymptotic", "Credible", "Bootstrapped", user-specified
#'   
#' @param AS_MSY_RANGE Percentile range of the confidence intervals provided for 
#'   the stock assessment. Optional.
#'   
#'   Default: 95
#'   
#' @param AS_BMSY_MAX Maximum estimated value within the approved confidence interval 
#'   of the Fishing Mortality estimate. This field should be equivalent to the 
#'   value of Bmsy Confidence Interval Upper estimate.
#'   
#'   Default: value extracted as the "B.msy.max" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_BMSY_MIN Minimum estimated value within the approved confidence interval 
#'   of the Fishing Mortality estimate. This field should be equivalent to the 
#'   value of Bmsy Confidence Interval Lower estimate.
#'   
#'   Default: value extracted as the "B.msy.min" key quantity
#'   within the key_quantities.csv file imported via `key_quantities_dir`.
#'   
#' @param AS_BMSY_RANGE_BASIS Approach used to calculate the confidence intervals 
#'   provided for the stock assessment. Optional.
#'   
#'  Default: NULL
#'  
#'  Options: "Asymptotic", "Credible", "Bootstrapped", user-specified
#'  
#' @param AS_BMSY_RANGE Percentile range of the confidence intervals provided for 
#'   the stock assessment. Optional.
#'   
#'   Default: 95
#' 
#' @param AS_BLIMIT Stock size threshold, below which the stock is considered to be overfished.
#' 
#' @param AS_BLIMIT_BASIS Basis for the Blimit estimate.
#' Examples: (0.7*Bmsy), B25%, etc.
#' 
#' @param AS_B_COMMENT Specific comments associated with the best estimate of biomass for this assessment. 1,000 character limit.
#' 
#' @param AS_F_COMMENT Specific comments associated with the best estimate of fishing mortality for this assessment. 1,000 character limit.
#' 
#' @param AS_IAS_FLIMIT International commission F limit estimate. Optional.
#' 
#' Default: NULL
#' 
#' @param AS_IAS_FLIMIT_BASIS International commission estimate of Flimit estimation method. Example: "msy".
#' Optional.
#' 
#' Default: NULL
#' 
#' @param AS_IAS_FMSY International commission estimate of Fmsy. Optional.
#' 
#' Default: NULL
#' 
#' @param AS_IAS_FMSY_BASIS International commission estimate of Fmsy estimation method. Optional.
#'
#' Default: NULL
#' 
#' @param AS_IAS_FTARGET International commission estimate of Ftarget. Optional.
#' 
#' Default: NULL
#' 
#' @param AS_IAS_FTARGET_BASIS International commission estimate of Ftarget estimation method. Optional.
#' 
#' Default: NULL
#' 
#' @param AS_IAS_BLIMIT International commission biomass limit estimate. Optional.
#' 
#' Default: NULL
#' 
#' @param AS_IAS_BLIMIT_BASIS International commission estimate of Blimit estimation method. Optional.
#' 
#' Default: NULL
#' 
#' @param AS_IAS_BMSY International commission estimate of Bmsy. Optional.
#' 
#' Default: NULL
#' 
#' @param AS_IAS_BMSY_BASIS International commission estimate of Bmsy estimation method. Optional.
#' 
#' Default: NULL
#'
#'  
#' @details This function acts within the following workflow:
#' 
#' 1. When a stock assessment is scheduled to conclude, SIS will generate an
#'  attachment or prompt containing metadata and identifiers.
#' 2. Upon notification, the user will input some of these SIS-provided data into 
#'    this function, as well as other information that should be sent to SIS.
#'    The function will format and upload this data to a specific Google Drive folder.
#' 3. The uploaded contents will be resubmitted to SIS to finalize the record.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' stockplotr::save_all_plots(dat = stockplotr::example_data)
#' export_to_sis(
#'   AS_MODEL = "SS3",
#'   AS_POINT_OF_CONTACT = "patrick.star@myemail.gov",
#'   AS_CATCH_DATA = 5,
#'   AS_ABUNDANCE_DATA = 5,
#'   AS_BIOLOGICAL_DATA = 4,
#'   AS_ECOSYSTEM_DATA = 3,
#'   AS_COMP_DATA = 4,
#'   AS_MODEL_CAT = 1,
#'   AS_REVIEW_TYPE = 3,
#'   ASSESSMENT_ID = 1000,
#'   AS_YEAR = 2026,
#'   AS_MONTH = 12,
#'   AS_B_BASIS = "Spawning Stock Biomass",
#'   AS_F_BASIS = 1,
#'   AS_FLIMIT_BASIS = "F from 2024 asmt corresponding to 2023 OFL",
#'   AS_STOCK_LEVEL_BMSY = "Near",
#'   AS_B_MIN = 8000,
#'   AS_B_MAX = 15000,
#'   AS_B_BEST = 12000,
#'   AS_F_MIN = 0.5,
#'   AS_F_MAX = 2.0,
#'   AS_F_BEST = 1.5,
#'   AS_FMSY_MAX = 2.0,
#'   AS_FMSY_MIN = 0.5,
#'   AS_BMSY_BASIS = "B35%",
#'   AS_FMSY_BASIS = "F35% as proxy",
#'   ENTITY_ID = 10026,
#'   AS_F_UNIT = 2,
#'   AS_B_UNIT = 2,
#'   AS_MODEL_VERSION = "1.0",
#'   AS_TYPE = "Research & Operational",
#'   AS_ENSEMBLE_FLAG = "N",
#'   AS_F_TRANSFORM = "Y",
#'   AS_B_TRANSFORM = "Y",
#'   AS_FTARGET_BASIS = "Example basis",
#'   AS_MSY = 100,
#'   AS_MSY_UNIT = "lbs",
#'   AS_MSY_MAX = 150,
#'   AS_MSY_MIN = 50,
#'   AS_BLIMIT = 200,
#'   AS_BLIMIT_BASIS = "B25%",
#'   AS_B_COMMENT = "B Comment",
#'   AS_F_COMMENT = "F Comment"
#' )
#' }
#' \dontrun{
#' export_to_sis(
#'   key_quantities_dir = getwd(),
#'   model_identifier = "base",
#'   AS_POINT_OF_CONTACT = "patrick.star@myemail.gov",
#'   AS_CATCH_DATA = 5,
#'   AS_ABUNDANCE_DATA = 5,
#'   AS_BIOLOGICAL_DATA = 4,
#'   AS_ECOSYSTEM_DATA = 2,
#'   AS_COMP_DATA = 4,
#'   AS_MODEL_CAT = 6,
#'   AS_TYPE = "Research & Operational",
#'   AS_REVIEW_TYPE = 3,
#'   ASSESSMENT_ID = 13879,
#'   ENTITY_ID = 10026,
#'   AS_YEAR = 2024,
#'   AS_MONTH = 12,
#'   AS_LAST_DATA_YEAR = 2024,
#'   AS_B_BASIS = "Spawning Stock Biomass",
#'   AS_F_BASIS = 2,
#'   AS_F_UNIT = 2,
#'   AS_B_UNIT = 1,
#'   AS_MODEL = "SS",
#'   AS_MODEL_VERSION = "3.30.22",
#'   AS_ENSEMBLE_FLAG = "N",
#'   AS_F_TRANSFORM = "N",
#'   AS_B_TRANSFORM = "N",
#'   AS_F_BEST = 0.074,
#'   AS_F_YEAR = 2023,
#'   AS_F_MIN = 0.09,
#'   AS_F_MAX = 0.09,
#'   AS_F_RANGE_BASIS = "Asymptotic",
#'   AS_F_RANGE = 95,
#'   AS_FMSY = 0.17,
#'   AS_FMSY_BASIS = "F35% as proxy",
#'   AS_FMSY_MIN = 0.10,
#'   AS_FMSY_MAX = 0.25,
#'   AS_FMSY_RANGE_BASIS = "Asymptotic",
#'   AS_FMSY_RANGE = 90,
#'   AS_FLIMIT = 0.208,
#'   AS_FLIMIT_BASIS = "F from 2024 asmt corresponding to 2023 OFL",
#'   AS_FTARGET = 0.136,
#'   AS_FTARGET_BASIS = "80% Fmsy",
#'   AS_B_BEST = 147511,
#'   AS_B_YEAR = 2024,
#'   AS_B_MIN = 134463,
#'   AS_B_MAX = 160559,
#'   AS_B_RANGE_BASIS = "Credible",
#'   AS_B_RANGE = 95,
#'   AS_BMSY = 103743,
#'   AS_STOCK_LEVEL_BMSY = "Above",
#'   AS_BMSY_BASIS = "B35%",
#'   AS_BMSY_MIN = 85000,
#'   AS_BMSY_MAX = 120000,
#'   AS_BMSY_RANGE_BASIS = "Bootstrapped",
#'   AS_BMSY_RANGE = 99,
#'   AS_BLIMIT = 51871.5,
#'   AS_BLIMIT_BASIS = "50% * Bmsy",
#'   AS_MSY = 100,
#'   AS_MSY_UNIT = "Metric tons",
#'   AS_MSY_MIN = 80,
#'   AS_MSY_MAX = 120,
#'   AS_MSY_RANGE_BASIS = "Bootstrapped",
#'   AS_MSY_RANGE = 99,
#'   AS_B_COMMENT = "Biomass estimate based on latest bottom trawl survey.",
#'   AS_F_COMMENT = "Fully selected F calculated across age classes 5-10.",
#'   AS_IAS_FLIMIT = 0.35,
#'   AS_IAS_FLIMIT_BASIS = "msy",
#'   AS_IAS_FMSY = 0.99,
#'   AS_IAS_FMSY_BASIS = "msy",
#'   AS_IAS_FTARGET = 0.99,
#'   AS_IAS_FTARGET_BASIS = "msy",
#'   AS_IAS_BLIMIT = 0.35,
#'   AS_IAS_BLIMIT_BASIS = "msy",
#'   AS_IAS_BMSY = 0.99,
#'   AS_IAS_BMSY_BASIS = "msy",
#'   TIME_SERIES = NULL
#' )
#' }
#'
export_to_sis <- function(
  key_quantities_dir = getwd(),
  # SIS summary
  model_identifier = "base",
  AS_POINT_OF_CONTACT,
  AS_CATCH_DATA,
  AS_ABUNDANCE_DATA,  
  AS_BIOLOGICAL_DATA,
  AS_ECOSYSTEM_DATA,
  AS_COMP_DATA,
  AS_MODEL_CAT,
  AS_TYPE,
  AS_REVIEW_TYPE,
  ASSESSMENT_ID,
  ENTITY_ID,
  AS_YEAR,
  AS_MONTH,
  AS_LAST_DATA_YEAR = NULL,
  AS_B_BASIS,
  AS_F_BASIS,
  AS_FMSY = NULL,
  AS_F_BEST,
  AS_FLIMIT_BASIS = NULL,
  AS_B_YEAR = NULL,
  AS_B_MAX,
  AS_BMSY = NULL,
  AS_B_BMSY_RATIO = NULL,
  AS_STOCK_LEVEL_BMSY = NULL,
  AS_B_MIN,
  AS_B_BEST,
  AS_BMSY_BASIS,
  AS_FMSY_BASIS,
  AS_FLIMIT = NULL,
  AS_F_YEAR = NULL,
  AS_F_UNIT,
  AS_B_UNIT,
  AS_MODEL,
  AS_MODEL_VERSION,
  AS_ENSEMBLE_FLAG,
  AS_F_TRANSFORM,
  AS_B_RANGE_BASIS = NULL,
  AS_B_RANGE = 95,
  AS_B_TRANSFORM,
  AS_F_MAX,
  AS_F_MIN,
  AS_F_RANGE_BASIS = NULL,
  AS_F_RANGE = 95,
  AS_FMSY_MAX,
  AS_FMSY_MIN,
  AS_FMSY_RANGE_BASIS = NULL,
  AS_FMSY_RANGE = 95,
  AS_FTARGET = NULL,
  AS_FTARGET_BASIS,
  AS_MSY,
  AS_MSY_UNIT,
  AS_MSY_MAX,
  AS_MSY_MIN,
  AS_MSY_RANGE_BASIS = NULL,
  AS_MSY_RANGE = 95,
  AS_BMSY_MAX = NULL,
  AS_BMSY_MIN = NULL,
  AS_BMSY_RANGE_BASIS = NULL,
  AS_BMSY_RANGE = 95,
  AS_BLIMIT,
  AS_BLIMIT_BASIS,
  AS_B_COMMENT = NULL,
  AS_F_COMMENT = NULL,
  AS_IAS_FLIMIT = NULL,
  AS_IAS_FLIMIT_BASIS = NULL,
  AS_IAS_FMSY = NULL,
  AS_IAS_FMSY_BASIS = NULL,
  AS_IAS_FTARGET = NULL,
  AS_IAS_FTARGET_BASIS = NULL,
  AS_IAS_BLIMIT = NULL,
  AS_IAS_BLIMIT_BASIS = NULL,
  AS_IAS_BMSY = NULL,
  AS_IAS_BMSY_BASIS = NULL,
  
  # SIS time series
  # this is formatted as a string to be imported as JSON into SIS, but could be formatted as a df and converted to JSON in the function
  # colnames: Year; Catch (Metric Tons); Spawners (Metric Tons); Recruitment (Recruits - Age 1);	Fmort (Fully-selected F)
  TIME_SERIES = NULL
){
  
  if (!is.null(AS_B_COMMENT)){
      if (nchar(AS_B_COMMENT) > 1000){
    stop("AS_B_COMMENT exceeds 1,000 character limit")
      }
    }

  if (!is.null(AS_F_COMMENT)){
    if (nchar(AS_F_COMMENT) > 1000){
      stop("AS_F_COMMENT exceeds 1,000 character limit")
    }
  }
  
  kqs <- read.csv(fs::path(key_quantities_dir,
                           "key_quantities.csv"), 
                  stringsAsFactors = FALSE)
  
  if (is.null(AS_LAST_DATA_YEAR)){
    AS_LAST_DATA_YEAR <- kqs |>
      dplyr::filter(key_quantity == "landings.end.year") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (is.null(AS_FMSY)){
    AS_FMSY <- kqs |>
      dplyr::filter(key_quantity == "F.MSY.terminal") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (is.null(AS_BMSY)){
    AS_BMSY <- kqs |>
      dplyr::filter(key_quantity == "B.msy") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (is.null(AS_BMSY_MIN)){
    AS_BMSY_MIN <- kqs |>
      dplyr::filter(key_quantity == "B.msy.min") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (is.null(AS_BMSY_MAX)){
    AS_BMSY_MAX <- kqs |>
      dplyr::filter(key_quantity == "B.msy.max") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (is.null(AS_B_YEAR)){
    AS_B_YEAR <- kqs |>
      dplyr::filter(key_quantity == "B.terminal.year") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (is.null(AS_F_YEAR)){
    AS_F_YEAR <- kqs |>
      dplyr::filter(key_quantity == "F.terminal.year") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (is.null(AS_FTARGET)){
    AS_FTARGET <- kqs |>
      dplyr::filter(key_quantity == "F.target") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (is.null(AS_FLIMIT)){
    AS_FLIMIT <- kqs |>
      dplyr::filter(key_quantity == "F.limit") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (!exists("AS_B_BEST") || is.null(AS_B_BEST)){
    AS_B_BEST <- kqs |>
      dplyr::filter(key_quantity == "B.terminal.est") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (!exists("AS_F_BEST") || is.null(AS_F_BEST)){
    AS_F_BEST <- kqs |>
      dplyr::filter(key_quantity == "F.terminal.est") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (!exists("AS_B_MIN") || is.null(AS_B_MIN)){
    AS_B_MIN <- kqs |>
      dplyr::filter(key_quantity == "B.terminal.min") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (!exists("AS_B_MAX") || is.null(AS_B_MAX)){
    AS_B_MAX <- kqs |>
      dplyr::filter(key_quantity == "B.terminal.max") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  
  if (!exists("AS_F_MIN") || is.null(AS_F_MIN)){
    AS_F_MIN <- kqs |>
      dplyr::filter(key_quantity == "F.terminal.min") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (!exists("AS_F_MAX") || is.null(AS_F_MAX)){
    AS_F_MAX <- kqs |>
      dplyr::filter(key_quantity == "F.terminal.max") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (!exists("AS_FMSY_MAX") || is.null(AS_FMSY_MAX)){
    AS_FMSY_MAX <- kqs |>
      dplyr::filter(key_quantity == "F.MSY.terminal.max") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  if (!exists("AS_FMSY_MIN") || is.null(AS_FMSY_MIN)){
    AS_FMSY_MIN <- kqs |>
      dplyr::filter(key_quantity == "F.MSY.terminal.min") |>
      dplyr::select(value) |>
      as.numeric()
  }
  
  

  # Explicitly Required Fields
  required_fields <- c(
    "AS_REVIEW_TYPE",
    "AS_LAST_DATA_YEAR",
    "AS_MODEL",
    "AS_POINT_OF_CONTACT",
    "AS_CATCH_DATA",
    "AS_ABUNDANCE_DATA",
    "AS_BIOLOGICAL_DATA",
    "AS_ECOSYSTEM_DATA",
    "AS_COMP_DATA",
    "AS_MODEL_CAT"
  )
  
  # Optional Fields
  optional_fields <- c(
    "AS_B_BMSY_RATIO",
    "AS_F_FLIMIT_RATIO",
    "AS_F_FMSY_RATIO",
    "AS_B_RANGE_BASIS",
    "AS_B_RANGE",
    "AS_F_RANGE_BASIS",
    "AS_F_RANGE",
    "AS_FMSY_RANGE_BASIS",
    "AS_FMSY_RANGE",
    "AS_MSY_RANGE_BASIS",
    "AS_MSY_RANGE",
    "AS_BMSY_RANGE_BASIS",
    "AS_BMSY_RANGE", 
    "AS_IAS_FLIMIT", 
    "AS_IAS_FLIMIT_BASIS",
    "AS_IAS_FMSY", 
    "AS_IAS_FMSY_BASIS",
    "AS_IAS_FTARGET",
    "AS_IAS_FTARGET_BASIS", 
    "AS_IAS_BLIMIT", 
    "AS_IAS_BLIMIT_BASIS", 
    "AS_IAS_BMSY",
    "AS_IAS_BMSY_BASIS"
  )
  
  # Unspecified / Default Required Fields (All remaining fields)
  default_required_fields <- c(
    "ASSESSMENT_ID", "AS_YEAR", "AS_MONTH", "AS_B_BASIS", "AS_F_BASIS", 
    "AS_FMSY", "AS_F_BEST", "AS_FLIMIT_BASIS", "AS_B_YEAR", "AS_B_MAX", 
    "AS_BMSY", "AS_STOCK_LEVEL_BMSY", "AS_B_MIN", "AS_B_BEST", "AS_BMSY_BASIS", 
    "AS_FMSY_BASIS", "AS_FLIMIT", "AS_F_YEAR", "ENTITY_ID", "AS_F_UNIT", 
    "AS_B_UNIT", "AS_MODEL_VERSION", "AS_TYPE", "AS_ENSEMBLE_FLAG", 
    "AS_F_TRANSFORM", "AS_B_TRANSFORM", 
    "AS_F_MAX", "AS_F_MIN", "AS_FMSY_MAX", "AS_FMSY_MIN", "AS_FTARGET", 
    "AS_FTARGET_BASIS", "AS_MSY", "AS_MSY_UNIT", "AS_MSY_MAX", "AS_MSY_MIN", 
    "AS_BMSY_MAX", "AS_BMSY_MIN", "AS_BLIMIT", "AS_BLIMIT_BASIS", "AS_B_COMMENT", 
    "AS_F_COMMENT"
    
    # Time Series & Surveys Metadata are optional
    
  )

  all_required_fields <- c(required_fields, default_required_fields)
  missing_fields <- all_required_fields[sapply(all_required_fields, function(x) {!exists(x) || is.null(get(x))})]
  if (length(missing_fields) > 0){
    cli::cli_bullets(c(
      "x" = "Missing {length(missing_fields)} required field{?s}:",
      stats::setNames(as.character(missing_fields), rep("*", length(missing_fields)))
    ))    
  }

  summary_list <- list(
    ASSESSMENT_ID = ASSESSMENT_ID,
    AS_YEAR = AS_YEAR,
    AS_MONTH = AS_MONTH,
    AS_REVIEW_TYPE = AS_REVIEW_TYPE,
    AS_LAST_DATA_YEAR = AS_LAST_DATA_YEAR,
    AS_B_BASIS = AS_B_BASIS,
    AS_F_BASIS = AS_F_BASIS,
    AS_FMSY = AS_FMSY,
    AS_F_BEST = AS_F_BEST,
    AS_FLIMIT_BASIS = AS_FLIMIT_BASIS,
    AS_B_YEAR = AS_B_YEAR,
    AS_B_MAX = AS_B_MAX,
    AS_BMSY = AS_BMSY,
    AS_B_BMSY_RATIO = AS_B_BMSY_RATIO,
    AS_STOCK_LEVEL_BMSY = AS_STOCK_LEVEL_BMSY,
    AS_B_MIN = AS_B_MIN,
    AS_B_BEST = AS_B_BEST,
    AS_BMSY_BASIS = AS_BMSY_BASIS,
    AS_FMSY_BASIS = AS_FMSY_BASIS,
    AS_FLIMIT = AS_FLIMIT,
    AS_F_YEAR = AS_F_YEAR,
    ENTITY_ID = ENTITY_ID,
    DATE_CREATED = NULL,
    CREATED_BY = NULL,
    DATE_MODIFIED = NULL,
    MODIFIED_BY = NULL,
    AS_LOCKED_FLAG = NULL,
    AS_F_UNIT = AS_F_UNIT,
    AS_B_UNIT = AS_B_UNIT,
    AS_MODEL = AS_MODEL,
    AS_MODEL_VERSION = AS_MODEL_VERSION,
    AS_LEAD_LAB = NULL,
    AS_POINT_OF_CONTACT = AS_POINT_OF_CONTACT,
    AS_TIMESERIES_LOCKED_FLAG = NULL,
    AS_SURVEY_LINK_LOCKED_FLAG = NULL,
    AS_CATCH_DATA = AS_CATCH_DATA,
    AS_ABUNDANCE_DATA = AS_ABUNDANCE_DATA,
    AS_BIOLOGICAL_DATA = AS_BIOLOGICAL_DATA,
    AS_ECOSYSTEM_DATA = AS_ECOSYSTEM_DATA,
    AS_COMP_DATA = AS_COMP_DATA,
    AS_MODEL_CAT = AS_MODEL_CAT,
    AS_TYPE = AS_TYPE,
    AS_ENSEMBLE_FLAG = AS_ENSEMBLE_FLAG,
    AS_FISCAL_YEAR = NULL,
    AS_F_TRANSFORM = AS_F_TRANSFORM,
    AS_B_RANGE_BASIS = AS_B_RANGE_BASIS,
    AS_B_RANGE = AS_B_RANGE,
    AS_B_TRANSFORM = AS_B_TRANSFORM,
    AS_LOCKED_FLAG_BY = NULL,
    AS_LOCKED_FLAG_DATE = NULL, 
    AS_TIMESERIES_LOCKED_FLAG_BY = NULL, 
    AS_TIMESERIES_LOCKED_FLAG_DATE = NULL, 
    AS_SURVEY_LINK_LOCKED_FLAG_BY = NULL, 
    AS_SURVEY_LINK_LOCKED_FLAG_DATE = NULL,
    PLANNED_ASSESSMENT_ID = NULL,
    ENT_ID = NULL, 
    ENT_NAME = NULL, 
    ATS_CNT = NULL, 
    ASL_CNT = NULL,
    AS_F_MAX = AS_F_MAX,
    AS_F_MIN = AS_F_MIN,
    AS_F_RANGE_BASIS = AS_F_RANGE_BASIS,
    AS_F_RANGE = AS_F_RANGE,
    AS_FMSY_MAX = AS_FMSY_MAX,
    AS_FMSY_MIN = AS_FMSY_MIN,
    AS_FMSY_RANGE_BASIS = AS_FMSY_RANGE_BASIS,
    AS_FMSY_RANGE = AS_FMSY_RANGE,
    AS_FTARGET = AS_FTARGET,
    AS_FTARGET_BASIS = AS_FTARGET_BASIS,
    AS_MSY = AS_MSY,
    AS_MSY_UNIT = AS_MSY_UNIT,
    AS_MSY_MAX = AS_MSY_MAX,
    AS_MSY_MIN = AS_MSY_MIN,
    AS_MSY_RANGE_BASIS = AS_MSY_RANGE_BASIS,
    AS_MSY_RANGE = AS_MSY_RANGE,
    AS_BMSY_MAX = AS_BMSY_MAX,
    AS_BMSY_MIN = AS_BMSY_MIN,
    AS_BMSY_RANGE_BASIS = AS_BMSY_RANGE_BASIS,
    AS_BMSY_RANGE = AS_BMSY_RANGE,
    AS_BLIMIT = AS_BLIMIT,
    AS_BLIMIT_BASIS = AS_BLIMIT_BASIS,
    AS_B_COMMENT = AS_B_COMMENT,
    AS_F_COMMENT = AS_F_COMMENT,
    AS_IAS_FLIMIT = AS_IAS_FLIMIT,
    AS_IAS_FLIMIT_BASIS = AS_IAS_FLIMIT_BASIS,
    AS_IAS_FMSY = AS_IAS_FMSY,
    AS_IAS_FMSY_BASIS = AS_IAS_FMSY_BASIS,
    AS_IAS_FTARGET = AS_IAS_FTARGET,
    AS_IAS_FTARGET_BASIS = AS_IAS_FTARGET_BASIS,
    AS_IAS_BLIMIT = AS_IAS_BLIMIT,
    AS_IAS_BLIMIT_BASIS = AS_IAS_BLIMIT_BASIS,
    AS_IAS_BMSY = AS_IAS_BMSY,
    AS_IAS_BMSY_BASIS = AS_IAS_BMSY_BASIS,
    model_identifier = model_identifier
  )
  
  summary_list <- replace(summary_list, sapply(summary_list, is.null), "")
  summary_list <- replace(summary_list, sapply(summary_list, is.na), "")
  
  for (i in seq_along(summary_list)) {
    if (is.double(summary_list[[i]])){
      summary_list <- lapply(summary_list, function(x) as.character(x))
    }
  }
  
  # left off here
  # TIME_SERIES <- data.frame(
  #   Year = c(2000, 2001, 2002),
  #   Catch_Metric_Tons = c(1000, 1100, 1200),
  #   Spawners_Metric_Tons = c(5000, 5500, 6000),
  #   Recruitment_Recruits_Age_1 = c(10000, 11000, 12000),
  #   Fmort_Fully_selected_F = c(0.1, 0.15, 0.2)
  # )
  
  # time_series <- TIME_SERIES |>
  #   tidyr::pivot_longer(cols = -Year, names_to = "Metric", values_to = "Value") |>
  #   tidyr::unite("Metric_Unit", Metric, sep = "_") |>
  #   tidyr::pivot_wider(names_from = Metric_Unit, values_from = Value)
  
  # final filename
  filename <- paste0(ASSESSMENT_ID, "_", ENTITY_ID, "_", model_identifier)  |>
    # Replace non-alphanumeric/hyphen/underscore characters with "_"
    stringr::str_replace_all("[^a-zA-Z0-9_-]", "_") |>
    # Collapse consecutive underscores into one
    stringr::str_replace_all("_+", "_") |>
    # Trim underscores from the beginning and end
    stringr::str_remove("^_+|_+$") |>
    paste0(".json")
  
  jsonlite::write_json(
    x = summary_list, 
    path = fs::path(getwd(), filename), 
    pretty = TRUE,       # Formats the JSON with clean indentation
    auto_unbox = TRUE    # Ensures single values don't convert to JSON arrays ([13879])
  )
  
  #TODO: create pipeline to upload to Google Drive via API once created
}

