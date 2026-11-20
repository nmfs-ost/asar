setup_valid_sis_files <- function(dir,
                                  include_model_id = FALSE) {
  assmt_df <- data.frame(
    Variable = c(
      "ASSESSMENT_ID",
      "ENTITY_ID",
      "AS_B_COMMENT",
      "AS_F_COMMENT",
      "OPTIONAL_FIELD"
    ),
    Explanation = rep("desc", 5),
    Optional = c("NO", "NO", "YES", "YES", "YES"),
    Default = rep("", 5),
    Value = c("12345", "67890", "Valid biomass note", "Valid F note", ""),
    stringsAsFactors = FALSE
  )
  
  if (include_model_id) {
    assmt_df <- rbind(
      assmt_df,
      data.frame(
        Variable = "model_identifier",
        Explanation = "desc",
        Optional = "YES",
        Default = "",
        Value = "Base Model v1.0!!",
        stringsAsFactors = FALSE
      )
    )
  }
  
  ts_df <- data.frame(
    Year = c(2020, 2020, 2021, 2021),
    Category = c("Biomass", "Catch", "Biomass", "Catch"),
    Primary = c("Y", "Y", "Y", "Y"),
    Description = c("Total Biomass", "Total Catch", "Total Biomass", "Total Catch"),
    Unit = c("mt", "mt", "mt", "mt"),
    Value = c(1000, 150, 1100, NA),
    stringsAsFactors = FALSE
  )
  
  write.csv(assmt_df, file.path(dir, "sis_assmt_template.csv"), row.names = FALSE)
  write.csv(ts_df, file.path(dir, "sis_ts_template.csv"), row.names = FALSE)
}

test_that("export_to_sis creates valid JSON and calls drive_upload on valid input", {
  # TODO Update this test once we swap out google drive for API
  tmp_dir <- withr::local_tempdir()
  withr::local_dir(tmp_dir)
  setup_valid_sis_files(tmp_dir, include_model_id = FALSE)
  
  uploaded_args <- NULL
  testthat::local_mocked_bindings(
    drive_upload = function(...) {
      uploaded_args <<- list(...)
      invisible(TRUE)
    },
    .package = "googledrive"
  )
  
  expect_no_error(export_to_sis(sis_data_dir = tmp_dir))
  
  expected_json <- file.path(tmp_dir, "12345_67890.json")
  expect_true(file.exists(expected_json))

  # Verify JSON structure and contents
  parsed <- jsonlite::fromJSON(expected_json, simplifyVector = FALSE)
  expect_named(parsed, c("SUMMARY", "TIMESERIES"))
  expect_equal(parsed$SUMMARY$ASSESSMENT_ID, "12345")
  expect_equal(parsed$SUMMARY$ENTITY_ID, "67890")
  # Verify null_fields were appended
  expect_true("DATE_CREATED" %in% names(parsed$SUMMARY))
  expect_equal(parsed$SUMMARY$DATE_CREATED, "")
  
  expect_equal(unlist(parsed$TIMESERIES$PARAMETERS$TSC_NAME), c("Biomass", "Catch"))
  expect_equal(unlist(parsed$TIMESERIES$PARAMETERS$TSP_PRIMARY_FLAG), c("Y", "Y"))
  # Verify NA values in time series convert to "(blank)"
  expect_equal(parsed$TIMESERIES$DATA[[2]][[3]], "(blank)")
})

test_that("export_to_sis cleans model_identifier in output filename", {
  tmp_dir <- withr::local_tempdir()
  withr::local_dir(tmp_dir)
  setup_valid_sis_files(tmp_dir, include_model_id = TRUE)
  
  testthat::local_mocked_bindings(
    drive_upload = function(...) invisible(TRUE),
    .package = "googledrive"
  )
  
  export_to_sis(sis_data_dir = tmp_dir)
  expect_true(file.exists(file.path(tmp_dir, "12345_67890_Base_Model_v1_0.json")))
})

test_that("export_to_sis errors when AS_B_COMMENT or AS_F_COMMENT exceeds 1000 chars", {
  tmp_dir <- withr::local_tempdir()
  setup_valid_sis_files(tmp_dir)
  
  assmt_path <- file.path(tmp_dir, "sis_assmt_template.csv")
  assmt_df <- read.csv(assmt_path, stringsAsFactors = FALSE)
  assmt_df$Value[assmt_df$Variable == "AS_B_COMMENT"] <- paste(rep("a", 1001), collapse = "")
  write.csv(assmt_df, assmt_path, row.names = FALSE)
  
  expect_error(
    export_to_sis(sis_data_dir = tmp_dir),
    "AS_B_COMMENT exceeds 1,000 character limit"
  )
  
  # Reset AS_B_COMMENT and test AS_F_COMMENT
  assmt_df$Value[assmt_df$Variable == "AS_B_COMMENT"] <- "Short comment"
  assmt_df$Value[assmt_df$Variable == "AS_F_COMMENT"] <- paste(rep("b", 1001), collapse = "")
  write.csv(assmt_df, assmt_path, row.names = FALSE)
  
  expect_error(
    export_to_sis(sis_data_dir = tmp_dir),
    "AS_F_COMMENT exceeds 1,000 character limit"
  )
})

test_that("export_to_sis errors when required fields (Optional == 'NO') are blank", {
  tmp_dir <- withr::local_tempdir()
  setup_valid_sis_files(tmp_dir)
  
  assmt_path <- file.path(tmp_dir, "sis_assmt_template.csv")
  assmt_df <- read.csv(assmt_path, stringsAsFactors = FALSE)
  assmt_df$Value[assmt_df$Variable == "ENTITY_ID"] <- ""
  write.csv(assmt_df, assmt_path, row.names = FALSE)
  
  expect_error(
    export_to_sis(sis_data_dir = tmp_dir),
    "Please provide values for the missing required fields"
  )
})

test_that("export_to_sis validates sis_ts_template.csv Year column and Primary rules", {
  tmp_dir <- withr::local_tempdir()
  setup_valid_sis_files(tmp_dir)
  ts_path <- file.path(tmp_dir, "sis_ts_template.csv")
  
  # 1. Missing Year column
  ts_no_year <- read.csv(ts_path, stringsAsFactors = FALSE) |> dplyr::select(-Year)
  write.csv(ts_no_year, ts_path, row.names = FALSE)
  expect_error(
    export_to_sis(sis_data_dir = tmp_dir),
    "missing the 'Year' column"
  )
  
  # 2. Zero categories marked Primary = 'Y'
  setup_valid_sis_files(tmp_dir)
  ts_zero_primary <- read.csv(ts_path, stringsAsFactors = FALSE)
  ts_zero_primary$Primary <- "N"
  write.csv(ts_zero_primary, ts_path, row.names = FALSE)
  expect_error(
    export_to_sis(sis_data_dir = tmp_dir),
    "Zero categories are marked as 'Y'"
  )
  
  # 3. Invalid category ('Index') marked Primary = 'Y'
  ts_index_primary <- data.frame(
    Year = 2020,
    Category = "Index",
    Primary = "Y",
    Description = "Survey Index",
    Unit = "Index",
    Value = 50
  )
  write.csv(ts_index_primary, ts_path, row.names = FALSE)
  expect_error(
    export_to_sis(sis_data_dir = tmp_dir),
    "Invalid Primary category detected"
  )
  
  # 4. Both 'Spawners' and 'Biomass' marked Primary = 'Y'
  ts_both_sb <- data.frame(
    Year = c(2020, 2020),
    Category = c("Biomass", "Spawners"),
    Primary = c("Y", "Y"),
    Description = c("Total Biomass", "Spawning Biomass"),
    Unit = c("mt", "mt"),
    Value = c(1000, 500)
  )
  write.csv(ts_both_sb, ts_path, row.names = FALSE)
  expect_error(
    export_to_sis(sis_data_dir = tmp_dir),
    "Both 'Spawners' and 'Biomass' are marked as 'Y'"
  )
})

test_that("export_to_sis errors when a category has inconsistent units", {
  tmp_dir <- withr::local_tempdir()
  setup_valid_sis_files(tmp_dir)
  
  ts_path <- file.path(tmp_dir, "sis_ts_template.csv")
  ts_df <- read.csv(ts_path, stringsAsFactors = FALSE)
  # Give Biomass two different units across years so length(units) != length(categories)
  ts_df$Unit[ts_df$Category == "Biomass" & ts_df$Year == 2021] <- "kg"
  ts_df <- ts_df |>
    # convert Catch into Index
    dplyr::mutate(Category = ifelse(Category == "Catch", "Index", Category)) |>
    dplyr::mutate(Primary = ifelse(Category == "Biomass", "Y", "N")) |>
    dplyr::mutate(Unit = ifelse(Category == "Biomass", "kg", NA))
  
    write.csv(ts_df, ts_path, row.names = FALSE)

  expect_error(
    export_to_sis(sis_data_dir = tmp_dir),
    "At least one time series does not have units"
  )
})

test_that("export_to_sis enforces category rules", {
  tmp_dir <- withr::local_tempdir()
  setup_valid_sis_files(tmp_dir)
  
  # test valid df with multiple Fmort, Recruitment, and Catch time series, each with exactly one Primary = 'Y'
  ts_df <- data.frame(
    Year = rep(2020:2021, 6),
    Category = rep(c("Fmort", "Fmort", "Recruitment", "Recruitment", "Catch", "Catch"), each = 2),
    Description = rep(c("F_primary", "F_secondary", "R_primary", "R_secondary", "C_primary", "C_secondary"), each = 2),
    Unit = rep(c("1/yr", "1/yr", "mt", "mt", "mt", "mt"), each = 2),
    Primary = rep(c("Y", "N", "Y", "N", "Y", "N"), each = 2),
    Value = runif(12, 10, 100),
    stringsAsFactors = FALSE
  )
  write.csv(ts_df, file.path(tmp_dir, "sis_ts_template.csv"), row.names = FALSE)
  
  ts_path <- file.path(tmp_dir, "sis_ts_template.csv")
  ts_df <- read.csv(ts_path, stringsAsFactors = FALSE)
  
  uploaded_args <- NULL
  testthat::local_mocked_bindings(
    drive_upload = function(...) {
      uploaded_args <<- list(...)
      invisible(TRUE)
    },
    .package = "googledrive"
  )
  
  expect_no_error(export_to_sis(sis_data_dir = tmp_dir))
  
  # test invalid df with multiple Fmort, Recruitment, and Catch time series, one with >1 Primary = 'Y'
  ts_df <- ts_df |>
    dplyr::mutate(Primary = ifelse(Category == "Fmort" & Description == "F_secondary", "Y", Primary))
  write.csv(ts_df, file.path(tmp_dir, "sis_ts_template.csv"), row.names = FALSE)
  
  expect_error(
    export_to_sis(sis_data_dir = tmp_dir),
    "Multiple time series are marked as 'Y'"
  )
  
  # test invalid case when Spawners and Biomass both marked Primary = 'Y'
  ts_df <- data.frame(
    Year = rep(2020:2021, 4),
    Category = rep(c("Spawners", "Spawners", "Biomass", "Biomass"), each = 2),
    Description = rep(c("S_primary", "S_secondary", "B_primary", "B_secondary"), each = 2),
    Unit = rep(c("mt", "mt", "mt", "mt"), each = 2),
    Primary = rep(c("Y", "N", "Y", "N"), each = 2),
    Value = runif(8, 10, 100),
    stringsAsFactors = FALSE
  )
  write.csv(ts_df, file.path(tmp_dir, "sis_ts_template.csv"), row.names = FALSE)
  
  expect_error(
    export_to_sis(sis_data_dir = tmp_dir),
    regexp = "Both 'Spawners' and 'Biomass' are"
  )
  
  # test invalid case when an Index series is marked Primary = 'Y'
  ts_df <- ts_df |>
    dplyr::mutate(Category = ifelse(Category == "Spawners", "Index", Category))
  
  write.csv(ts_df, file.path(tmp_dir, "sis_ts_template.csv"), row.names = FALSE)
  
  expect_error(
    export_to_sis(sis_data_dir = tmp_dir),
    regexp = "Invalid Primary category detected"
  )
  
})