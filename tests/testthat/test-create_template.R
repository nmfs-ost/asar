test_that("Can trace template files from package", {
  path <- system.file("templates", "skeleton", package = "asar")
  base_temp_files <- c(
    "01_executive_summary.qmd",
    "02_introduction.qmd",
    "03_data.qmd",
    "04a_assessment-configuration.qmd",
    "04b_assessment-results.qmd",
    "04c_assessment-sensitivity.qmd",
    "04d_assessment-benchmarks.qmd",
    "04e_assessment-projections.qmd",
    "05_discussion.qmd",
    "06_acknowledgments.qmd",
    "07_references.qmd",
    # "10_notes.qmd",
    "11_appendix.qmd"
    # "09_figures.qmd",
    # "in-header.tex",
    # "08_tables.qmd",
    # "title.tex"
  )
  expect_equal(list.files(path), base_temp_files)
})

test_that("create_template() creates correct files", {
  # Define expected report files
  expect_report_files <- c(
    "01_executive_summary.qmd",
    "02_introduction.qmd",
    "03_data.qmd",
    "04a_assessment-configuration.qmd",
    "04b_assessment-results.qmd",
    "04c_assessment-sensitivity.qmd",
    "04d_assessment-benchmarks.qmd",
    "04e_assessment-projections.qmd",
    "05_discussion.qmd",
    "06_acknowledgments.qmd",
    "07_references.qmd",
    "08_tables.qmd",
    "09_figures.qmd",
    # "10_notes.qmd",
    "11_appendix.qmd",
    "preamble.R",
    "SAR_species_skeleton.qmd",
    #   "model_results_metadata.md",
    "report_glossary.tex",
    "asar_references.bib",
    "support_files"
  )

  # Define expected support files
  expect_support_files <- c(
    "_titlepage.tex",
    "before-body.tex",
    "in-header.tex",
    "us_doc_logo.png"
  )

  # Test case 1: Provide no inputs
  path <- getwd()

  create_template() |> suppressWarnings()

  no_inputs_output_path <- file.path(path, "report")
  object_report_files <- list.files(no_inputs_output_path)
  object_support_files <- list.files(file.path(no_inputs_output_path, "support_files"))

  # Check if all expected report files are created
  expect_true(all(sort(expect_report_files) == sort(object_report_files)))

  # Check if all expected support files are created
  expect_true(all(sort(expect_support_files) == sort(object_support_files)))

  # erase temporary testing files
  unlink(fs::path(path, "report"), recursive = T)


  # Test case 2: Provide multiple inputs

  office <- "NWFSC"
  species <- "Dover sole"
  year <- 2010
  create_template(
    new_template = TRUE,
    format = "pdf",
    office = office,
    species = species,
    spp_latin = "Pomatomus saltatrix",
    year = year,
    author = c("John Snow" = "AFSC", "Danny Phantom" = "SWFSC", "Patrick Star" = "SEFSC"),
    include_affiliation = TRUE,
    parameters = FALSE
  ) |>
    suppressWarnings() |>
    suppressMessages()

  long_inputs_output_path <- file.path(path, "report")
  object_report_files <- list.files(long_inputs_output_path)
  object_support_files <- list.files(file.path(long_inputs_output_path, "support_files"))

  # Define expected report files for Dover sole
  expect_report_files <- c(
    "01_executive_summary.qmd",
    "02_introduction.qmd",
    "03_data.qmd",
    "04a_assessment-configuration.qmd",
    "04b_assessment-results.qmd",
    "04c_assessment-sensitivity.qmd",
    "04d_assessment-benchmarks.qmd",
    "04e_assessment-projections.qmd",
    "05_discussion.qmd",
    "06_acknowledgments.qmd",
    "07_references.qmd",
    "08_tables.qmd",
    "09_figures.qmd",
    # "10_notes.qmd",
    "11_appendix.qmd",
    "SAR_Dover_sole_skeleton.qmd",
    "asar_references.bib",
    "preamble.R",
    "report_glossary.tex",
    "support_files"
  )
  # Define expected support files for Dover sole
  expect_dover_sole_support_files <- c(
    "_titlepage.tex",
    "before-body.tex",
    "Dover_sole.png",
    "in-header.tex",
    "us_doc_logo.png"
  )

  # Check if all expected report files are created for Dover sole
  expect_true(all(sort(expect_report_files) == sort(object_report_files)))

  # Check if all expected support files are created for Dover sole
  expect_true(all(sort(expect_dover_sole_support_files) == sort(object_support_files)))

  # erase temporary testing files
  unlink(fs::path(path, "report"), recursive = T)
})

test_that("warning is triggered for existing files", {
  create_template(
    new_template = TRUE,
    format = "pdf",
    office = "NWFSC",
    species = "Dover sole",
    spp_latin = "Pomatomus saltatrix",
    year = 2010,
    author = c("John Snow" = "AFSC", "Danny Phantom" = "SWFSC", "Patrick Star" = "SEFSC"),
    include_affiliation = TRUE,
    parameters = FALSE
  )

  # Test if warning is triggered when there are existing files in the provided location
  # file_path <- tempfile(tmpdir = getwd())
  # on.exit(unlink(file_path), add = TRUE)
  # ifelse(!file.exists(file_path),
  #        file.create(file_path, showWarnings = FALSE),
  #        FALSE
  # )
  expect_message(
    create_template(
      new_template = TRUE,
      format = "pdf",
      office = "NWFSC",
      species = "Dover sole",
      spp_latin = "Pomatomus saltatrix",
      year = 2010,
      author = c("John Snow" = "AFSC", "Danny Phantom" = "SWFSC", "Patrick Star" = "SEFSC"),
      include_affiliation = TRUE,
      parameters = FALSE
    ),
    regexp = "There are files in this location."
  )

  path <- getwd()

  # erase temporary testing files
  unlink(fs::path(path, "report"), recursive = T)
})


test_that("file_dir works", {
  dir <- fs::path(getwd(), "data")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  ifelse(!dir.exists(dir),
    dir.create(dir, showWarnings = FALSE),
    FALSE
  )

  # read in sample dataset
  dat <- asar::convert_output(
    file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.sso"),
    model = "ss3",
    save_dir = dir
  )

  create_template(
    format = "pdf",
    office = "NWFSC",
    species = "Dover sole",
    spp_latin = "Pomatomus saltatrix",
    year = 2010,
    author = c("John Snow" = "AFSC", "Danny Phantom" = "SWFSC", "Patrick Star" = "SEFSC"),
    include_affiliation = TRUE,
    parameters = FALSE,
    model_results = file.path(dir, "std_output.rda"),
    file_dir = dir
  )

  file_path <- file.path(dir, "report")
  expect_gt(length(list.files(file_path)), 1)

  expect_gte(length(list.files(dir)), 1)
})

test_that("model_results metadata file created", {
  # identify wd on github to debug failing test there (but not locally)
  dat <- asar::convert_output(
    file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.sso"),
    model = "ss3",
    save_dir = getwd()
  )
  # print(paste0("The working directory is: ", getwd()))
  # message(paste0("The working directory is: ", getwd()))

  create_template(
    model_results = here::here("std_output.rda"),
    format = "pdf",
    office = "NWFSC",
    # species = "Dover sole",
    spp_latin = "Pomatomus saltatrix",
    year = 2010,
    author = c("John Snow" = "AFSC", "Danny Phantom" = "SWFSC", "Patrick Star" = "SEFSC"),
    include_affiliation = TRUE,
    parameters = FALSE
  )

  # message("create_template run")

  file_path <- file.path(getwd(), "report")

  # message(paste0("file_path: ", file_path))

  expect_true(file.exists(file_path))

  expect_report_files <- c(
    "01_executive_summary.qmd",
    "02_introduction.qmd",
    "03_data.qmd",
    "04a_assessment-configuration.qmd",
    "04b_assessment-results.qmd",
    "04c_assessment-sensitivity.qmd",
    "04d_assessment-benchmarks.qmd",
    "04e_assessment-projections.qmd",
    "05_discussion.qmd",
    "06_acknowledgments.qmd",
    "07_references.qmd",
    "08_tables.qmd",
    "09_figures.qmd",
    # "10_notes.qmd",
    "11_appendix.qmd",
    "SAR_species_skeleton.qmd",
    "preamble.R",
    "std_output_metadata.md",
    "report_glossary.tex",
    "asar_references.bib",
    "support_files"
  )

  object_report_files <- list.files(file_path)

  message(paste0("object_report_files: ", object_report_files))

  # Check if all expected report files are created
  expect_true(all(sort(expect_report_files) == sort(object_report_files)))

  # erase temporary testing files
  unlink(file_path, recursive = T)
  file.remove(fs::path(getwd(), "std_output.rda"))
})
