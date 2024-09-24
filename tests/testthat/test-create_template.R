test_that("Can trace template files from package", {
  path <- system.file("templates", "skeleton", package = "asar")
  base_temp_files <- c(
    "acknowledgments.qmd",
    "appendix.qmd",
    "assessment_glossaries.tex",
    "data.qmd",
    "discussion.qmd",
    "executive_summary.qmd",
    # "figures.qmd",
    # "in-header.tex",
    "introduction.qmd",
    "modeling_approach.qmd",
    "projections.qmd",
    "references.qmd",
    "results.qmd"
    # "tables.qmd",
    # "title.tex"
  )
  expect_equal(list.files(path), base_temp_files)
})

test_that("create_template() creates correct files", {
  # Define expected report files
  expect_report_files <- c(
    "acknowledgments.qmd",
    "appendix.qmd",
    "assessment_glossaries.tex",
    "data.qmd",
    "discussion.qmd",
    "executive_summary.qmd",
    "figures.qmd",
    "introduction.qmd",
    "modeling_approach.qmd",
    "projections.qmd",
    "references.qmd",
    "results.qmd",
    "SAR_species_skeleton.qmd",
    "support_files",
    "tables.qmd"
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

  create_template()

  no_inputs_output_path <- file.path(path, "stock_assessment_reports", "report")
  object_report_files <- list.files(no_inputs_output_path)
  object_support_files <- list.files(file.path(no_inputs_output_path, "support_files"))

  # Check if all expected report files are created
  expect_true(all(expect_report_files == object_report_files))

  # Check if all expected support files are created
  expect_true(all(expect_support_files == object_support_files))

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
    author = c("John Snow", "Danny Phantom", "Patrick Star"),
    include_affiliation = TRUE,
    parameters = FALSE,
    resdir = NULL,
    model_results = "Report.sso",
    model = "SS3"
  )

  long_inputs_output_path <- file.path(path, "stock_assessment_reports", office, species, year)
  object_report_files <- list.files(long_inputs_output_path)
  object_support_files <- list.files(file.path(long_inputs_output_path, "support_files"))

  # Define expected support files for Dover sole
  expect_dover_sole_support_files <- c(
    "_titlepage.tex",
    "before-body.tex",
    "Dover_sole.png",
    "in-header.tex",
    "us_doc_logo.png"
  )

  # Check if all expected report files are created for Dover sole
  expect_true(all(expect_report_files == object_report_files))

  # Check if all expected support files are created for Dover sole
  expect_true(all(expect_dover_sole_support_files == object_support_files))
})

test_that("warning is triggered for missing models", {
  # Test if warning is triggered when resdir is NULL and results or model name is not defined
  expect_warning(
    create_template(
      new_template = TRUE,
      format = "pdf",
      office = "NWFSC",
      species = "Dover sole",
      spp_latin = "Pomatomus saltatrix",
      year = 2010,
      author = c("John Snow", "Danny Phantom", "Patrick Star"),
      include_affiliation = TRUE,
      parameters = FALSE,
      resdir = NULL,
      model_results = NULL,
      model = NULL
    ),
    regexp = "Results file or model name not defined."
  )
})

test_that("warning is triggered for existing files", {
  # Test if warning is triggered when there are existing files in the provided location
  file_path <- tempfile(tmpdir = getwd())
  on.exit(unlink(file_path), add = TRUE)
  ifelse(!file.exists(file_path),
    file.create(file_path, showWarnings = FALSE),
    FALSE
  )
  expect_warning(
    create_template(
      new_template = TRUE,
      format = "pdf",
      office = "NWFSC",
      species = "Dover sole",
      spp_latin = "Pomatomus saltatrix",
      year = 2010,
      author = c("John Snow", "Danny Phantom", "Patrick Star"),
      include_affiliation = TRUE,
      parameters = FALSE,
      resdir = NULL,
      model_results = "Report.sso",
      model = "SS3"
    ),
    regexp = "There are files in this location."
  )
})

