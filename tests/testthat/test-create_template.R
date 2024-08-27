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
    "SAR_region_species_skeleton.qmd",
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
  expect_warning(create_template(
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
  regexp = "Results file or model name not defined.")
})

test_that("warning is triggered for existing files", {

  # Test if warning is triggered when there are existing files in the provided location
  file_path <- tempfile(tmpdir = getwd())
  on.exit(unlink(file_path), add = TRUE)
  ifelse(!file.exists(file_path),
         file.create(file_path, showWarnings = FALSE),
         FALSE)
  expect_warning(create_template(
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
  regexp = "There are files in this location.")

})


# Test it creates the current full template before cat() and save
# This will need to be update

# test_that("Create accurrate skeleton script."{
#   expect_snapshot(create_template(
#     new_template = TRUE,
#     format = "docx",
#     office = "SWFSC",
#     species = "Pacific sardine",
#     spp_latin = "Sardinops sagax",
#     year = 2020,
#     type = "RT",
#     author = c("Peter T. Kuriyama", "Caitlin Allen Akselrud", "Juan P. Zwolinski", "Kevin T. Hill"),
#     include_affiliation = TRUE,
#     simple_affiliation = TRUE,
#     param_names = c("NBC","PNW"),
#     param_values = c("northern Baja California", "Pacific Northwest"),
#     resdir = "C:/Users/samantha.schiano/Documents/Stock Assessment Workflow/Regional FSC Resources/SWFSC/2024_sardine/model/modb8i_base",
#     model_results = c("Report.sso","ss_summary.sso","CumReport.sso"),
#     model = "SS",
#     custom = TRUE,
#     custom_sections = c("introduction","data","model","acknowledgments","tables","figures","appendix","references"),
#     new_section = c("Harvest Control Rules", "Regional Management Considerations","Research and Data Needs"),
#     section_location = c("after-model", "after-model", "after-model")
#   ))
#   test_skeleton <- create_template(
#     new_template = TRUE,
#     format = "docx",
#     office = "SWFSC",
#     species = "Pacific sardine",
#     spp_latin = "Sardinops sagax",
#     year = 2024,
#     type = "RT",
#     author = c("Peter T. Kuriyama", "Caitlin Allen Akselrud", "Juan P. Zwolinski", "Kevin T. Hill"),
#     include_affiliation = TRUE,
#     simple_affiliation = TRUE,
#     param_names = c("NBC","PNW"),
#     param_values = c("northern Baja California", "Pacific Northwest"),
#     resdir = "C:/Users/samantha.schiano/Documents/Stock Assessment Workflow/Regional FSC Resources/SWFSC/2024_sardine/model/modb8i_base",
#     model_results = c("Report.sso","ss_summary.sso","CumReport.sso"),
#     model = "SS",
#     custom = TRUE,
#     custom_sections = c("introduction","data","model","acknowledgments","tables","figures","appendix","references"),
#     new_section = c("Harvest Control Rules", "Regional Management Considerations","Research and Data Needs"),
#     section_location = c("after-model", "after-model", "after-model")
#   )
#
#   verify_skeleton <- "---\ntitle: 'Status of the Pacific sardine stock along the U.S. West Coast in 2020'\nauthor:\n  - name: 'Caitlin Allen Akselrud'\n    affiliations: 'Southwest Fisheries Science Center'\n  - name: 'Kevin T. Hill'\n    affiliations: 'Southwest Fisheries Science Center'\n  - name: 'Peter T. Kuriyama'\n    affiliations: 'Southwest Fisheries Science Center'\n  - name: 'Juan P. Zwolinski'\n    affiliations: 'Southwest Fisheries Science Center'\ndate: today\nformat: \n  docx: \n    toc: true \n    keep-tex: true \n    template-partials: \n       - title.tex \n    include-in-header: \n       - in-header.tex \nparams:\n   office: 'SWFSC'\n   species: 'Pacific sardine'\n   spp_latin: 'Sardinops sagax'\n   NBC: 'northern Baja California'\n   PNW: 'Pacific Northwest'\n---\n```{r} \n#| label: 'model_output'\n#| echo: false \n#| warning: false \n#| eval: false \nconvert_output(output.file = c('Report.sso', 'ss_summary.sso', 'CumReport.sso'), model = 'SS', outdir = 'C:/Users/samantha.schiano/Documents/Stock Assessment Workflow/Regional FSC Resources/SWFSC/2024_sardine/model/modb8i_base')\n``` \n\n{{< pagebreak >}} \n\nPlease cite this publication as \n\nKuriyama, P.T., C.Allen Akselrud, J.P. Zwolinski, K.T. Hill. 2020. Status of the Pacific sardine stock along the U.S. West Coast in 2020. NOAA Fisheries Science Center, La Jolla, CA. \n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '02_introduction'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('02_introduction.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '03_data'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('03_data.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '04_model'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('04_model.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: 'Harvest_Control_Rules'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('Harvest_Control_Rules.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: 'Regional_Management_Considerations'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('Regional_Management_Considerations.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: 'Research_and_Data_Needs'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('Research_and_Data_Needs.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '07_acknowledgments'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('07_acknowledgments.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '09_tables'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('09_tables.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '10_figures'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('10_figures.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '11_appendix'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('11_appendix.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '08_references'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('08_references.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n"
#
#   expect_equal(cat())
#
# })
