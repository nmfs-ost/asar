testthat::test_that("Can trace template files from package", {
  path <- system.file("templates", "skeleton", package = "ASAR")
  base_temp_files <- c("00_abstract.qmd",
                       "01_executive_summary.qmd",
                       "01a_proj_table.qmd",
                       "01b_refpt_table.qmd",
                       "02_introduction.qmd",
                       "03_data.qmd",
                       "04_model.qmd",
                       "05_results.qmd",
                       "06_discussion.qmd",
                       "07_acknowledgements.qmd",
                       "08_references.qmd",
                       "09_tables.qmd",
                       "10_figures.qmd",
                       "11_appendix.qmd",
                       "assessment_glossaries.tex",
                       "in-header.tex",
                       "title.tex"
                       )
  expect_equal(list.files(path), base_temp_files)
})

