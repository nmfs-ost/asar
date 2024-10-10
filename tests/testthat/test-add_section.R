test_that("Adding new sections works.", {
  sec_list <- add_base_section(c("introduction", "assessment", "discussion"))
  sections <- add_section(
    new_section = c("abstract", "ecosystem_considerations", "alt_models"),
    section_location = c("before-introduction", "after-discussion", "after-assessment"),
    custom_sections = sec_list,
    subdir = tempdir()
  )
  exp_list <- list(
    "abstract.qmd",
    "02_introduction.qmd",
    "04a_assessment-configuration.qmd",
    "04b_assessment-results.qmd",
    "04c_assessment-sensitivity.qmd",
    "04d_assessment-benchmarks.qmd",
    "04e_assessment-projections.qmd",
    "alt_models.qmd",
    "05_discussion.qmd",
    "ecosystem_considerations.qmd"
  )
  expect_equal(sections, exp_list)
})

test_that("Adding new sections does not work.", {
  sec_list <- add_base_section(c("introduction", "data", "assessment", "acknowledgments", "appendix", "references"))
  sections <- add_section(
    new_section = c("Harvest Control Rules", "Regional Management Considerations", "Research and Data Needs"),
    section_location = c("after-assessment", "after-assessment", "after-assessment"),
    custom_sections = sec_list,
    subdir = tempdir()
  )
  exp_list <- list(
    "abstract.qmd",
    "02_introduction.qmd",
    "03_data.qmd",
    "04a_assessment-configuration.qmd",
    "04b_assessment-results.qmd",
    "04c_assessment-sensitivity.qmd",
    "04d_assessment-benchmarks.qmd",
    "04e_assessment-projections.qmd",
    "harvest_control_rules.qmd",
    "regional_management_considerations.qmd",
    "research_and_data_needs.qmd",
    "06_acknowledgements.qmd",
    "11_appendix.qmd",
    "07_references.qmd"
  )
  expect_equal(sections, exp_list)
})


# test_that("Error thrown if section location is designated as in.", {
#   sec_list <- c(
#     "introduction.qmd",
#     "model.qmd",
#     "results.qmd",
#     "discussion.qmd"
#   )
#   # sections <- add_section(sec_names = c("abstract","ecosystem_considerations", "alt_models", "stock_status"),
#   #                         location = c("before-introduction", "after-discussion","after-model", "in-discussion"),
#   #                         other_sections = sec_list,
#   #                         subdir = tempdir())
#
#   expect_that(add_section(sec_names = c("abstract","ecosystem_considerations", "alt_models", "stock_status"),
#                            location = c("before-introduction", "after-discussion","after-model", "in-discussion"),
#                            other_sections = sec_list,
#                            subdir = tempdir()),
#                throws_error("Error: No available option for adding a new section 'in' another quarto document."))
# })
