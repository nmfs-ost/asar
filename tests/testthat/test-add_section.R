test_that("Adding new sections works.", {
  sec_list <- add_base_section(c("introduction", "model", "results", "discussion"))
  sections <- add_section(
    sec_names = c("abstract", "ecosystem_considerations", "alt_models"),
    location = c("before-introduction", "after-discussion", "after-model"),
    other_sections = sec_list,
    subdir = tempdir()
  )
  exp_list <- list(
    "abstract.qmd",
    "introduction.qmd",
    "model.qmd",
    "alt_models.qmd",
    "results.qmd",
    "discussion.qmd",
    "ecosystem_considerations.qmd"
  )
  expect_equal(sections, exp_list)
})

test_that("Adding new sections works.", {
  sec_list <- add_base_section(c("introduction", "data", "model", "acknowledgements", "tables", "figures", "appendix", "references"))
  sections <- add_section(
    sec_names = c("Harvest Control Rules", "Regional Management Considerations", "Research and Data Needs"),
    location = c("after-model", "after-model", "after-model"),
    other_sections = sec_list,
    subdir = tempdir()
  )
  exp_list <- list(
    "introduction.qmd", "data.qmd", "model.qmd", "Harvest_Control_Rules.qmd",
    "Regional_Management_Considerations.qmd", "Research_and_Data_Needs.qmd",
    "acknowledgements.qmd", "tables.qmd", "figures.qmd", "appendix.qmd", "references.qmd"
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
