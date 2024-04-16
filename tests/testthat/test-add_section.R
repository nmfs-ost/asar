test_that("Adding new sections works.", {
  sec_list <- c(
    "introduction.qmd",
    "model.qmd",
    "results.qmd",
    "discussion.qmd"
  )
  sections <- add_section(
    sec_names = c("abstract", "ecosystem_considerations", "alt_models"),
    location = c("before-introduction", "after-discussion", "after-model"),
    other_sections = sec_list,
    subdir = tempdir()
  )
  exp_list <- c(
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
