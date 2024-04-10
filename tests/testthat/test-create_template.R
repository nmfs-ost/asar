act <- create_template(new_template = TRUE,
                       format = "pdf",
                       office = "NEFSC",
                       region = "GB",
                       species = "Atlantic Bluefish",
                       spp_latin = "Pomatomus saltatrix",
                       year = 2024,
                       author = c("John Snow", "Danny Phantom", "Patrick Star"),
                       include_affiliation = TRUE,
                       parameters = TRUE,
                       param_names = c("fleet1", "fleet2", "model"),
                       param_values = c("Commercial", "Recreational", "Woods Hole Assessment Model"),
                       type = "RT",
                       model_results = "report.sso",
                       model = "SS")

test_that("saves template .qmd file", {
  expect_visible(
    grep(".qmd", act)
  )
})

test_that("creates template before cat() save", {
  expect_invisible(
    report_template
  )
})
