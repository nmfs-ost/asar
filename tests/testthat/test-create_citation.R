test_that("citation generated properly for SWFSC", {
  fxn_test <- create_citation(
    authors = c(
      "Peter T. Kuriyama" = "SWFSC",
      "Caitlin Allen Akselrud" = "SWFSC",
      "Juan P. Zwolinski" = "SWFSC",
      "Kevin T. Hill" = "SWFSC"
    ),
    title = "Check",
    year = 2024,
    type = "SAR"
  )
  expected_output <- "{{< pagebreak >}} \n\nPlease cite this publication as: \n\nKuriyama, P. T., Allen Akselrud, C., Zwolinski, J. P., and Hill, K. T. 2024. Check. Pacific Fishery Management Council, Portland, OR. Available from https://www.pcouncil.org/stock-assessments-and-fishery-evaluation-safe-documents/. \\pageref*{LastPage}{} pp."
  expect_equal(fxn_test, expected_output)
})

test_that("citation generated properly for SEFSC", {
  fxn_test <- create_citation(
    type = "sedar",
    title = "Check",
    year = 2024
  )
  expected_output <- "{{< pagebreak >}} \n\nPlease cite this publication as: \n\nSEDAR. 2024. Check. SEDAR, North Charleston SC. \\pageref*{LastPage}{} pp."
  expect_equal(fxn_test, expected_output)
})

