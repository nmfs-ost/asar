test_that("citation generated properly for SWFSC", {
  fxn_test <- create_citation(
    author = c(
      "Peter T. Kuriyama",
      "Caitlin Allen Akselrud",
      "Juan P. Zwolinski",
      "Kevin T. Hill"
    ),
    title = "Check",
    year = 2024
  )
  expected_output <- "{{< pagebreak >}} \n\nPlease cite this publication as: \n\nKuriyama, P. T., Allen Akselrud, C., Zwolinski, J. P., and Hill, K. T. 2024. Check. Pacific Fishery Management Council, Portland, OR. Available from https://www.pcouncil.org/stock-assessments-and-fishery-evaluation-safe-documents/."
  expect_equal(fxn_test, expected_output)
})
