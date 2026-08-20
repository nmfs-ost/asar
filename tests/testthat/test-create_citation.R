test_that("citation generated properly for SWFSC", {
  fxn_test <- create_citation(
    authors = c(
      "Peter T. Kuriyama" = "SWFSC",
      "Caitlin Allen Akselrud" = "SWFSC",
      "Juan P. Zwolinski" = "SWFSC",
      "Kevin T. Hill" = "SWFSC"
    ),
    title = "Check",
    year = 2024
  )
  expected_output <- "{{< pagebreak >}} \n\nPlease cite this publication as: \n\nKuriyama, P. T., Allen Akselrud, C., Zwolinski, J. P., and Hill, K. T. 2024. Check. Pacific Fishery Management Council, Portland, OR. Available from https://www.pcouncil.org/stock-assessments-and-fishery-evaluation-safe-documents/. \\pageref*{LastPage}{} pp."
  expect_equal(fxn_test, expected_output)
})

test_that("single-component author names are supported", {
  initials_citation <- create_citation(
    authors = c("SB" = "SWFSC"),
    title = "Check",
    year = 2024
  )
  mononym_citation <- create_citation(
    authors = c("Joe" = "SWFSC"),
    title = "Check",
    year = 2024
  )
  mixed_citation <- create_citation(
    authors = c("SB" = "SWFSC", "Peter T. Kuriyama" = "SWFSC"),
    title = "Check",
    year = 2024
  )

  expect_match(initials_citation, "SB 2024", fixed = TRUE)
  expect_match(mononym_citation, "Joe 2024", fixed = TRUE)
  expect_match(mixed_citation, "SB, and Kuriyama, P. T. 2024", fixed = TRUE)
})

test_that("duplicate author names are supported in citations", {
  same_name_same_affiliation <- create_citation(
    authors = c("Jane Doe" = "SWFSC", "Jane Doe" = "SWFSC"),
    title = "Check",
    year = 2024
  )

  same_name_different_affiliation <- create_citation(
    authors = c("Jane Doe" = "SWFSC", "Jane Doe" = "AFSC"),
    title = "Check",
    year = 2024
  )

  same_last_with_middle_name <- create_citation(
    authors = c("Jane A. Doe" = "SWFSC", "Jane Doe" = "SWFSC"),
    title = "Check",
    year = 2024
  )

  expect_match(same_name_same_affiliation, "Doe, J., and Doe, J. 2024", fixed = TRUE)
  expect_match(same_name_different_affiliation, "Doe, J., and Doe, J. 2024", fixed = TRUE)
  expect_match(same_last_with_middle_name, "Doe, J. A., and Doe, J. 2024", fixed = TRUE)
})
