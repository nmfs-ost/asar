test_that("citaiton generated properly for SWFSC", {
  fxn_test <- generate_citation(
    author = c(
      "Peter T. Kuriyama",
      "Caitlin Allen Akselrud",
      "Juan P. Zwolinski",
      "Kevin T. Hill"
    ),
    title = "Check",
    year = 2024,
    office = "SWFSC"
  )
  expected_output <- "{{< pagebreak >}} \n\nPlease cite this publication as \n\nKuriyama, P.T., C.Allen Akselrud, J.P. Zwolinski, K.T. Hill. 2024. Check. NOAA Fisheries Science Center, La Jolla, CA. "
  expect_equal(fxn_test, expected_output)
})
