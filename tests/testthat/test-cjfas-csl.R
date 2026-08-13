test_that("cjfas CSL includes stock-assessment report formatting", {
  csl <- paste(
    readLines(
      system.file("resources", "cjfas.csl", package = "asar"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(csl, '<if type="report" variable="genre" match="all">', fixed = TRUE)
  expect_match(csl, '<text macro="issued-month-year"/>', fixed = TRUE)
  expect_match(csl, '<text value="Accessible at"/>', fixed = TRUE)
})
