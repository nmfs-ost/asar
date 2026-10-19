test_that("add_child() adds labeled child chunks and separates them", {
  output <- add_child(
    x = c("introduction.qmd", "tables.qmd"),
    label = c("introduction", "tables")
  )

  expect_true(grepl("knit_child('introduction.qmd'", output, fixed = TRUE))
  expect_true(grepl("knit_child('tables.qmd'", output, fixed = TRUE))
  expect_true(grepl("{{< pagebreak >}}", output, fixed = TRUE))
  expect_true(grepl("\\clearpage", output, fixed = TRUE))
})
