test_that("format_quarto() builds PDF and HTML formatting", {
  pdf <- format_quarto(format = "pdf", type = "pfmc")
  pdf_sar <- format_quarto(format = "pdf", type = "sar")
  html <- format_quarto(format = "html")

  expect_true(grepl("support_files/pfmc.tex", pdf, fixed = TRUE))
  expect_false(grepl("support_files/pfmc.tex", pdf_sar, fixed = TRUE))
  expect_true(grepl("support_files/theme.scss", html, fixed = TRUE))
  expect_false(grepl("support_files/pfmc.tex", html, fixed = TRUE))
})

test_that("format_quarto() reports unsupported formats", {
  expect_error(format_quarto(format = "docx"), "not yet a functional format")
  expect_error(format_quarto(format = "other"), "Invalid render format")
})
