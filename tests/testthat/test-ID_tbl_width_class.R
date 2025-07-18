test_that("Table widths calculated correctly", {
  # convert sample dataset
  dat <- convert_output(
    file = file.path(
      "fixtures", "ss3_models", "models", "Sablefish2015",
      "Report.sso"
    ),
    model = "ss3"
  )

  stockplotr::table_indices(dat,
    make_rda = TRUE,
    tables_dir = getwd()
  )

  # wide width
  tbl_width <- ID_tbl_width_class(
    plot_name = "indices.abundance",
    tables_dir = getwd(),
    portrait_pg_width = 5
  )

  expected_output <- "wide"
  expect_equal(tbl_width, expected_output)


  # wide width
  stockplotr::table_bnc(dat,
    make_rda = TRUE,
    tables_dir = getwd()
  )

  tbl_width2 <- ID_tbl_width_class(
    plot_name = "bnc",
    tables_dir = getwd(),
    portrait_pg_width = 5
  )

  expected_output <- "wide"
  expect_equal(tbl_width2, expected_output)

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
})
