test_that("Table widths calculated correctly", {

  # convert sample dataset
  convert_output(
    output_file = "Report.sso",
    outdir = file.path("fixtures", "ss3_models", "models", "Sablefish2015"),
    model = "ss3",
    file_save = TRUE,
    savedir = getwd()
  )

  dat <- utils::read.csv(
    file.path("std_model_output.csv")
  )

  stockplotr::table_indices(dat,
                            make_rda = TRUE,
                            rda_dir = getwd())

  # extra-wide width
  tbl_width <- ID_tbl_width_class(
    plot_name = "indices.abundance_table.rda",
    rda_dir = getwd(),
    portrait_pg_width = 5
    )

  expected_output <- "extra-wide"
  expect_equal(tbl_width, expected_output)


  # regular width
  stockplotr::table_bnc(dat,
                            make_rda = TRUE,
                            rda_dir = getwd())

  tbl_width2 <- ID_tbl_width_class(
    plot_name = "bnc_table.rda",
    rda_dir = getwd(),
    portrait_pg_width = 5
  )

  expected_output <- "wide"
  expect_equal(tbl_width2, expected_output)

  # erase temporary testing files
  file.remove(fs::path(getwd(), "std_model_output.csv"))
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "rda_files"), recursive = T)
})
