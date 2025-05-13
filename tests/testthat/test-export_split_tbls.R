test_that("Number of split tables is calculated correctly for converted bam model", {
  # bam model with table split into 2
  dat <- utils::read.csv(
    file.path("fixtures", "bam_models_converted", "bsb_conout.csv")
    # file.path("tests", "testthat", "fixtures", "bsb_conout.csv")
  )

  stockplotr::table_indices(dat,
    make_rda = TRUE,
    rda_dir = getwd()
  )

  # indices table
  num_tabs <- export_split_tbls(
    rda_dir = getwd(),
    plot_name = "indices.abundance_table.rda",
    essential_columns = 1
  )

  # expect 2 tables
  expected_output <- 2
  expect_equal(num_tabs, expected_output)

  # expect to see an "indices.abundance_table_split.rda" file
  expect_no_error("indices.abundance_table_split.rda" %in% list.files(file.path("rda_files")))

  # expect that an object "table_list" imported into environment
  expect_no_error(load(file.path("rda_files", "indices.abundance_table_split.rda")))

  expect_equal(length(table_list), 2)

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "rda_files"), recursive = T)
})
