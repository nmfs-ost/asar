test_that("Creates expected start of nearly empty tables doc", {
  # create tables doc
  create_tables_doc(
    subdir = getwd(),
    tables_dir = getwd()
  )

  # read in tables doc
  table_content <- readLines("08_tables.qmd")
  # extract first 8 lines
  head_table_content <- head(table_content, 8)
  # remove line numbers and collapse
  tc_pasted <- paste(head_table_content, collapse = "")

  # expected tables doc head
  expected_head_table_content <- "# Tables {#sec-tables}"

 # expected_head_table_content <- "## Tables {#sec-tables} ```{r} #| label: 'set-rda-dir-tbls'#| echo: false #| warning: false #| eval: true #| include: false"

  # test expectation of start of tables doc
  testthat::expect_equal(
    tc_pasted,
    expected_head_table_content
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "08_tables.qmd"))
})

# this won't pass until stockplotr is updated to output to 'figures' and 'tables'
# folders rather than "rda_dir"
# test_that("Creates expected start of tables doc with table", {
#
#   # convert sample dataset
#   dat <- convert_output(
#     output_file = file.path("fixtures", "ss3_models", "models", "Hake_2018",
#                             "Report.sso"),
#     model = "ss3"
#   )
#
#   stockplotr::table_landings(dat,
#                             make_rda = TRUE,
#                             rda_dir = getwd())
#
#   # create tables doc
#   create_tables_doc(
#     subdir = getwd(),
#     tables_dir = getwd()
#   )
#
#   # read in tables doc
#   table_content <- readLines("08_tables.qmd")
#   # extract first 7 lines
#   head_table_content <- head(table_content, 7)
#   # remove line numbers and collapse
#   fc_pasted <- paste(head_table_content, collapse = "")
#
#   # expected tables doc head
#   expected_head_table_content <- "# Tables {#sec-tables} ```{r} #| label: 'set-rda-dir-tbls'#| echo: false #| warning: false #| eval: true "
#
#   # test expectation of start of tables doc
#   testthat::expect_equal(
#     fc_pasted,
#     expected_head_table_content
#   )
#
#   # erase temporary testing files
#   file.remove(fs::path(getwd(), "08_tables.qmd"))
#   file.remove(fs::path(getwd(), "captions_alt_text.csv"))
#   unlink(fs::path(getwd(), "rda_files"), recursive = T)
# })
