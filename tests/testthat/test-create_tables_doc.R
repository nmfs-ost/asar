test_that("Creates expected start of tables doc", {
  # create tables doc
  create_tables_doc(
    subdir = getwd(),
    include_all = TRUE,
    tables_dir = getwd()
  )

  # read in tables doc
  table_content <- readLines("08_tables.qmd")
  # extract first 8 lines
  head_table_content <- head(table_content, 8)
  # remove line numbers and collapse
  tc_pasted <- paste(head_table_content, collapse = "")

  # expected tables doc head
  expected_head_table_content <- "## Tables {#sec-tables}"
 # expected_head_table_content <- "## Tables {#sec-tables} ```{r} #| label: 'set-rda-dir-tbls'#| echo: false #| warning: false #| eval: true #| include: false"

  # test expectation of start of tables doc
  testthat::expect_equal(
    tc_pasted,
    expected_head_table_content
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "08_tables.qmd"))
})
