test_that("Creates expected start of nearly empty tables doc", {
  # create tables doc
  create_tables_doc(
    subdir = getwd(),
    tables_dir = getwd()
  )

  # read in tables doc
  table_content <- readLines("08_tables.qmd")
  # extract first line
  head_table_content <- table_content[1]
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

test_that("Creates expected start of tables doc with table", {
  # load sample dataset
  load(file.path(
    "fixtures", "ss3_models_converted", "Hake_2018",
    "std_output.rda"
  ))

  stockplotr::table_landings(out_new,
    make_rda = TRUE
  )

  # create tables doc
  create_tables_doc(
    subdir = getwd(),
    tables_dir = getwd()
  )

  # read in tables doc
  table_content <- readLines("08_tables.qmd")
  # extract first 7 lines
  head_table_content <- head(table_content, 7)
  # remove line numbers and collapse
  fc_pasted <- paste(head_table_content, collapse = "")

  # expected tables doc head
  expected_head_table_content <- "# Tables {#sec-tables} ```{r} #| label: 'set-rda-dir-tbls'#| echo: false #| warning: false #| include: false"

  # test expectation of start of tables doc
  testthat::expect_equal(
    fc_pasted,
    expected_head_table_content
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "08_tables.qmd"))
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("Throws warning if chunks with identical labels", {
  # load sample dataset
  load(file.path(
    "fixtures", "ss3_models_converted", "Hake_2018",
    "std_output.rda"
  ))
  
  stockplotr::table_landings(out_new,
                             make_rda = TRUE
  )
  
  # create tables doc
  create_tables_doc(
    subdir = getwd(),
    tables_dir = getwd()
  )
  
  expect_message(
    create_tables_doc(
      subdir = getwd(),
      tables_dir = getwd()
    ),
    "Tables doc will not render if chunks have identical labels."
  )
  
  # erase temporary testing files
  file.remove(fs::path(getwd(), "08_tables.qmd"))
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
})
