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

test_that("Uses explicit tables doc name without creating default tables doc", {
  report_dir <- fs::path(getwd(), "report")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(fs::path(report_dir, "06_tables.qmd"))

  create_tables_doc(
    subdir = report_dir,
    tables_dir = getwd(),
    tables_doc_name = "06_tables.qmd"
  )

  expect_true(file.exists(fs::path(report_dir, "06_tables.qmd")))
  expect_false(file.exists(fs::path(report_dir, "08_tables.qmd")))

  unlink(report_dir, recursive = TRUE)
})

test_that("Creates expected start of tables doc with table", {
  create_template()

  stockplotr::table_landings(
    stockplotr::example_data,
    make_rda = TRUE,
    interactive = FALSE
  )

  # create tables doc
  create_tables_doc(
    subdir = file.path(getwd(), "report"),
    tables_dir = getwd()
  )

  # read in tables doc
  tables_doc_name <- id_fig_tab_num(
    subdir = file.path(getwd(), "report"),
    fig_or_tab = "table",
    type = "default"
  )$detected_doc_name
  table_content <- readLines(file.path(getwd(), "report", tables_doc_name))
  # extract first 8 lines
  head_table_content <- head(table_content, 8)
  # remove line numbers and collapse
  fc_pasted <- paste(head_table_content, collapse = "")

  # expected tables doc head
  expected_head_table_content <- "# Tables {#sec-tables} ```{r} #| label: 'set-rda-dir-tbls'#| echo: false #| warning: false #| include: falselibrary(gt)"

  # test expectation of start of tables doc
  testthat::expect_equal(
    fc_pasted,
    expected_head_table_content
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
  unlink(fs::path(getwd(), "report"), recursive = T)
})

# moot bc new checks account for this
# not sure when this might happen
# test_that("Throws warning if chunks with identical labels", {
#   stockplotr::table_landings(
#     stockplotr::example_data,
#     make_rda = TRUE,
#     interactive = FALSE
#   )
#
#   # create tables doc
#   create_tables_doc(
#     subdir = getwd(),
#     tables_dir = getwd()
#   )
#
#   expect_message(
#     create_tables_doc(
#       subdir = getwd(),
#       tables_dir = getwd()
#     ),
#     "Tables doc contains chunks with identical labels:"
#   )
#
#   # erase temporary testing files
#   file.remove(fs::path(getwd(), "08_tables.qmd"))
#   file.remove(fs::path(getwd(), "captions_alt_text.csv"))
#   file.remove(fs::path(getwd(), "key_quantities.csv"))
#   unlink(fs::path(getwd(), "tables"), recursive = T)
# })

test_that("Formerly empty tables doc renders correctly", {
  # create empty tables doc
  create_template()

  # create example table
  stockplotr::table_landings(
    dat = stockplotr::example_data,
    interactive = FALSE,
    make_rda = TRUE,
    module = "CATCH"
  )

  # rerender tables doc, appending new table
  create_tables_doc(
    subdir = file.path(getwd(), "report"),
    tables_dir = getwd()
  )

  # read in tables doc
  tables_doc_name <- id_fig_tab_num(
    subdir = file.path(getwd(), "report"),
    fig_or_tab = "table",
    type = "default"
  )$detected_doc_name
  table_content <- readLines(file.path(getwd(), "report", tables_doc_name))
  # extract first 8 lines
  head_table_content <- head(table_content, 8)
  # remove line numbers and collapse
  fc_pasted <- paste(head_table_content, collapse = "")

  # expected tables doc head
  expected_head_table_content <- "# Tables {#sec-tables} ```{r} #| label: 'set-rda-dir-tbls'#| echo: false #| warning: false #| include: falselibrary(gt)"

  # test expectation of start of tables doc
  expect_equal(
    fc_pasted,
    expected_head_table_content
  )

  # erase temporary testing files
  # file.remove(fs::path(getwd(), "08_tables.qmd"))
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
  unlink(fs::path(getwd(), "report"), recursive = T)
})

test_that("Adds new table from tables folder.", {
  # Create one table
  stockplotr::table_landings(
    dat = stockplotr::example_data,
    make_rda = TRUE,
    module = "CATCH"
  )

  create_template()

  # Make another table
  # stockplotr::table_index(
  #   dat = stockplotr::example_data,
  #   make_rda = TRUE
  # )
  # temporarily create a copy of landings table
  # add above index table once ready
  file.copy(
    from = file.path(getwd(), "tables", "landings_table.rda"),
    to = file.path(getwd(), "tables", "land_table.rda")
  )

  # rerender tables doc, appending new table
  create_tables_doc(
    subdir = file.path(getwd(), "report"),
    tables_dir = getwd()
  )

  # read in tables doc
  tables_doc_name <- id_fig_tab_num(
    subdir = file.path(getwd(), "report"),
    fig_or_tab = "table",
    type = "default"
  )$detected_doc_name
  table_content <- readLines(file.path(getwd(), "report", tables_doc_name))
  # Remove the first lines so test doesn't test path differences
  # Note: you CAN NOT test rendering with this approach
  table_content <- table_content[-c(3:11)]
  # remove line numbers and collapse
  fc_pasted <- paste(table_content, collapse = "\n")

  # test expectation of start of tables doc
  expect_snapshot(
    cat(fc_pasted)
  )

  # erase temporary testing files
  file.remove(fs::path("captions_alt_text.csv"))
  file.remove(fs::path("key_quantities.csv"))
  unlink(fs::path("tables"), recursive = T)
  unlink(fs::path("report"), recursive = T)
})
