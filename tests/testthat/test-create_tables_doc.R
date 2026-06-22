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
  stockplotr::table_landings(
    stockplotr::example_data,
    make_rda = TRUE,
    interactive = FALSE
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
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
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
  table_content <- readLines(file.path(getwd(), "report", "08_tables.qmd"))
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
  # Create one figure
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
  
  # rerender figures doc, appending new figure
  create_tables_doc(
    subdir = file.path(getwd(), "report"),
    tables_dir = getwd()
  )
  
  # read in figures doc
  table_content <- readLines(file.path(getwd(), "report", "08_tables.qmd"))
  # remove line numbers and collapse
  fc_pasted <- paste(table_content, collapse = "")
  
  # expected figures doc head
  expected_head_table_content <- "# Tables {#sec-tables} ```{r} #| label: 'set-rda-dir-tbls'#| echo: false #| warning: false #| include: falselibrary(gt)tables_dir <- fs::path('C:/Users/samantha.schiano.NMFS/Documents/GitHub/asar', 'tables')``` ```{r} #| label: 'tab-landings-setup'#| warnings: false #| eval: true# load rdaload(file.path(tables_dir, 'landings_table.rda'))# save rda with table-specific namelandings_table_rda <- rda# save table and caption as separate objectslandings_table <- landings_table_rda$tablelandings_cap <- landings_table_rda$caption``` ::: {.landscape}```{r} #| label: 'tbl-landings1'#| echo: false #| tbl-cap: !expr paste0(landings_cap, ' (1 of 6)') #| tbl-pos: 't'# plot table 1landings_table |>  gt::tab_options(    table.width = pct(100),    table.layout = 'auto'  ) |>  gt::cols_width(    everything() ~ pct(20)  ) |>  asar::gt_split(row_every_n = 28) |> gt::grp_pull(1)``` :::::: {.landscape}```{r} #| label: 'tbl-landings2'#| echo: false #| tbl-cap: !expr paste0(landings_cap, ' (2 of 6)') #| tbl-pos: 't'# plot table 2landings_table |>  gt::tab_options(    table.width = pct(100),    table.layout = 'auto'  ) |>  gt::cols_width(    everything() ~ pct(20)  ) |>  asar::gt_split(row_every_n = 28) |> gt::grp_pull(2)``` :::::: {.landscape}```{r} #| label: 'tbl-landings3'#| echo: false #| tbl-cap: !expr paste0(landings_cap, ' (3 of 6)') #| tbl-pos: 't'# plot table 3landings_table |>  gt::tab_options(    table.width = pct(100),    table.layout = 'auto'  ) |>  gt::cols_width(    everything() ~ pct(20)  ) |>  asar::gt_split(row_every_n = 28) |> gt::grp_pull(3)``` :::::: {.landscape}```{r} #| label: 'tbl-landings4'#| echo: false #| tbl-cap: !expr paste0(landings_cap, ' (4 of 6)') #| tbl-pos: 't'# plot table 4landings_table |>  gt::tab_options(    table.width = pct(100),    table.layout = 'auto'  ) |>  gt::cols_width(    everything() ~ pct(20)  ) |>  asar::gt_split(row_every_n = 28) |> gt::grp_pull(4)``` :::::: {.landscape}```{r} #| label: 'tbl-landings5'#| echo: false #| tbl-cap: !expr paste0(landings_cap, ' (5 of 6)') #| tbl-pos: 't'# plot table 5landings_table |>  gt::tab_options(    table.width = pct(100),    table.layout = 'auto'  ) |>  gt::cols_width(    everything() ~ pct(20)  ) |>  asar::gt_split(row_every_n = 28) |> gt::grp_pull(5)``` :::::: {.landscape}```{r} #| label: 'tbl-landings6'#| echo: false #| tbl-cap: !expr paste0(landings_cap, ' (6 of 6)') #| tbl-pos: 't'# plot table 6landings_table |>  gt::tab_options(    table.width = pct(100),    table.layout = 'auto'  ) |>  gt::cols_width(    everything() ~ pct(20)  ) |>  asar::gt_split(row_every_n = 28) |> gt::grp_pull(6)``` :::{{< pagebreak >}} ```{r} #| label: 'tab-land-setup'#| warnings: false #| eval: true# load rdaload(file.path(tables_dir, 'land_table.rda'))# save rda with table-specific nameland_table_rda <- rda# save table and caption as separate objectsland_table <- land_table_rda$tableland_cap <- land_table_rda$caption``` ::: {.landscape}```{r} #| label: 'tbl-land1'#| echo: false #| tbl-cap: !expr paste0(land_cap, ' (1 of 6)') #| tbl-pos: 't'# plot table 1land_table |>  gt::tab_options(    table.width = pct(100),    table.layout = 'auto'  ) |>  gt::cols_width(    everything() ~ pct(20)  ) |>  asar::gt_split(row_every_n = 28) |> gt::grp_pull(1)``` :::::: {.landscape}```{r} #| label: 'tbl-land2'#| echo: false #| tbl-cap: !expr paste0(land_cap, ' (2 of 6)') #| tbl-pos: 't'# plot table 2land_table |>  gt::tab_options(    table.width = pct(100),    table.layout = 'auto'  ) |>  gt::cols_width(    everything() ~ pct(20)  ) |>  asar::gt_split(row_every_n = 28) |> gt::grp_pull(2)``` :::::: {.landscape}```{r} #| label: 'tbl-land3'#| echo: false #| tbl-cap: !expr paste0(land_cap, ' (3 of 6)') #| tbl-pos: 't'# plot table 3land_table |>  gt::tab_options(    table.width = pct(100),    table.layout = 'auto'  ) |>  gt::cols_width(    everything() ~ pct(20)  ) |>  asar::gt_split(row_every_n = 28) |> gt::grp_pull(3)``` :::::: {.landscape}```{r} #| label: 'tbl-land4'#| echo: false #| tbl-cap: !expr paste0(land_cap, ' (4 of 6)') #| tbl-pos: 't'# plot table 4land_table |>  gt::tab_options(    table.width = pct(100),    table.layout = 'auto'  ) |>  gt::cols_width(    everything() ~ pct(20)  ) |>  asar::gt_split(row_every_n = 28) |> gt::grp_pull(4)``` :::::: {.landscape}```{r} #| label: 'tbl-land5'#| echo: false #| tbl-cap: !expr paste0(land_cap, ' (5 of 6)') #| tbl-pos: 't'# plot table 5land_table |>  gt::tab_options(    table.width = pct(100),    table.layout = 'auto'  ) |>  gt::cols_width(    everything() ~ pct(20)  ) |>  asar::gt_split(row_every_n = 28) |> gt::grp_pull(5)``` :::::: {.landscape}```{r} #| label: 'tbl-land6'#| echo: false #| tbl-cap: !expr paste0(land_cap, ' (6 of 6)') #| tbl-pos: 't'# plot table 6land_table |>  gt::tab_options(    table.width = pct(100),    table.layout = 'auto'  ) |>  gt::cols_width(    everything() ~ pct(20)  ) |>  asar::gt_split(row_every_n = 28) |> gt::grp_pull(6)``` :::{{< pagebreak >}} "
  
  # test expectation of start of figures doc
  expect_equal(
    fc_pasted,
    expected_head_figure_content
  )
  
  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
  unlink(fs::path(getwd(), "report"), recursive = T)
})