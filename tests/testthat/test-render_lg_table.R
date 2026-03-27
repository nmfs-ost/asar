test_that("accurate number of split tables identified", {
  library(gt)
  # make very wide table
  ft <- as.data.frame(faithful) |>
    t() |>
    as.data.frame() |>
    dplyr::select(1:50) |>
    gt::gt()

  dir.create("tables")

  # 0 essential columns
  tables_test1 <- render_lg_table(
    report_gt = ft,
    essential_columns = 0,
    tables_dir = getwd(),
    plot_name = "test_plot1.rda"
  )

  expect_equal(tables_test1, 10)

  # 2 essential columns
  tables_test2 <- render_lg_table(
    report_gt = ft,
    essential_columns = 1:2,
    tables_dir = getwd(),
    plot_name = "test_plot2.rda"
  )

  expect_equal(tables_test2, 16)


  # make very very wide table
  ft <- as.data.frame(faithful) |>
    t() |>
    as.data.frame() |>
    dplyr::select(1:70) |>
    gt::gt()

  # 0 essential columns
  tables_test3 <- render_lg_table(ft,
    essential_columns = 0,
    tables_dir = getwd(),
    plot_name = "test_plot3.rda"
  )

  expect_equal(tables_test3, 14)

  # 3 essential columns
  tables_test4 <- render_lg_table(ft,
    essential_columns = 1:3,
    tables_dir = getwd(),
    plot_name = "test_plot4.rda"
  )

  expect_equal(tables_test4, 34)


  # make slightly wide table
  ft <- as.data.frame(faithful) |>
    t() |>
    as.data.frame() |>
    dplyr::select(1:20) |>
    gt::gt()

  # 0 essential columns
  tables_test5 <- render_lg_table(ft,
    essential_columns = 0,
    tables_dir = getwd(),
    plot_name = "test_plot5.rda"
  )

  expect_equal(tables_test5, 4)

  # 3 essential columns
  tables_test6 <- render_lg_table(ft,
    essential_columns = 1:3,
    tables_dir = getwd(),
    plot_name = "test_plot6.rda"
  )

  expect_equal(tables_test6, 9)

  # erase temporary testing files
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("table_list saved as an rda", {
  # make very wide table
  ft <- as.data.frame(faithful) |>
    t() |>
    as.data.frame() |>
    dplyr::select(1:50) |>
    gt::gt()

  dir.create("tables")

  render_lg_table(ft,
    essential_columns = 0,
    tables_dir = getwd(),
    plot_name = "test_plot1.rda"
  )

  expect_true(file.exists(fs::path(
    getwd(),
    "tables",
    "test_plot1_split.rda"
  )))

  # erase temporary testing files
  unlink(fs::path(getwd(), "tables"), recursive = T)
})
