test_that("Table widths calculated correctly for wide table", {

  # make sample dataset
  library(flextable)
  test_table <- data.frame(year = rep(seq(1987, 2024, 1), times = 3),
                     estimate = rep(rnorm(38, mean = 1000, sd = 100), times = 6),
                     label = rep(c("spawning_biomass", "catch", "abundance", "new_col1", "new_col2", "new_col3"), each = 38)
  ) |>
    tidyr::pivot_wider(id_cols = year, names_from = label, values_from = estimate) |>
    flextable()
  
  rda <- list(
    "table" = test_table,
    "cap" = "test_caption"
  )
  
  tbl_path <- fs::path(getwd(), "tables")
  dir.create(tbl_path)
  save(rda, file = fs::path(getwd(), "tables", "indices.abundance_table.rda"))

  # wide width
  tbl_width <- ID_tbl_width_class(
    plot_name = "indices.abundance",
    tables_dir = getwd(),
    portrait_pg_width = 5
  )

  expected_output <- "wide"
  expect_equal(tbl_width, expected_output)
  
  # erase temporary testing files
  unlink(tbl_path, recursive = T)
})

test_that("Table widths calculated correctly for extra-wide table", {

  # make sample dataset
  library(flextable)
  test_table <- data.frame(
    species = sample(c("Tuna", "Cod", "Trout", "Salmon"), 20, replace = TRUE),
    location = c(
      "Providence_Rhode_Island",
      "Narragansett_Bay",
      "Point_Judith_Pond",
      "Block_Island_Sound",
      "Cape_Cod_Bay",
      "Georges_Bank",
      "Gulf_of_Maine",
      "Montauk_New_York",
      "Hudson_Canyon",
      "Long_Island_Sound",
      "Chesapeake_Bay",
      "Outer_Banks_NC",
      "Florida_Keys",
      "Gulf_of_Mexico",
      "Monterey_Bay_CA",
      "Puget_Sound_WA",
      "Kodiak_Alaska",
      "Bering_Sea",
      "Hawaiian_Islands",
      "Sargasso_Sea"
    ),
    is_tagged = sample(c(TRUE, FALSE), 20, replace = TRUE)  )|>
    tidyr::pivot_wider(id_cols = species, names_from = location, values_from = is_tagged) |>
    flextable()

  rda <- list(
    "table" = test_table,
    "cap" = "test_caption"
  )

  tbl_path <- fs::path(getwd(), "tables")
  dir.create(tbl_path)
  save(rda, file = fs::path(getwd(), "tables", "bnc_table.rda"))

  tbl_width2 <- ID_tbl_width_class(
    plot_name = "bnc",
    tables_dir = getwd(),
    portrait_pg_width = 5
  )

  expected_output <- "extra-wide"
  expect_equal(tbl_width2, expected_output)

  # erase temporary testing files
  unlink(tbl_path, recursive = T)
})
