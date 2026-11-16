test_that("Table length calculated correctly for regular table", {
  # make sample dataset
  library(gt)
  test_table <- data.frame(
    year = rep(seq(1987, 2000, 1), times = 3),
    estimate = rep(rnorm(42, mean = 1000, sd = 100)),
    label = rep(c("spawning_biomass", "catch", "abundance"), each = 14)
  ) |>
    tidyr::pivot_wider(id_cols = year, names_from = label, values_from = estimate) |>
    gt()

  rda <- list(
    "table" = test_table,
    "caption" = "test_caption"
  )

  tbl_path <- fs::path(getwd(), "tables")
  dir.create(tbl_path)
  save(rda, file = fs::path(getwd(), "tables", "indices.abundance_table.rda"))

  # regular length
  tbl_width <- ID_tbl_length_class(
    plot_name = "indices.abundance",
    tables_dir = getwd()
  )

  expected_output <- "regular"
  expect_equal(tbl_width, expected_output)

  # erase temporary testing files
  unlink(tbl_path, recursive = T)
})

test_that("Table lengths calculated correctly for a long table", {
  # make sample dataset
  library(gt)
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
      "Sargasso_Sea",
      "Buzzards_Bay_MA",
      "Stellwagen_Bank",
      "Vineyard_Sound",
      "Great_South_Channel",
      "Casco_Bay_Maine",
      "Grand_Banks",
      "Scotian_Shelf",
      "Delaware_Bay",
      "Pamlico_Sound_NC",
      "Charleston_Bump_SC",
      "Gray_Reef_GA",
      "St_Johns_River_FL",
      "Mobile_Bay_AL",
      "Flower_Garden_Banks",
      "Channel_Islands_CA",
      "Columbia_River_Estuary",
      "Gulf_of_the_Farallones",
      "Aleutian_Islands",
      "Chukchi_Sea",
      "Beaufort_Sea"
    )
  ) |>
    gt()

  rda <- list(
    "table" = test_table,
    "caption" = "test_caption"
  )

  tbl_path <- fs::path(getwd(), "tables")
  dir.create(tbl_path)
  save(rda, file = fs::path(getwd(), "tables", "bnc_table.rda"))

  tbl_width2 <- ID_tbl_length_class(
    plot_name = "bnc",
    tables_dir = getwd()
  )

  expected_output <- "long"
  expect_equal(tbl_width2, expected_output)

  # erase temporary testing files
  unlink(tbl_path, recursive = T)
})
