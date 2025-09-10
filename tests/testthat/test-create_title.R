test_that("AFSC report given accurate title", {
  title_ex <- create_title(
    office = "AFSC",
    species = "Alaskan Pollock",
    region = "Gulf of Alaska",
    year = 2024
  )
  expect_equal(title_ex, "Assessment of the Alaskan Pollock Stock in the Gulf of Alaska")

  title_ex <- create_title(
    office = "AFSC",
    species = "Alaskan Pollock",
    region = "Gulf of Alaska",
    year = 2024,
    complex = TRUE
  )
  expect_equal(title_ex, "Assessment of the Alaskan Pollock Stock Complex in the Gulf of Alaska")
})

test_that("NEFSC report given accurate title", {
  title_ex <- create_title(
    office = "NEFSC",
    species = "Atlantic Bluefish",
    region = "Georges Bank",
    year = 2024,
    type = "MT",
    spp_latin = "Pomatomus saltatrix"
  )
  if (as.numeric(format(Sys.Date(), "%m")) %in% c(3, 4, 5)) {
    season <- "Spring"
  } else if (as.numeric(format(Sys.Date(), "%m")) %in% c(6, 7, 8)) {
    season <- "Summer"
  } else if (as.numeric(format(Sys.Date(), "%m")) %in% c(9, 10, 11)) {
    season <- "Fall"
  } else if (as.numeric(format(Sys.Date(), "%m")) %in% c(12, 1, 2)) {
    season <- "Winter"
  }
  exp_title <- paste0("Management Track Assessment of Atlantic Bluefish (\\textit{Pomatomus saltatrix}) ", season, " 2024")
  expect_equal(title_ex, exp_title)
})

test_that("NWFSC report given accurate title", {
  title_ex <- create_title(
    office = "NWFSC",
    species = "Sablefish",
    region = "Washington",
    year = 2024
  )
  expect_equal(title_ex, "Status of Sablefish stock in U.S. waters off the coast of Washington in 2024")

  title_ex <- create_title(
    office = "NWFSC",
    species = "Sablefish",
    year = 2024
  )
  expect_equal(title_ex, "Status of Sablefish stock along the U.S. West Coast in 2024")

  title_ex <- create_title(
    office = "NWFSC",
    species = "Sablefish",
    year = 2024,
    region = "north coast"
  )
  expect_equal(title_ex, "Status of Sablefish stock off the north coast in 2024")
})

test_that("PIFSC report given accurate title", {
  title_ex <- create_title(
    office = "PIFSC",
    species = "Big7",
    spp_latin = "Latin name",
    year = 2024
  )
  expect_equal(title_ex, "Stock Assessment for Big7 (\\textit{Latin name}) along the main Hawaiian Islands in 2024")

  title_ex <- create_title(
    office = "PIFSC",
    species = "Big7",
    spp_latin = "Latin name",
    year = 2024,
    region = "Western atoll"
  )
  expect_equal(title_ex, "Stock Assessment for Big7 (\\textit{Latin name}) on Western atoll in 2024")
})

test_that("SEFSC report given accurate title", {
  title_ex <- create_title(
    office = "SEFSC",
    species = "Red Snapper",
    spp_latin = "Lutjanus campechanus",
    region = "South Atlantic",
    year = 2024
  )
  expect_equal(title_ex, "SEDAR XX Assessment Report for Red Snapper (\\textit{Lutjanus campechanus}) in the South Atlantic in 2024")

  title_ex <- create_title(
    office = "SEFSC",
    species = "Red Snapper",
    spp_latin = "Lutjanus campechanus",
    year = 2024
  )
  expect_equal(title_ex, "SEDAR XX Assessment Report for Red Snapper (\\textit{Lutjanus campechanus}) in 2024")
})

test_that("SWFSC report given accurate title", {
  title_ex <- create_title(
    office = "SWFSC",
    species = "Sardine",
    region = "California",
    year = 2024
  )
  expect_equal(title_ex, "Status of the Sardine stock in U.S. waters off the coast of California in 2024")

  title_ex <- create_title(
    office = "SWFSC",
    species = "Sardine",
    year = 2024
  )
  expect_equal(title_ex, "Status of the Sardine stock along the U.S. West Coast in 2024")
})

test_that("general report given accurate title", {
  title_ex <- create_title(
    species = "species",
    year = 2024
  )
  expect_equal(title_ex, "Stock Assessment Report Template")

  title_ex <- create_title(
    species = "bluefin tuna",
    region = "east coast",
    year = 2024
  )
  expect_equal(title_ex, "Stock Assessment Report for the bluefin tuna Stock in 2024")
})
