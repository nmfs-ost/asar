# AFSC Test
test_that("multiplication works", {
  title_ex <- write_title(
    office = "AFSC",
    species = "Alaskan Pollock",
    region = "Gulf of Alaska",
    year = 2024
  )
  expect_equal(title_ex, "Assessment of the Alaskan Pollock Stock in the Gulf of Alaska")
})

# NEFSC Test
test_that("multiplication works", {
  title_ex <- write_title(
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
  exp_title <- paste0("Management Track Assessment of Atlantic Bluefish (Pomatomus saltatrix) ", season, " 2024")
  expect_equal(title_ex, exp_title)
})

# NWFSC Test
test_that("multiplication works", {
  title_ex <- write_title(
    office = "NWFSC",
    species = "Sablefish",
    region = "Washington",
    year = 2024
  )
  expect_equal(title_ex, "Status of the Sablefish stock in U.S. waters off the coast of Washington in 2024")
})

# PIFSC Test
test_that("multiplication works", {
  title_ex <- write_title(
    office = "PIFSC",
    species = "Big7",
    spp_latin = "Latin name",
    year = 2024
  )
  expect_equal(title_ex, "Stock Assessment for Big7 (Latin name) along the main Hawaiian Islands in 2024")
})

# SEFSC Test
test_that("multiplication works", {
  title_ex <- write_title(
    office = "SEFSC",
    species = "Red Snapper",
    spp_latin = "Lutjanus campechanus",
    region = "South Atlantic",
    year = 2024
  )
  expect_equal(title_ex, "SEDAR XX Assessment Report for Red Snapper (Lutjanus campechanus) in the South Atlantic in 2024")
})

# SWFSC Test
test_that("multiplication works", {
  title_ex <- write_title(
    office = "SWFSC",
    species = "Sardine",
    region = "California",
    year = 2024
  )
  expect_equal(title_ex, "Status of the Sardine stock in U.S. waters off the coast of California in 2024")
})
