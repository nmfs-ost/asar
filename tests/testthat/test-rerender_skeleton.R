test_that("rerender updates SAR legacy figures/tables order in skeleton", {
  # don't run on GitHub because can't rename files in the GH testing env
  skip_on_ci()
  # SAR
  create_template(bib_file = FALSE) |> suppressWarnings()
  
  report_dir <- fs::path(getwd(), "report")
  skeleton_path <- fs::path(report_dir, "sar_species_skeleton.qmd")
  skeleton <- readLines(skeleton_path)
  figures_idx <- grep("08_figures.qmd", skeleton, fixed = TRUE)
  tables_idx <- grep("09_tables.qmd", skeleton, fixed = TRUE)
  
  skeleton[figures_idx] <- stringr::str_replace(skeleton[figures_idx], "08_figures.qmd", "08_tables.qmd")
  skeleton[tables_idx] <- stringr::str_replace(skeleton[tables_idx], "09_tables.qmd", "09_figures.qmd")
  writeLines(skeleton, skeleton_path)
  
  file.rename(
    from = fs::path(report_dir, "08_figures.qmd"),
    to = fs::path(report_dir, "09_figures.qmd")
  )
  file.rename(
    from = fs::path(report_dir, "09_tables.qmd"),
    to = fs::path(report_dir, "08_tables.qmd")
  )
  
  rerender_skeleton(
    file_dir = "report"
  ) |> suppressWarnings()
  
  updated_skeleton <- readLines(skeleton_path)
  updated_figures_idx <- grep("08_figures.qmd", updated_skeleton, fixed = TRUE)
  updated_tables_idx <- grep("09_tables.qmd", updated_skeleton, fixed = TRUE)
  
  expect_true(file.exists(fs::path(report_dir, "08_figures.qmd")))
  expect_true(file.exists(fs::path(report_dir, "09_tables.qmd")))
  expect_false(file.exists(fs::path(report_dir, "09_figures.qmd")))
  expect_false(file.exists(fs::path(report_dir, "08_tables.qmd")))
  expect_lt(updated_figures_idx, updated_tables_idx)
  
  unlink(report_dir, recursive = TRUE)
})

test_that("rerender updates SAFE legacy figures/tables order in skeleton", {
  # don't run on GitHub because can't rename files in the GH testing env
  skip_on_ci()
  # SAFE
  create_template(type = "safe", bib_file = FALSE)
  
  report_dir <- fs::path(getwd(), "report")
  skeleton_path <- fs::path(report_dir, "safe_species_skeleton.qmd")
  skeleton <- readLines(skeleton_path)
  figures_idx <- grep("11_figures.qmd", skeleton, fixed = TRUE)
  tables_idx <- grep("12_tables.qmd", skeleton, fixed = TRUE)
  
  skeleton[figures_idx] <- stringr::str_replace(skeleton[figures_idx], "11_figures.qmd", "11_tables.qmd")
  skeleton[tables_idx] <- stringr::str_replace(skeleton[tables_idx], "12_tables.qmd", "12_figures.qmd")
  writeLines(skeleton, skeleton_path)
  
  file.rename(
    from = fs::path(report_dir, "11_figures.qmd"),
    to = fs::path(report_dir, "12_figures.qmd")
  )
  file.rename(
    from = fs::path(report_dir, "12_tables.qmd"),
    to = fs::path(report_dir, "11_tables.qmd")
  )
  
  rerender_skeleton(
    type = "safe",
    file_dir = "report"
  ) |> suppressWarnings()
  
  updated_skeleton <- readLines(skeleton_path)
  updated_figures_idx <- grep("11_figures.qmd", updated_skeleton, fixed = TRUE)
  updated_tables_idx <- grep("12_tables.qmd", updated_skeleton, fixed = TRUE)
  
  expect_true(file.exists(fs::path(report_dir, "11_figures.qmd")))
  expect_true(file.exists(fs::path(report_dir, "12_tables.qmd")))
  expect_false(file.exists(fs::path(report_dir, "12_figures.qmd")))
  expect_false(file.exists(fs::path(report_dir, "11_tables.qmd")))
  expect_lt(updated_figures_idx, updated_tables_idx)
  
  unlink(report_dir, recursive = TRUE)
})

test_that("rerender updates NEMT legacy figures/tables order in skeleton", {
  # don't run on GitHub because can't rename files in the GH testing env
  skip_on_ci()
  # NEMT
  create_template(type = "nemt", bib_file = FALSE)
  
  report_dir <- fs::path(getwd(), "report")
  skeleton_path <- fs::path(report_dir, "nemt_species_skeleton.qmd")
  skeleton <- readLines(skeleton_path)
  figures_idx <- grep("05_figures.qmd", skeleton, fixed = TRUE)
  tables_idx <- grep("06_tables.qmd", skeleton, fixed = TRUE)
  
  skeleton[figures_idx] <- stringr::str_replace(skeleton[figures_idx], "05_figures.qmd", "05_tables.qmd")
  skeleton[tables_idx] <- stringr::str_replace(skeleton[tables_idx], "06_tables.qmd", "06_figures.qmd")
  writeLines(skeleton, skeleton_path)
  
  file.rename(
    from = fs::path(report_dir, "05_figures.qmd"),
    to = fs::path(report_dir, "06_figures.qmd")
  )
  file.rename(
    from = fs::path(report_dir, "06_tables.qmd"),
    to = fs::path(report_dir, "05_tables.qmd")
  )
  
  rerender_skeleton(
    type = "nemt",
    file_dir = "report"
  ) |> suppressWarnings()
  
  updated_skeleton <- readLines(skeleton_path)
  updated_figures_idx <- grep("05_figures.qmd", updated_skeleton, fixed = TRUE)
  updated_tables_idx <- grep("06_tables.qmd", updated_skeleton, fixed = TRUE)
  
  expect_true(file.exists(fs::path(report_dir, "05_figures.qmd")))
  expect_true(file.exists(fs::path(report_dir, "06_tables.qmd")))
  expect_false(file.exists(fs::path(report_dir, "06_figures.qmd")))
  expect_false(file.exists(fs::path(report_dir, "05_tables.qmd")))
  expect_lt(updated_figures_idx, updated_tables_idx)
  
  unlink(report_dir, recursive = TRUE)
})

test_that("species is updated in skeleton.",{
  create_template(bib_file = FALSE)
  
  report_dir <- fs::path(getwd(), "report")
  file_names <- list.files(report_dir, full.names = FALSE)
  skeleton_name <- file_names[grepl("species_skeleton.qmd", file_names)]
  skeleton <- readLines(fs::path(report_dir, skeleton_name))
  init_species_params <- skeleton[grep("species:", skeleton, fixed = TRUE)]
  
  # rerender for species
  rerender_skeleton(file_dir = report_dir, species = "Red snapper")
  
  re_file_names <- list.files(report_dir, full.names = FALSE)
  rerender_skeleton_name <- re_file_names[grepl("_skeleton.qmd", re_file_names)]
  rerender_skeleton <- readLines(fs::path(report_dir, rerender_skeleton_name))
  rerender_species_params <- rerender_skeleton[grep("species:", rerender_skeleton, fixed = TRUE)]
  
  # species is updated in params
  expect_equal("   species: 'Red snapper' ", rerender_species_params)
  # species is updated in skeleton
  expect_no_match(skeleton_name, rerender_skeleton_name)
  # species changed in params
  expect_no_match(init_species_params, rerender_species_params)
  
  unlink(report_dir, recursive = TRUE)
})

test_that("office is updated in skeleton", {
  create_template(bib_file = FALSE)
  
  report_dir <- fs::path(getwd(), "report")
  file_names <- list.files(report_dir, full.names = FALSE)
  skeleton_name <- file_names[grepl("_skeleton.qmd", file_names)]
  skeleton <- readLines(fs::path(report_dir, skeleton_name))
  init_office_params <- skeleton[grep("office:", skeleton, fixed = TRUE)]
  
  # rerender for office
  rerender_skeleton(file_dir = report_dir, office = "NEFSC")
  
  re_file_names <- list.files(report_dir, full.names = FALSE)
  rerender_skeleton_name <- re_file_names[grepl("_skeleton.qmd", re_file_names)]
  rerender_skeleton <- readLines(fs::path(report_dir, rerender_skeleton_name))
  rerender_office_params <- rerender_skeleton[grep("office:", rerender_skeleton, fixed = TRUE)]
  
  # office is updated in params
  expect_equal("   office: 'gls{nefsc}' ", rerender_office_params)
  # office changed in params
  # cannot test bc negative interaction with {}
  # expect_no_match(init_office_params, rerender_office_params)
  
  unlink(report_dir, recursive = TRUE)
})

