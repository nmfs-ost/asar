test_that("rerender updates SAR legacy figures/tables order in skeleton", {
  # don't run on GitHub because can't rename files in the GH testing env
  skip_on_ci()
  # SAR
  create_template() |> suppressWarnings()
  
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
  create_template(type = "safe")
  
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
  create_template(type = "nemt")
  
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