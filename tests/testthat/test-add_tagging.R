test_that("add_alttext() creates .tex file", {
  path <- getwd()

  # run create template
  asar::create_template(
    office = "SEFSC",
    region = "South Atlantic",
    species = "Atlantic Bluefish",
    spp_latin = "Pomatomus saltatrix",
    year = 2025,
    author = "John Snow"
  ) |> suppressMessages() |> suppressWarnings()

  # render template
  quarto::quarto_render(file.path(path, "report", "SAR_SA_Atlantic_Bluefish_skeleton.qmd"))

  # run add_tagging and check if new one is created
  withr::with_dir(
    file.path(getwd(), "report"),
    add_tagging(
      x = "SAR_SA_Atlantic_Bluefish_skeleton.tex",
      dir = getwd(),
      compile = TRUE,
      rename = "SAB_SAR_2025_tagging"
    )
  )

  expect_snapshot_file(
    path = file.path(path, "report"),
    name = "SAB_SAR_2025_tagging.tex"
  )

  # Remove testing files
  on.exit(unlink(file.path(getwd(), "report"),
                 recursive = TRUE
  ), add = TRUE)
})
