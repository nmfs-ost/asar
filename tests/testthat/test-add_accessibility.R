test_that("add_accessibility() runs without error", {
  
  stockplotr::plot_biomass(dat = stockplotr::example_data,
                           interactive = FALSE,
                           make_rda = TRUE)
  create_template(
    office = "NWFSC",
    region = "USWC",
    species = "Big skate",
    year = 2025
  ) |> suppressMessages() |> suppressWarnings()

  # render template
  quarto::quarto_render(file.path(getwd(), "report", "SAR_U_Big_skate_skeleton.qmd"))

    expect_no_error(
      add_accessibility(
        x = "SAR_U_Big_skate_skeleton.tex",
        dir = file.path(getwd(), "report"),
        rename = "Simple_SAR_2025_a11y"
      )
    )
    
    expect_true(
      file.exists(file.path(getwd(), "report", "Simple_SAR_2025_a11y.pdf"))
    )

  # Remove testing files
  on.exit(unlink(file.path(getwd(), "report"),
      recursive = TRUE
    ), add = TRUE)
  on.exit(unlink(file.path(getwd(), "figures"),
      recursive = TRUE
    ), add = TRUE)
  on.exit(unlink(file.path(getwd(), "captions_alt_text.csv"),
    recursive = TRUE
  ), add = TRUE)
  on.exit(unlink(file.path(getwd(), "key_quantities.csv"),
    recursive = TRUE
  ), add = TRUE)
})

