test_that("Creates expected start of nearly empty figures doc", {
  # create figures doc
  create_figures_doc(
    subdir = getwd(),
    figures_dir = getwd()
  )

  # read in figures doc
  figure_content <- readLines("09_figures.qmd")
  # extract first line
  head_figure_content <- figure_content[1]
  # remove line numbers and collapse
  fc_pasted <- paste(head_figure_content, collapse = "")

  # expected figures doc head
  expected_head_figure_content <- "# Figures {#sec-figures}"

  # test expectation of start of figures doc
  # TODO: Update test, and add more, after further development and integration
  # of create_figures_doc()
  testthat::expect_equal(
    fc_pasted,
    expected_head_figure_content
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "09_figures.qmd"))
})

test_that("Creates expected start of figures doc with figure", {
 stockplotr::plot_biomass(
    dat = stockplotr::example_data,
    make_rda = TRUE,
    module = "TIME_SERIES"
  )

  # create figures doc
  create_figures_doc(
    subdir = getwd(),
    figures_dir = getwd()
  )

  # read in figures doc
  figure_content <- readLines("09_figures.qmd")
  # extract first 6 lines
  head_figure_content <- head(figure_content, 6)
  # remove line numbers and collapse
  fc_pasted <- paste(head_figure_content, collapse = "")

  # expected figures doc head
  expected_head_figure_content <- "# Figures {#sec-figures} ```{r} #| label: 'set-rda-dir-figs'#| warnings: false #| eval: true"

  # test expectation of start of figures doc
  expect_equal(
    fc_pasted,
    expected_head_figure_content
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "09_figures.qmd"))
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})

test_that("Formerly empty figures doc renders correctly", {
  # create empty figures doc
  create_template()

  stockplotr::plot_biomass(
    dat = stockplotr::example_data,
    make_rda = TRUE,
    module = "TIME_SERIES"
  )

  # rerender figures doc, appending new figure
  create_figures_doc(
    subdir = file.path(getwd(), "report"),
    figures_dir = getwd()
  )

  # read in figures doc
  figure_content <- readLines(file.path(getwd(), "report", "09_figures.qmd"))
  # extract first 7 lines
  head_figure_content <- head(figure_content, 7)
  # remove line numbers and collapse
  fc_pasted <- paste(head_figure_content, collapse = "")

  # expected figures doc head
  expected_head_figure_content <- "# Figures {#sec-figures} ```{r} #| label: 'set-rda-dir-figs'#| warnings: false #| eval: true"

  # test expectation of start of figures doc
  expect_equal(
    fc_pasted,
    expected_head_figure_content
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
  unlink(fs::path(getwd(), "report"), recursive = T)
})

test_that("Throws warning if chunks with identical labels", {
  stockplotr::plot_biomass(
    dat = stockplotr::example_data,
    make_rda = TRUE,
    module = "TIME_SERIES"
  )

  # create figures doc
  create_figures_doc(
    subdir = getwd(),
    figures_dir = getwd()
  )

  expect_message(
    create_figures_doc(
      subdir = getwd(),
      figures_dir = getwd()
    ),
    "Figures doc contains chunks with identical labels:"
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "09_figures.qmd"))
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})

test_that("Adds new figure from figures folder.", {
  # Create one figure
  stockplotr::plot_biomass(
    dat = stockplotr::example_data,
    make_rda = TRUE,
    module = "TIME_SERIES"
  )
  
  create_template()
  
  # Make another figure
  stockplotr::plot_abundance_at_age(
    dat = stockplotr::example_data,
    make_rda = TRUE
  )
  
  # rerender figures doc, appending new figure
  create_figures_doc(
    subdir = file.path(getwd(), "report"),
    figures_dir = getwd()
  )
  
  # read in figures doc
  figure_content <- readLines(file.path(getwd(), "report", "09_figures.qmd"))
  # remove line numbers and collapse
  fc_pasted <- paste(figure_content, collapse = "")
  
  # expected figures doc head
  expected_head_figure_content <- "# Figures {#sec-figures} ```{r} #| label: 'set-rda-dir-figs'#| warnings: false #| eval: truefigures_dir <- fs::path('C:/Users/samantha.schiano.NMFS/Documents/GitHub/asar', 'figures')``` ```{r} #| label: 'fig-biomass-setup'#| warnings: false #| eval: true# load rdaload(file.path(figures_dir, 'biomass_figure.rda'))# save rda with plot-specific namebiomass_plot_rda <- rda# remove generic rda objectrm(rda)# save figure, caption, and alt text as separate objectsbiomass_plot <- biomass_plot_rda$figurebiomass_cap <- biomass_plot_rda$captionbiomass_alt_text <- biomass_plot_rda$alt_text``` ```{r} #| label: 'fig-biomass'#| echo: false #| warning: false #| fig-cap: !expr biomass_cap #| fig-alt: !expr biomass_alt_textbiomass_plot``` {{< pagebreak >}} ```{r} #| label: 'fig-abundance_at_age-setup'#| warnings: false #| eval: true# load rdaload(file.path(figures_dir, 'abundance_at_age_figure.rda'))# save rda with plot-specific nameabundance_at_age_plot_rda <- rda# remove generic rda objectrm(rda)# save figure, caption, and alt text as separate objectsabundance_at_age_plot <- abundance_at_age_plot_rda$figureabundance_at_age_cap <- abundance_at_age_plot_rda$captionabundance_at_age_alt_text <- abundance_at_age_plot_rda$alt_text``` ```{r} #| label: 'fig-abundance_at_age'#| echo: false #| warning: false #| fig-cap: !expr abundance_at_age_cap #| fig-alt: !expr abundance_at_age_alt_textabundance_at_age_plot``` {{< pagebreak >}} "
  
  # test expectation of start of figures doc
  expect_equal(
    fc_pasted,
    expected_head_figure_content
  )
  
  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
  unlink(fs::path(getwd(), "report"), recursive = T)
})
