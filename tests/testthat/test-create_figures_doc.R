test_that("Creates expected start of nearly empty figures doc", {
  # create tables doc
  create_figures_doc(
    subdir = getwd(),
    figures_dir = getwd()
  )

  # read in tables doc
  figure_content <- readLines("09_figures.qmd")
  # extract first 8 lines
  head_figure_content <- head(figure_content, 7)
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

  # convert sample dataset
  dat <- convert_output(
    file = file.path("fixtures", "ss3_models", "models", "Hake_2018",
                            "Report.sso"),
    model = "ss3"
  )

  stockplotr::plot_landings(dat,
                            make_rda = TRUE)

  # create tables doc
  create_figures_doc(
    subdir = getwd(),
    figures_dir = getwd()
  )

  # read in tables doc
  figure_content <- readLines("09_figures.qmd")
  # extract first 7 lines
  head_figure_content <- head(figure_content, 7)
  # remove line numbers and collapse
  fc_pasted <- paste(head_figure_content, collapse = "")

  # expected figures doc head
  expected_head_figure_content <- "# Figures {#sec-figures} ```{r} #| label: 'set-rda-dir-figs'#| echo: false #| warnings: false #| eval: true"

  # test expectation of start of figures doc
  testthat::expect_equal(
    fc_pasted,
    expected_head_figure_content
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "09_figures.qmd"))
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})
