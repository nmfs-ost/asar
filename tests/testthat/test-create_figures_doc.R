test_that("Creates expected start of figures doc", {
  # create tables doc
  create_figures_doc(
    subdir = getwd(),
    figures_tables_dir = getwd()
  )

  # read in tables doc
  figure_content <- readLines("09_figures.qmd")
  # extract first 8 lines
  head_figure_content <- head(figure_content, 7)
  # remove line numbers and collapse
  fc_pasted <- paste(head_figure_content, collapse = "")

  # expected figures doc head
  expected_head_figure_content <- "## Figures {#sec-figures} ```{r} #| label: 'set-rda-dir-figs'#| echo: false #| warning: false #| eval: true "

  # test expectation of start of figures doc
  # TODO: Update test, and add more, after further development and integration
  # of create_figures_doc()
  # testthat::expect_equal(
  #   fc_pasted,
  #   expected_head_figure_content
  # )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "09_figures.qmd"))
})
