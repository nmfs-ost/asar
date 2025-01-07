test_that("Creates expected start of figures doc", {
  # create tables doc
  asar::create_figures_doc(
    subdir = getwd(),
    rda_dir = getwd()
  )

  # read in tables doc
  figure_content <- readLines("09_figures.qmd")
  # extract first 8 lines
  head_figure_content <- head(figure_content, 7)
  # remove line numbers and collapse
  fc_pasted <- paste(head_figure_content, collapse = "")

  # expected figures doc head
  expected_head_figure_content <- "## Figures  ```{r} #| label: 'set-rda-dir-figs'#| echo: false #| warning: false #| eval: true "

  # test expectation of start of figures doc
  testthat::expect_equal(
    fc_pasted,
    expected_head_figure_content
  )
})
