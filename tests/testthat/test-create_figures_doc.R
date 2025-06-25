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

 # expected_head_figure_content <- "## Figures {#sec-figures} ```{r} #| label: 'set-rda-dir-figs'#| echo: false #| warning: false #| eval: true "

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

<<<<<<< HEAD
# this won't pass until stockplotr is updated to output to 'figures' and 'tables'
# folders rather than "rda_dir"
# test_that("Creates expected start of figures doc with figure", {
#
#   # convert sample dataset
#   dat <- convert_output(
#     output_file = file.path("fixtures", "ss3_models", "models", "Hake_2018",
#                             "Report.sso"),
#     model = "ss3"
#   )
#
#   stockplotr::plot_landings(dat,
#                             make_rda = TRUE,
#                             rda_dir = getwd())
#
#   # create tables doc
#   create_figures_doc(
#     subdir = getwd(),
#     figures_dir = getwd()
#   )
#
#   # read in tables doc
#   figure_content <- readLines("09_figures.qmd")
#   # extract first 7 lines
#   head_figure_content <- head(figure_content, 7)
#   # remove line numbers and collapse
#   fc_pasted <- paste(head_figure_content, collapse = "")
#
#   # expected figures doc head
#   expected_head_figure_content <- "# Figures {#sec-figures} ```{r} #| label: 'set-rda-dir-figs'#| echo: false #| warnings: false #| eval: true"
#
#   # test expectation of start of figures doc
#   testthat::expect_equal(
#     fc_pasted,
#     expected_head_figure_content
#   )
#
#   # erase temporary testing files
#   file.remove(fs::path(getwd(), "09_figures.qmd"))
#   file.remove(fs::path(getwd(), "captions_alt_text.csv"))
#   unlink(fs::path(getwd(), "rda_files"), recursive = T)
# })
=======
test_that("Creates expected start of figures doc with figure", {

  # convert sample dataset
  dat <- convert_output(
    output_file = file.path("fixtures", "ss3_models", "models", "Hake_2018",
                            "Report.sso"),
    model = "ss3"
  )

  stockplotr::plot_landings(dat,
                            make_rda = TRUE,
                            rda_dir = getwd())

  # create tables doc
  create_figures_doc(
    subdir = getwd(),
    rda_dir = getwd()
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
  unlink(fs::path(getwd(), "rda_files"), recursive = T)
})
>>>>>>> e0c156f (Update create_figures/tables_doc tests; update .R files to ensure starts at header 1 (one # vs 2 ##))
