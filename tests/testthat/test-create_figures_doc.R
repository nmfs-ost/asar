test_that("Creates expected start of nearly empty figures doc", {
  # create figures doc
  create_figures_doc(
    subdir = getwd(),
    figures_dir = getwd()
  )

  # read in figures doc
  figure_content <- readLines("08_figures.qmd")
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
  file.remove(fs::path(getwd(), "08_figures.qmd"))
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
  figure_content <- readLines("08_figures.qmd")
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
  file.remove(fs::path(getwd(), "08_figures.qmd"))
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
  figure_content <- readLines(file.path(getwd(), "report", "08_figures.qmd"))
  # extract first 7 lines
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
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
  unlink(fs::path(getwd(), "report"), recursive = T)
})

# TODO: update test and find condition where chunks might have identical labels
# test_that("Throws warning if chunks with identical labels", {
#   stockplotr::plot_biomass(
#     dat = stockplotr::example_data,
#     make_rda = TRUE,
#     module = "TIME_SERIES"
#   )
#
#   # create figures doc
#   create_figures_doc(
#     subdir = getwd(),
#     figures_dir = getwd()
#   )
#
#   expect_message(
#     create_figures_doc(
#       subdir = getwd(),
#       figures_dir = getwd()
#     ),
#     "Figures doc contains chunks with identical labels:"
#   )
#
#   # erase temporary testing files
#   file.remove(fs::path(getwd(), "08_figures.qmd"))
#   file.remove(fs::path(getwd(), "captions_alt_text.csv"))
#   file.remove(fs::path(getwd(), "key_quantities.csv"))
#   unlink(fs::path(getwd(), "figures"), recursive = T)
# })

# DO NOT RERUN MANUALLY -- snapshot path will not be correct if it adjusts and test will fail
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
  figure_content <- readLines(file.path(getwd(), "report", "08_figures.qmd"))
  # Remove the first lines so test doesn't test path differences
  # Note: you CAN NOT test rendering with this approach
  figure_content <- figure_content[-c(3:11)]
  # remove line numbers and collapse
  fc_pasted <- paste(figure_content, collapse = "\n")

  # test expectation of start of figures doc
  expect_snapshot(
    cat(fc_pasted)
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
  unlink(fs::path(getwd(), "report"), recursive = T)
})

test_that("Legacy figures doc name is renamed to new order", {
  # standalone doc
  create_figures_doc(
    subdir = getwd(),
    figures_dir = getwd()
  )
  
  expect_true(file.exists("08_figures.qmd"))
  expect_false(file.exists("09_figures.qmd"))
  
  file.remove(fs::path(getwd(), "08_figures.qmd"))
  
  # legacy-named doc
  create_figures_doc()
  
  file.rename(
    from = file.path(getwd(),"08_figures.qmd"),
    to   = file.path(getwd(), "09_figures.qmd")
  )
  
  create_figures_doc()
  expect_true(file.exists("08_figures.qmd"))
  expect_false(file.exists("09_figures.qmd"))
  
  file.remove(fs::path(getwd(), "08_figures.qmd"))
  
  # figures doc within report directory
  create_template()
  
  file.rename(
    from = file.path(getwd(), "report", "08_figures.qmd"),
    to   = file.path(getwd(), "report", "09_figures.qmd")
  )
  
  create_template(rerender_skeleton = TRUE,
                  species = "my species",
                  file_dir = file.path(getwd(), "report"))
  
  expect_true(file.exists(file.path(getwd(), "report", "08_figures.qmd")))
  expect_false(file.exists(file.path(getwd(), "report", "09_figures.qmd")))
  
  unlink(fs::path(getwd(), "report"), recursive = T)
  
})

test_that("nemt figures doc name is named correctly", {
  create_template(type = "nemt")
  
  expect_true(file.exists(file.path("report", "05_figures.qmd")))
  expect_false(file.exists(file.path("report", "06_figures.qmd")))
  
  unlink(fs::path("report"), recursive = T)
})

test_that("safe figures doc name is named correctly", {
  create_template(type = "safe")
  
  expect_true(file.exists(file.path("report", "11_figures.qmd")))
  expect_false(file.exists(file.path("report", "12_figures.qmd")))
  
  unlink(fs::path("report"), recursive = T)
})
