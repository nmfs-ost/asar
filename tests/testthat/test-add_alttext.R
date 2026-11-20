test_that("add_alttext() adds tagged alt text and leaves existing text unchanged", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  tex <- c(
    "\\includegraphics[width=\\linewidth,keepaspectratio]{biomass.pdf}",
    "\\caption{\\label{fig-biomass} Biomass}",
    "\\includegraphics[width=\\linewidth,keepaspectratio,alt={'Existing text'}]{survey.pdf}",
    "\\caption{\\label{fig-survey} Survey}"
  )
  writeLines(tex, file.path(dir, "source.tex"))
  utils::write.csv(
    data.frame(
      label = c("biomass", "survey", "missing"),
      alt_text = c("Biomass plot", "Updated text", "No figure")
    ),
    file.path(dir, "alttext.csv"),
    row.names = FALSE
  )

  add_alttext(
    x = "source.tex",
    dir = dir,
    alttext_csv = file.path(dir, "alttext.csv"),
    compile = FALSE,
    rename = "renamed"
  )

  output <- readLines(file.path(dir, "renamed.tex"))
  expect_true(any(grepl("keepaspectratio,alt={'Biomass plot'}", output, fixed = TRUE)))
  expect_true(any(grepl("alt={'Existing text'}", output, fixed = TRUE)))
  expect_false(any(grepl("Updated text", output, fixed = TRUE)))
  expect_equal(readLines(file.path(dir, "source.tex")), tex)
})

test_that("add_alttext() supports the tooltip path", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  writeLines(
    c(
      "\\pandocbounded{\\includegraphics[width=\\linewidth,keepaspectratio]{biomass.pdf}}",
      "\\caption{\\label{fig-biomass} Biomass}"
    ),
    file.path(dir, "source.tex")
  )
  utils::write.csv(
    data.frame(label = "biomass", alt_text = "Biomass plot"),
    file.path(dir, "alttext.csv"),
    row.names = FALSE
  )

  add_alttext(
    x = "source.tex",
    dir = dir,
    alttext_csv = file.path(dir, "alttext.csv"),
    compile = FALSE,
    tagged = FALSE
  )

  output <- readLines(file.path(dir, "source.tex"))
  expect_true(any(grepl("Biomass plot", output, fixed = TRUE)))
})

test_that("add_alttext() reports missing input files", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  expect_error(add_alttext(x = "missing.tex", dir = dir, compile = FALSE))

  writeLines("text", file.path(dir, "source.tex"))
  expect_error(add_alttext(
    x = "source.tex",
    dir = dir,
    alttext_csv = file.path(dir, "missing.csv"),
    compile = FALSE
  ))
})
#
# # test_that("add_alttext() renames altered tex file.", {
#
# # })
