# test_that("add_alttext() creates .tex file", {
#   path <- getwd()
#
#   # run create template
#   asar::create_template(
#     office = "SEFSC",
#     region = "South Atlantic",
#     species = "Atlantic Bluefish",
#     spp_latin = "Pomatomus saltatrix",
#     year = 2025,
#     author = "John Snow"
#   ) |> suppressMessages() |> suppressWarnings()
#
#   # render template
#   quarto::quarto_render(file.path(path, "report", "SAR_SA_Atlantic_Bluefish_skeleton.qmd"))
#
#   # run add_tagging and check if new one is created
#     add_tagging(
#       x = "SAR_SA_Atlantic_Bluefish_skeleton.tex",
#       dir = file.path(getwd(), "report"),
#       compile = TRUE,
#       rename = "SAB_SAR_2025_tagging"
#   )
#
#   expect_snapshot_file(
#     path = file.path(path, "report"),
#     name = "SAB_SAR_2025_tagging.tex"
#   )
#
#   # Remove testing files
#   on.exit(unlink(file.path(getwd(), "report"),
#                  recursive = TRUE
#   ), add = TRUE)
# })

test_that("id_num_headers() tags tables and leaves documents without tables unchanged", {
  no_tables <- c("\\section{Introduction}", "Text")
  expect_identical(id_num_headers(no_tables), no_tables)

  tables <- c(
    "\\begin{table}",
    "Table contents",
    "\\end{table}",
    "\\begin{longtable}",
    "Long table contents",
    "\\end{longtable}"
  )
  output <- id_num_headers(tables)

  expect_equal(sum(grepl("table/header-rows", output, fixed = TRUE)), 2)
  expect_lt(
    match("\\tagpdfsetup{table/header-rows={1}}", output),
    match("\\begin{table}", output)
  )
  expect_lt(
    match("\\tagpdfsetup{table/header-rows={1}}", output),
    match("\\begin{longtable}", output)
  )
})

test_that("add_tagging() writes tagged TeX and accessibility metadata without compiling", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  writeLines(
    c(
      "\\PassOptionsToPackage{dvipsnames,svgnames,x11names}{xcolor}",
      "\\hypersetup{linkcolor=}",
      "\\IfFileExists{microtype.sty}{% use microtype if available",
      "microtype line 2",
      "microtype line 3",
      "microtype line 4",
      "\\begin{table}",
      "Table contents",
      "\\end{table}"
    ),
    file.path(dir, "source.tex")
  )

  add_tagging(
    x = "source.tex",
    dir = dir,
    compile = FALSE,
    rename = "tagged"
  )

  output <- readLines(file.path(dir, "tagged.tex"))
  expect_true(any(grepl("\\input{accessibility.tex}", output, fixed = TRUE)))
  expect_true(any(grepl("\\hypersetup{linkcolor=.}", output, fixed = TRUE)))
  expect_true(any(grepl("table/header-rows={1}", output, fixed = TRUE)))
  expect_true(grepl("^% ", output[grep("microtype.sty", output)]))
  expect_true(file.exists(file.path(dir, "accessibility.tex")))
  expect_true(any(grepl("tagging = on", readLines(file.path(dir, "accessibility.tex")), fixed = TRUE)))
})
