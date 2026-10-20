# test_that("add_alttext() creates .tex file", {
#   path <- getwd()
#   # run convert_output for example model
#   simple_model <- file.path(test_path("fixtures", "ss3_models"), "models", "Simple")
#   stockplotr::convert_output(
#     output_file = "Report.sso",
#     outdir = simple_model,
#     model = "SS3",
#     file_save = TRUE,
#     save_name = "spp_conout"
#   ) |> suppressMessages() |> suppressWarnings()
#
#   # run stockplotr::exp_all_figs_tables()
#   stockplotr::exp_all_figs_tables(
#     dat = utils::read.csv(file.path(path, "spp_conout.csv")),
#     end_year = 2023,
#     ref_line = "msy",
#     ref_line_sb = "msy",
#     indices_unit_label = ""
#   ) |> suppressMessages() |> suppressWarnings()
#
#   # run create template
#   asar::create_template(
#     office = "NWFSC",
#     region = "USWC",
#     species = "Simple",
#     spp_latin = "latin spp",
#     year = 2025,
#     author = "John Snow",
#     resdir = path,
#     model_results = "spp_conout.csv",
#     model = "SS3"
#   ) |> suppressMessages() |> suppressWarnings()
#
#   # render template
#   quarto::quarto_render(file.path(path, "report", "SAR_USWC_Simple_skeleton.qmd"))
#
#   # run add_tagging and check if new one is created
#     add_alttext(
#       x = "SAR_USWC_Simple_skeleton.tex",
#       dir = file.path(getwd(), "report"),
#       figures_dir = getwd(),
#       compile = TRUE,
#       alttext_csv_dir = getwd(),
#       rename = "Simple_SAR_2025_alttext"
#   )
#
#   # Extract file to compare to snapshot
#   # compare_file <- readLines(file.path(path, "report", "Simple_SAR_2025_alttext.tex"))
#
#   # TODO: make check to be expect equal by reading the latex as string
#   # and make an expectation to specific line
#   # Make wrapper for expect_snapshot_file
#   # expect_snapshot_tex <- function(tex_file, tex_dir, rename, path) {
#   #   # Other packages might affect results
#   #   skip_if_not_installed("tinytex")
#   #   # Or maybe the output is different on some operation systems
#   #   # skip_on_os("windows")
#   #   # You'll need to carefully think about and experiment with these skips
#   #
#   #   # tex_file <- glue::glue("{name}.txt")
#   #   announce_snapshot_file(path = file.path(path, "report"),
#   #                          name = "Simple_SAR_2025_alttext.tex")
#   #
#   #   path <- withr::with_dir(
#   #     file.path(path, "report"),
#   #     add_alttext(
#   #       x = tex_file,
#   #       dir = tex_dir,
#   #       figures_dir = path,
#   #       compile = TRUE,
#   #       rename = rename
#   #     )
#   #   )
#   #   testthat::expect_snapshot_file(
#   #     path = path,
#   #     name = tex_file)
#   # }
#   #
#   # expect_snapshot_tex(
#   #   tex_file = "SAR_USWC_Simple_skeleton.tex",
#   #   tex_dir = getwd(),
#   #   rename = "Simple_SAR_2025_tagged",
#   #   path = path
#   # )
#
#   expect_snapshot_file(
#     path = file.path(path, "report"),
#     name = "Simple_SAR_2025_alttext.tex"
#   )
#
#   # Remove testing files
#   on.exit(unlink(file.path(getwd(), "report"),
#       recursive = TRUE
#     ), add = TRUE)
#   on.exit(unlink(file.path(getwd(), "figures"),
#       recursive = TRUE
#     ), add = TRUE)
#   on.exit(unlink(file.path(getwd(), "spp_conout.csv"),
#     recursive = TRUE
#   ), add = TRUE)
#   on.exit(unlink(file.path(getwd(), "captions_alt_text.csv"),
#     recursive = TRUE
#   ), add = TRUE)
#   on.exit(unlink(file.path(getwd(), "key_quantities.csv"),
#     recursive = TRUE
#   ), add = TRUE)
# })

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
