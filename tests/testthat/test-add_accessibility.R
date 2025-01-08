# test_that("add_accessibility() creates .tex file", {
#   path <- getwd()
#   # run convert_output for example model
#   simple_model <- file.path(test_path("fixtures", "ss3_models"), "models", "Simple")
#   asar::convert_output(
#     output_file = "Report.sso",
#     outdir = simple_model,
#     model = "SS3",
#     file_save = TRUE,
#     save_name = "spp_conout"
#   ) |> suppressMessages() |> suppressWarnings()

#   # run satf::exp_all_figs_tables()
#   satf::exp_all_figs_tables(
#     dat = utils::read.csv(file.path(path, "spp_conout.csv")),
#     end_year = 2023,
#     ref_line = "msy",
#     ref_line_sb = "msy",
#     indices_unit_label = ""
#   ) |> suppressMessages() |> suppressWarnings()

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
#   # render template
#   quarto::quarto_render(file.path(path, "report", "SAR_USWC_Simple_skeleton.qmd"))
#   # run add_tagging and check if new one is created
#   withr::with_dir(
#     file.path(getwd(), "report"),
#     testthat::expect_snapshot_file(
#       add_accessibility(
#       x = "SAR_USWC_Simple_skeleton.tex",
#       dir = getwd(),
#       rda_dir = path,
#       compile = TRUE,
#       rename = "Simple_SAR_2025_tagged"
#       ),
#       name = readLines("Simple_SAR_2025_tagged.tex")
#     )
#   )
  
#   # expect_snapshot_file(
#   #   compare_file_text(
#   #     readLines(file.path(getwd(), "report", "SAR_USWC_Simple_skeleton.tex")),
#   #     readLines(file.path(getwd(), "tests", "testthat", "snaps", "Simple_SAR_2025_tagged.tex"))
#   #   )
#   # )

#   # Remove testing files
#   on.exit(unlink(file.path(getwd(), "report"),
#       recursive = TRUE
#     ), add = TRUE)
#   on.exit(unlink(file.path(getwd(), "rda_files"),
#       recursive = TRUE
#     ), add = TRUE)
#   on.exit(unlink(file.path(getwd(), "spp_conout.csv"),
#     recursive = TRUE
#   ), add = TRUE)
#   on.exit(unlink(file.path(getwd(), "captions_alt_text.csv"),
#     recursive = TRUE
#   ), add = TRUE)
# })

# # test_that("add_accessbility() renames altered tex file.", {

# # })