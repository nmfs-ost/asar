# test_that("add_alttext() creates .tex file", {
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
#     species = "Big skate",
#     spp_latin = "latin spp",
#     year = 2025,
#     author = "John Snow",
#     resdir = path,
#     model_results = "spp_conout.csv",
#     model = "SS3"
#   ) |> suppressMessages() |> suppressWarnings()
#
#   # render template
#   quarto::quarto_render(file.path(path, "report", "SAR_USWC_Big_skate_skeleton.qmd"))
#
#   # run add_tagging and check if new one is created
#   withr::with_dir(
#     file.path(getwd(), "report"),
#     add_accessibility(
#       x = "SAR_USWC_Big_skate_skeleton.tex",
#       dir = getwd(),
#       figures_dir = path,
#       compile = TRUE,
#       alttext_csv_dir = path,
#       rename = "Simple_SAR_2025_a11y"
#     )
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
# })
#
