#' Add Accessibility to .tex documents
#'
#' Altering latex file of report to increase accessibility of the document.
#'
#' @param x .tex file containing report. Typically produced after initially
#' rendering the skeleton made from create_template.
#' @param dir directory where the tex file is located that will be edited
#' @param alttext_csv Directory for the csv file containing alternative
#' text and captions generated when running stockplotr::exp_all_figs_tables
#' @param compile Indicate whether the document (X) should be
#' rendered after these files are changed. Default TRUE.
#' @param rename Indicate a name for the new tex file produced from this
#' function. There is no need to include ".tex" in the name. Defaults to current
#' name and overwrites the current tex file.
#' @param tagged Indicate if the input tex file from dir has the latex package 
#' \tagpdf used so that tagging is present.
#'
#' @return This function runs all functions from `asar` associated with
#' accessibility and renders the final document. The document is tagged and
#' includes alternative text from the captions_alt_text.csv produced from
#' `stockplotr` package also available on GitHub.
#'
#' @examples
#' \dontrun{
#' create_template(
#'   new_template = TRUE,
#'   format = "pdf",
#'   office = "NWFSC",
#'   region = "U.S. West Coast",
#'   species = "Dover sole",
#'   spp_latin = "Microstomus pacificus",
#'   year = 2010,
#'   author = c("John Snow" = "AFSC", "Danny Phantom" = "NWFSC", "Patrick Star" = "SEFSC"),
#'   model_results = output,
#'   model = "SS3",
#'   new_section = "an_additional_section",
#'   section_location = "after-introduction"
#' )
#'
#' path <- getwd()
#'
#' quarto::quarto_render(file.path(path, "report", "SAR_USWC_Dover_sole_skeleton.qmd"))
#'
#' withr::with_dir(
#'   file.path(path, "report"),
#'   add_accessibility(
#'     x = "SAR_USWC_Dover_sole_skeleton.tex",
#'     dir = getwd(),
#'     figures_dir = path,
#'     compile = TRUE
#'   )
#' )
#' }
#'
add_accessibility <- function(
    x = list.files(getwd())[grep("skeleton.tex", list.files(getwd()))],
    dir = getwd(),
    alttext_csv = file.path(getwd(), "captions_alt_text.csv"),
    compile = TRUE,
    rename = NULL) {
  # Add tagpdf pkg to template and create accessibility.tex
  add_tagging(
    x = x,
    dir = dir,
    compile = FALSE,
    rename = rename
  )
  # add alternative text to template
  add_alttext(
    x = ifelse(is.null(rename), x, glue::glue("{rename}.tex")),
    dir = dir,
    compile = compile,
    rename = NULL,
    alttext_csv = alttext_csv
  )
}
