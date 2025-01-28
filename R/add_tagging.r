#' Add tagging structure to latex documents produced from quarto
#'
#' @inheritParams add_accessibility
#'
#' @return DRAFT: This function was made to help add in
#' latex packages and content associated with PDF
#' tagging. Quarto does not allow the user to edit anything
#' before documentclass, so this function alters the rendered .tex file.
#' @export
#'
#' @examples
#' \dontrun {
#'   create_template(
#'   new_template = TRUE,
#'   format = "pdf",
#'   office = "NWFSC",
#'   region = "U.S. West Coast",
#'   species = "Dover sole",
#'   spp_latin = "Microstomus pacificus",
#'   year = 2010,
#'   author = c("John Snow", "Danny Phantom", "Patrick Star"),
#'   include_affiliation = TRUE,
#'   convert_output = TRUE,
#'   resdir = "C:/Users/Documents/Example_Files",
#'   model_results = "Report.sso",
#'   model = "SS3",
#'   new_section = "an_additional_section",
#'   section_location = "after-introduction",
#'   rda_dir = getwd()
#'   )
#'
#'   path <- getwd()
#'
#'   quarto::quarto_render(file.path(path, "report", "SAR_USWC_Dover_sole_skeleton.qmd"))
#'
#'   withr::with_dir(
#'   file.path(path, "report"),
#'    add_tagging(
#'      x = "SAR_USWC_Dover_sole_skeleton.tex",
#'      dir = getwd(),
#'      rda_dir = path,
#'      compile = TRUE)
#'    )
#' }
#'
add_tagging <- function(
    x = list.files(getwd())[grep("skeleton.tex", list.files(getwd()))],
    dir = getwd(),
    compile = TRUE,
    rename = NULL
) {
    # Read latex file
  tex_file <- readLines(file.path(dir, x))

  # Identify line where the new accessibility content should be added after
  line_after <- grep("\\PassOptionsToPackage\\{dvipsnames\\,svgnames\\,x11names\\}\\{xcolor\\}", tex_file)
  # Acessibility additions before /documentclass
  line_to_add <- "\\input{accessibility.tex}"
  # Add line into file
  tex_file <- append(line_to_add, tex_file, after = line_after)
  # DO NOT UNCOMMENT FOLLOWING LINES WHEN OPERATING FXN AS A WHOLE
  # We want to keep tex_file open so we can make changes to figures later down the line
  # Export file
  write(tex_file, file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))

  # Add accessibility.tex to directory
  accessibility <- paste0(
    "\\DocumentMetadata{%", "\n",
    "  ", "%  uncompress, %only for debugging!!", "\n",
    "  ", "pdfversion=2.0,", "\n",
    "  ", "testphase={phase-II, tabular, graphic}%", "\n",
    "  ", "% testphase={phase-II,math, tabular, graphic}% TOC Does not work", "\n",
    "  ", "% testphase={phase-III,math}% TOC works", "\n",
    "}", "\n",
    "\\tagpdfsetup{activate, tabsorder=structure}", "\n",
    "% Use the following to fix bug in November 2023 download of LaTeX", "\n",
    "\\ExplSyntaxOn", "\n",
    "\\cs_generate_variant:Nn__tag_prop_gput:Nnn{cnx}", "\n",
    "\\ExplSyntaxOff", "\n",
    "%", "\n"
  )
  # Save accessibility partial
  utils::capture.output(cat(accessibility), file = file.path(dir, "accessibility.tex"), append = FALSE)
  message("______Tagging structure added to tex file.______")
  if (compile) {
    message("______Compiling in progress - This can take a while...______")
    # test if this can be done when skeleton is in different folder than the wd
    tinytex::lualatex(file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))
    message("______Compiling finished______")
  }
}
