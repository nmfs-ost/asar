#' Add tagging structure to latex documents produced from quarto
#'
#' @inheritParams add_accessibility
#'
#' @return This function was made to help add in
#' latex packages and content associated with PDF
#' tagging. Quarto does not allow the user to edit anything
#' before documentclass, so this function alters the rendered .tex file.
#'
#' @export
#'
#' @examples
#' \dontrun{
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
#'   new_section = "an_additional_section",
#'   section_location = "after-introduction"
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
  # Add in . from default quarto issue
  # \hypersetup{linkcolor=}
  issue_color <- grep("\\hypersetup\\{linkcolor=\\}", tex_file)
  if (length(issue_color) > 1) {
    cli::cli_alert_warning("Failed to solve ~ ! LaTeX Error: Unknown color ''.")
  } else {
    tex_file[issue_color] <- gsub("(linkcolor=)",
                                  "\\1.",
                                  tex_file[issue_color])
  }
  # Export file
  write(tex_file, file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))

  # Add accessibility.tex to directory
  accessibility <- paste0(
    "\\DocumentMetadata{%", "\n",
    "  ", "testphase={phase-III,math,table,title},", "\n",
    "  ", "pdfversion=2.0,", "\n",
    "  ", "pdfstandard=ua-2,","\n",
    "  ", "pdfstandard=a-4f", "\n",
    "  ", "% testphase={phase-II, tabular, graphic}%", "\n",
    "  ", "% testphase={phase-II,math, tabular, graphic}% TOC Does not work", "\n",
    "  ", "% testphase={phase-III,math}% TOC works", "\n",
    "}", "\n",
    "\\tagpdfsetup{activate, tabsorder=structure}", "\n",
    "% Use the following to fix bug in November 2023 download of LaTeX", "\n",
    "% \\ExplSyntaxOn", "\n",
    "% \\cs_generate_variant:Nn__tag_prop_gput:Nnn{cnx}", "\n",
    "% \\ExplSyntaxOff", "\n",
    "%", "\n"
  )

  # Save accessibility partial
  utils::capture.output(cat(accessibility), file = file.path(dir, "accessibility.tex"), append = FALSE)
  cli::cli_alert_success("______Tagging structure added to tex file.______")
  if (compile) {
    cli::cli_alert_info("______Compiling in progress - This can take a while...______")
    # test if this can be done when skeleton is in different folder than the wd
    tinytex::lualatex(file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))
    cli::cli_alert_success("______Compiling finished______")
  }
}
