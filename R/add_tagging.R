#' Tagging .tex files
#'
#' Add Accessibility to .tex documents
#'
#' @param x .tex file to add accessibility into
#' @param dir directory where the tex file is located that will be edited
#' @param compile TRUE/FALSE - indicate whether the document (X) should be rendered after these files are changed
#'
#' @return This function was made to help add in latex packages and content
#' associated with PDF tagging. Quarto does not allow the user to edit anything
#' before documentclass, so this function alters the rendered .tex file. Users
#' should either compile directly through the function or run
#' tinytex::lualatex(...) afterwards in the console.
#' @export
#'
add_tagging <- function(x = list.files(getwd())[grep("skeleton.tex", list.files(getwd()))],
                        dir = getwd(),
                        compile = TRUE) {
  # Read latex file
  tex_file <- readLines(file.path(dir, x))
  # Identify line where the new accessibility content should be added after
  line_after <- grep("\\PassOptionsToPackage\\{dvipsnames\\,svgnames\\,x11names\\}\\{xcolor\\}", tex_file)
  # Acessibility additions before /documentclass
  line_to_add <- "\\input{accessibility.tex}"
  # Add line into file
  tex_file <- append(line_to_add, tex_file, after = line_after)
  # Export file
  write(tex_file, file = paste(dir, x, sep = "/"))

  # Add accessibility.tex to directory
  accessibility <- paste0(
    "\\DocumentMetadata{%", "\n",
    "  %  uncompress, %only for debugging!!", "\n",
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
  utils::capture.output(cat(accessibility), file = file.path(dir, "accessibility.tex"), append = FALSE)
  # Render the .tex file after edits
  if (compile) {
    tinytex::lualatex(file.path(dir, x))
  }
}
