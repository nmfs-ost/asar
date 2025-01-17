#' Add Accessibility to .tex documents
#'
#' Altering latex file of report to increase accessibility of the document.
#'
#' @param x .tex file to add accessibility into
#' @param dir directory where the tex file is located that will be edited
#' @param rda_dir folder where rda files containing alternative text is located
#' @param compile TRUE/FALSE - indicate whether the document (X) should be rendered after these files are changed
#' @param rename change the name of the latex file for final compilation or saving
#'
#' @return DRAFT: This function was made to help add in 
#' latex packages and content associated with PDF 
#' tagging as well as alternative text for latex 
#' documents. Quarto does not allow the user to edit anything
#' before documentclass, so this function alters the rendered .tex file.
#' @export
#'
add_accessibility <- function(
  x = list.files(getwd())[grep("skeleton.tex", list.files(getwd()))],
  dir = getwd(),
  rda_dir = getwd(),
  compile = TRUE,
  rename = NULL
  ) {
  
  # Add tagpdf pkg to template and create accessibility.tex
  add_tagging(
    x = x,
    dir = dir,
    compile = FALSE,
    rename = rename
  )
  message("______Tagging structure added to tex file.______")
  # add alternative text to template
  add_alttext(
    x = ifelse(is.null(rename), x, glue::glue("{rename}.tex")),
    dir = dir,
    rda_dir = rda_dir,
    compile = FALSE,
    rename = rename
  )
  message("______Alternative text added to tex file.______")
 # Render the .tex file after edits
  if (compile) {
    # test if this can be done when skeleton is in different folder than the wd
    tinytex::lualatex(file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))
  }
}
