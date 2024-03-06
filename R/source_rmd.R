#' Render markdown document within file
#'
#' @param file Rmd or qmd file to be rendered
#'
#' @return Use this function to render a markdown or quarto document within the
#'         template without using child calls. This will apply to general sections
#'         so structures can be more modularly generated and customized.
#' @export
#'
#' @examples
#'
source_rmd <- function(
    file,
    ...){

  tmp_file = tempfile(fileext=".R")
  on.exit(unlink(tmp_file), add = TRUE)
  knitr::purl(file, output=tmp_file)
  source(file = tmp_file, ...)

}
