#' Add Formatting Arguments for YAML Header
#'
#' @param format format for rendering (pdf, html, or docx)
#'
#' @return
#' @export
#'
#' @examples
format_quarto <- function(format){
  if(format == "pdf" | format == "html"){
    paste0(
      "format: ", "\n",
      "  ", format, ": \n",
      "  ", "  ", "documentclass: scrartcl", "\n",
      "  ", "  ", "number-sections: true", "\n",
      "  ", "  ", "template-partials:", "\n",
      "  ", "  ", "  ", "- 'before-body.tex'",
      "  ", "  ", "  ", "- 'titlepage.tex'", "\n",
      "  ", "  ", "include-in-header:", "\n",
      "  ", "  ", "  ", "- 'in-header.tex'", "\n",
      "  ", "  ", "toc: true", "\n",
      "  ", "  ", "lof: true", "\n",
      "  ", "  ", "lot: true", "\n",
      "  ", "  ", "titlepage-geometry: ", "\n",
      "  ", "  ", "  ", "- top=2in", "\n",
      "  ", "  ", "  ", "- bottom=1in", "\n",
      "  ", "  ", "  ", "- right=1in", "\n",
      "  ", "  ", "  ", "- left=1in", "\n"
    )
  } else if (format == "docx") {
    paste0(
      "format: \n",
      "  ", format, ": \n",
      "  ", "  ", "toc: ", "true \n",
      "  ", "  ", "template-partials: \n",
      "  ", "  ", "  ", "- title.tex \n",
      "  ", "  ", "keep-tex: true \n"
    )
  } else {
    stop("Invalid render format.")
  }
}
