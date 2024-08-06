#' Add Formatting Arguments for YAML Header
#'
#' @param format format for rendering (pdf, html, or docx)
#'
#' @return This function returns part of a quarto YAML header that helps with
#' formatting the document during rendering.
#' @export
#'
#' @examples format_quarto(format = "pdf")
format_quarto <- function(format = NULL){
  if(format == "pdf" | format == "html"){
    paste0(
      "format: ", "\n",
      "  ", format, ": \n",
      "  ", "  ", "documentclass: scrartcl", "\n",
      "  ", "  ", "number-sections: true", "\n",
      "  ", "  ", "template-partials:", "\n",
      "  ", "  ", "  ", "- 'support_files/before-body.tex'", "\n",
      "  ", "  ", "  ", "- 'support_files/titlepage.tex'", "\n",
      "  ", "  ", "include-in-header:", "\n",
      "  ", "  ", "  ", "- 'support_files/in-header.tex'", "\n",
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
