#' Add Formatting Arguments for YAML Header
#'
#' @inheritParams create_template
#'
#' @return This function returns part of a quarto YAML header
#' involved in formatting the document during rendering.
#' @export
#'
#' @examples format_quarto(format = "pdf")
format_quarto <- function(format = NULL) {
  if (tolower(format) == "pdf") {
    paste0(
      "format: ", "\n",
      "  ", format, ": \n",
      "  ", "  ", "documentclass: scrartcl", "\n",
      "  ", "  ", "number-sections: true", "\n",
      "  ", "  ", "template-partials:", "\n",
      "  ", "  ", "  ", "- 'support_files/before-body.tex'", "\n",
      "  ", "  ", "  ", "- 'support_files/_titlepage.tex'", "\n",
      "  ", "  ", "include-in-header:", "\n",
      "  ", "  ", "  ", "- 'support_files/in-header.tex'", "\n",
      if (tolower(type) == "pfmc") "  ", "  ", "  ", "- 'support_files/pfmc.tex'", "\n",
      "  ", "  ", "header-includes:", "\n",
      "  ", "  ", "  ", "\\usepackage{draftwatermark}", "\n",
      "  ", "  ", "  ", "\\SetWatermarkText{DRAFT}", "\n",
      "  ", "  ", "toc: true", "\n",
      "  ", "  ", "lof: true", "\n",
      "  ", "  ", "lot: true", "\n",
      "  ", "  ", "titlepage-geometry: ", "\n",
      "  ", "  ", "  ", "- top=2in", "\n",
      "  ", "  ", "  ", "- bottom=1in", "\n",
      "  ", "  ", "  ", "- right=1in", "\n",
      "  ", "  ", "  ", "- left=1in", "\n"
    )
  } else if (tolower(format) == "html") {
    paste0(
      "format: ", "\n",
      "  ", format, ": \n",
      "  ", "  ", "theme: [cosmo, support_files/theme.scss]", "\n",
      "  ", "  ", "mainfont: 'cambria'", "\n",
      "  ", "  ", "number-sections: true", "\n",
      "  ", "  ", "toc: true", "\n",
      "  ", "  ", "toc-depth: 3", "\n",
      "  ","  ", "citations-hover: true", "\n"
    )
  } else if (tolower(format) == "docx") {
    paste0(
      "format: \n",
      "  ", format, ": \n",
      "  ", "  ", "toc: ", "true \n",
      "  ", "  ", "toc-depth: 2", "\n",
      "  ", "  ", "reference-doc: template.docx", "\n",
      "  ", "  ", "always_allow_html: true", "\n",
      "  ", "  ", "keep-tex: true", "\n"
    )
  } else {
    stop("Invalid render format.")
  }
}
