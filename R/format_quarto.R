format_quarto <- function(format,
                          include_affiliation){
  if(format == "pdf" | format == "html"){
    pasteo(yaml,
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
    yaml <- paste0(
      yaml, "format: \n",
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
