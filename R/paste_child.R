#' Write R Chunk to Add Child Document
#'
#' @param x section .qmd file to add into the template, options for sections are in the 'skeleton' folder; only select XX_section.qmd files and not a, b, c... subfiles
#' @param label description for the child document being added (short - one/two words max)
#'
#' @return Formatting R chunk for child document to add section into the template/skeleton.
#'         Utilize the cat() function to implement into readable text.
#' @export
#'
#' @examples paste_child("test_quarto.qmd", label = "test_doc")
paste_child <- function(x,
                        label = NULL) {
  child <- c()
  for (i in 1:length(x)) {
    sec_num <- x[i]
    child_loop <- paste0(
      "\n",
      "```{r, results='asis'}", "\n",
      "#| label: ", "'",
      ifelse(is.null(label), NA, label[i]), "'", "\n",
      "#| eval: true", "\n",
      "#| echo: false", "\n",
      "#| warning: false", "\n",
      "a <- knitr::knit_child(", "'", sec_num, "'", ", quiet = TRUE)", "\n",
      "cat(a, sep = '\\n')", "\n",
      "```", "\n"
    )
    child <- paste(child, child_loop, sep = "\n {{< pagebreak >}} \n")
  }
  return(child)
}
