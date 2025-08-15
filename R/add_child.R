#' Write R Chunk to Add Child Document
#'
#' @param x An additional section to add into the template. Options for
#' additional sections are in the 'skeleton' folder. Appropriate files are .qmd
#' files and are formatted as such: XX_section.qmd (i.e., not a, b, c... subfiles).
#' @param label Description of the child document being added. It should be short-
#' one or two words, maximum.
#'
#' @return Formatting R chunk for child document to add section into the template/skeleton.
#'         Utilize the cat() function to implement into readable text.
#' @export
#'
#' @examples add_child("test_quarto.qmd", label = "test_doc")
add_child <- function(x,
                      label = NULL) {
  if (is.null(x)) cli::cli_abort("Child document selection (`x`) is NULL.")

  child <- c()
  for (i in seq_along(x)) {
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
  child
}
