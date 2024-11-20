#' Write R chunk to template
#'
#' @param x Content to be written within the R chunk. Wrap in quotation marks ("").
#' @param label The name of the chunk in the 'label:' section of the R code chunk.
#'  This should be in snakecase (i.e., in which words are written in lowercase and
#'   connected by underscores).
#' @param echo TRUE/FALSE; Option to repeat code in the document. Default is false.
#' @param warnings TRUE/FALSE; Option to report warnings in the console during render.
#'  Default is false.
#' @param eval TRUE/FALSE; Option to evaluate the chunk. Default is true.
#' @param add_option TRUE/FALSE; Option to add additional chunk options. Default is false.
#' @param chunk_op List of chunk options to add. For example: c("output: true", "error: false)
#' @param rmark_op List of chunk options to add after indicating the language of the chunk as used in Rmarkdown.
#'
#' @return Write an additional R chunk into the template using this function.
#'         The code can be written as usual, just remember to put it entirely
#'         in quotes for the function to render it properly
#' @export
#'
#' @examples add_chunk("plot(cars$speed, cars$distance)")
add_chunk <- function(
    x,
    echo = "false",
    warnings = "false",
    eval = "true",
    label = NULL,
    add_option = FALSE,
    chunk_op = NULL,
    rmark_op = NULL) {
  chunk <- paste0(
    "```{r", ifelse(add_option, paste0(c("",rmark_op), collapse = ", "), ""), "} \n"
  )
  if (!is.null(label)) {
    chunk <- paste0(
      chunk,
      "#| label: ", "'", label, "'", "\n"
    )
  }
  # Add output options
  chunk <- paste0(
    chunk,
    "#| echo: ", echo, " \n",
    "#| warning: ", warnings, " \n",
    "#| eval: ", eval, " \n",
    ifelse(add_option & !is.null(chunk_op), paste0(paste0("#| ", chunk_op, collapse = " \n"), "\n"), "")
  )
  # if (add_option == TRUE) {
  #   for (i in 1:length(chunk_op)) {
  #     chunk <- paste0(chunk, "#| ", chunk_op[i], " \n")
  #   }
  # }
  paste0(chunk, x, "\n", "``` \n")
}
