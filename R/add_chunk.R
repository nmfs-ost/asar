#' Write R chunk to template
#'
#' @param x Content to be written within the R chunk. Wrap in quotation marks ("").
#' @param label The name of the chunk in the 'label:' section of the R code chunk.
#'  This should be in snakecase (i.e., in which words are written in lowercase and
#'   connected by underscores).
#' @param add_option TRUE/FALSE; Option to add additional chunk options. Default is false.
#' @param chunk_option List of chunk options to add. For example: c("output: true", "error: false)
#' @param rmark_option List of chunk options to add after indicating the language of the chunk as used in Rmarkdown.
#'
#' @return Write an additional R chunk into the template using this function.
#'         The code can be written as usual, just remember to put it entirely
#'         in quotes for the function to render it properly
#' @export
#'
#' @examples add_chunk("plot(cars$speed, cars$distance)")
add_chunk <- function(
    x,
    label = NULL,
    add_option = TRUE,
    chunk_option = c("echo: false", "warnings: false","eval: true"),
    rmark_option = NULL) {
  chunk <- paste0(
    "```{r", ifelse(add_option, paste0(c("", rmark_option), collapse = ", "), ""), "} \n"
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
    # "#| echo: ", echo, " \n",
    # "#| warning: ", warnings, " \n",
    # "#| eval: ", eval, " \n",
    ifelse(add_option & !is.null(chunk_option), paste0(paste0("#| ", chunk_option, collapse = " \n"), "\n"), "")
  )
  # if (add_option == TRUE) {
  #   for (i in 1:length(chunk_option)) {
  #     chunk <- paste0(chunk, "#| ", chunk_option[i], " \n")
  #   }
  # }
  paste0(chunk, x, "\n", "``` \n")
}
