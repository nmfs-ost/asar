#' Write R chunk to template
#'
#' @param x content within the R chunk to be written contained inside quotation marks
#' @param echo repeat code into the document; default is false
#' @param warnings Report warnings into console during render? Default is false
#' @param eval Evaluate the chunk? default is true
#' @param add_option TRUE/FALSE add additional chunk options? default is FALSE
#' @param chunk_op list of chunk options to add; example: c("output: true", "error: false)
#'
#' @return
#' @export
#'
#' @examples
chunkr <- function(
    x,
    echo = "false",
    warnings = "false",
    eval = "true",
    add_option = FALSE,
    chunk_op = NULL){
  chunk <- paste0(
    "```{r} \n",
    # Add output options
    "|# echo: ", echo, " \n",
    "|# warning: ", warnings, " \n",
    "|# eval: ", eval, " \n")
    if(add_option==TRUE){
      for (i in 1:length(chunk_op)) {
        chunk <- paste0(chunk, "|# ", chunk_op[i], " \n")
      }
    }
    chunk <- paste0(chunk, x, "\n", "``` \n")
    return(chunk)
}
