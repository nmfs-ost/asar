#' Write R Chunk to Add Child Document
#'
#' @param x section .qmd file to add into the template, options for sections are in the 'skeleton' folder; only select XX_section.qmd files and not a, b, c... subiles
#'
#' @return Formatting R chunk for child document to add section into the template/skeleton
#' @export
#'
#' @examples
paste_child <- function(x){
  child <- c()
  for(i in length(x)){
    sec_num <- x[i]
    secdir <- here::here('inst', 'templates', 'update')
    child_loop <- paste0(
      "```{r}", "\n",
      "#| eval: true", "\n",
      "#| echo: false", "\n",
      "a <- knitr::knit_child(here::here('inst', 'templates', 'skeleton', ", "'", sec_num, "')", ", quiet = TRUE)", "\n",
      "cat(a, sep = '\\n')", "\n",
      "```"
      )
    child <- paste(child, child_loop, sep = "")
  }
  return(child)
}
