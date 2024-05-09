#' Add NOAA formatting to figure or table
#'
#' @param x table or figures object from ggplot, base r plot, gt table, flextable, or kable extra
#'
#' @return Add the standard formatting for stock assessment reports for any
#' figure or table. Currently, the function is able to format objects from:
#' ggplot (ggplot2), base r plot, flextable (flextable), gt tables (gt), and kable tables (kableExtra).
#' @export
#'
#' @examples add_theme(ggplot2::ggplot(data = cars, ggplot2::aes(x = speed, y = dist)) + ggplot2::geom_point())
add_theme <- function(x){
  # this is bad coding practice, but what I have for now
  if (as.character(class(x)[1]) == "gg" | as.character(class(x)[2]) == "ggplot"){
    theme_obj <- x +
      # temporary formatting for example
      ggplot2::theme_classic()
      # theme()
  }

  if (as.character(class(x)[1]) == "flextable"){
    theme_obj <- x

  }

  # gt object
  if (as.character(class(x)[1]) == "gt_tbl"){
    theme_obj <- x
  }

  if (as.character(class(x)[1]) == "kableExtra" | as.character(class(x)[2]) == "knitr_kable"){
    theme_obj <- x
  }
  return(theme_obj)
}
