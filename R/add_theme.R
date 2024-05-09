#' Add NOAA formatting to figure or table
#'
#' @param x table or figures object from ggplot, base r plot, gt table, flextable, or kable extra
#'
#' @return Add the standard formatting for stock assessment reports for any
#' figure or table. Currently, the function is able to format objects from:
#' ggplot (ggplot2), base r plot, flextable (flextable), gt tables (gt), and kable tables (kableExtra).
#' @export
#'
#' @examples add_theme(ggplot2::ggplot(data = cars,aes(x=speed,y=dist))+ggplot2::geom_point())
add_theme <- function(x){
  if(class(x) %in% c("gg", "ggplot")){
    theme_obj <- x +
      theme_classic()
      # theme()
  }

  if(class(x)=="flextable"){
    theme_obj <- x

  }

  # gt object
  if(class(x)=="gt_tbl"){
    theme_obj <- x
  }

  if(class(x)[1] %in% c("kableExtra", "knitr_kable")){
    theme_obj <- x
  }
  return(theme_obj)
}
