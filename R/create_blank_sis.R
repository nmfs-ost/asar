#' Create blank templates for SIS data transfer
#'
#' @param dir Location to save blank templates ("sis_assmt_template.csv" and "sis_ts_template.csv") used to 
#' hold data that will be transmitted to SIS.
#' 
#' Default: the working directory (getwd())
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_blank_sis(getwd())
#' }
#'
create_blank_sis <- function(dir){
  #TODO: Add to create_template()
  download.file("https://raw.githubusercontent.com/nmfs-ost/stockplotr/refs/heads/main/inst/resources/sis_assmt_template.csv",
                fs::path(dir, "sis_assmt_template.csv"))
  
  download.file("https://raw.githubusercontent.com/nmfs-ost/stockplotr/refs/heads/main/inst/resources/sis_ts_template.csv",
                fs::path(dir, "sis_ts_template.csv"))
}
