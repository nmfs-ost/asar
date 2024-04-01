#' Export report object
#'
#' Function to export specified object from R environment found in the
#' stock assessment report.
#'
#' @param object table, plot, or other object in the environment to export for
#'                reference outside of the report document
#' @param file_format pdf, docx, xlsx, csv, rdata; default is docx
#' @param subdir directory where to export the object
#' @return Objects put through this function will be put into the folder "exported"
#'         for better organization; DO NOT PUSH THESE TO THE REPO
#' @author Samantha Schiano
#' @export
#'
#' @examples
#' source <- c("NMFS Groundfish Survey", "", "U.S. Trawl Fisheries", "", "")
#' data <- c("Survey biomass", "Age Composition", "Catch", "Age Composition", "Length Composition")
#' years <- c(
#'   "1984-1999 (triennial), 2001-2013 (biennial)",
#'   "1984, 1987, 1990, 1993, 1996, 1999, 2003, 2005, 2007, 2009, 2011", "1961-2013",
#'   "1990,1998-2002, 2004, 2005, 2006, 2008, 2010", "1963-1977, 1991-1997"
#' )
#' test_obj <- data.frame(source, data, years)
#'
#' export_object(object = test_obj, file_format = "csv", subdir = "~")
#'
export_object <- function(
    object = NULL,
    file_format = "docx",
    subdir = NULL) {
  if (file_format == "pdf") {
    grDevices::pdf(file = paste0(subdir, "/", sprintf(deparse(substitute(object))), ".pdf")) # pdf fxn in base r
    gridExtra::grid.table(object)
    grDevices::dev.off()
    # Doesn't fit to page
  } else if (file_format == "docx") {
    doc_exp <- officer::read_docx()
    doc_exp <- officer::body_add_table(object)
    print(doc_exp,
      target = paste0(subdir, "/", sprintf(deparse(substitute(object))), ".docx")
    ) # %>% invisible()
  } else if (file_format == "xlsx") {
    openxlsx::write.xlsx(object,
      file = paste0(subdir, "/", sprintf(deparse(substitute(object))), ".xlsx"),
      overwrite = FALSE
    )
  } else if (file_format == "csv") {
    utils::write.csv(object,
      file = paste0(subdir, "/", sprintf(deparse(substitute(object))), ".csv"),
      row.names = FALSE
    )
  } else if (file_format == "rdata") {
    save(object,
      file = paste0(subdir, "/", sprintf(deparse(substitute(object))), ".Rdata")
    )
  } else {
    stop("File format is not currently available for output. Please open an issue in the GitHub repository to request the addition.")
  }
}
