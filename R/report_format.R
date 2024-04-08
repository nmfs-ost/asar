#' Add FSC Formatting
#'
#' @param office office the analyst is associated with and in turn will active the formatting for the RFMO in the region
#'
#' @return Change the formatting of the report based on current requirements from the fishery management councils to the fishery science centers.
#' @export
#'
#' @examples report_format(office = "NEFSC")
report_format <- function(
    office = NULL) {
  # Add report formatting based on region
  if (office == "AFSC") {

  } else if (office == "NEFSC") {

  } else if (office == "NWFSC") {
    template <- paste0(
      "output: ",
      "  ", "sa"
    )
  } else if (office == "PIFSC") {

  } else if (office == "SEFSC") {

  } else if (office == "SWFSC") {

  } else {
    print("office (FSC) is not defined. Please define which office you are associated with.")
  }
}
