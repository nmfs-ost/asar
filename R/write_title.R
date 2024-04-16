#' Write Stock Assessment Title
#'
#' @param office Fishery science center performing the assessment
#' @param species Target species
#' @param region Region where the stock is being assessed if applicable.
#'               Please be explicit in the name and follow the preferred convention
#'               for the management organization.
#' @param year Year conducting the assessment report
#' @param complex TRUE/FALSE is this a species complex?
#' @param type Stock assessment type; usually inherited from the template
#' @param spp_latin Latin name for the target species
#'
#' @return Terminology for writing a title for a stock assessment across the U.S.
#'         Function is implemented within create_template.R and inherits its parameters.
#' @export
#'
#' @examples write_title(
#'   office = "SEFSC", species = "Red Snapper", region = "South Atlantic",
#'   year = 2024, type = "RT", spp_latin = "Lutjanus campechanus"
#' )
write_title <- function(
    office = NULL,
    species = NULL,
    region = NULL,
    year = NULL,
    complex = NULL,
    type = NULL,
    spp_latin = NULL) {
  if (is.null(year)) {
    year <- format(Sys.Date(), "%Y")
  }

  # Create title dependent on regional language
  if (office == "AFSC") {
    if (is.null(complex)) {
      title <- paste0("Assessment of the ", species, " Stock in the ", region)
    } else {
      title <- paste0("Assessment of the ", species, " Stock Complex in the ", region)
    }
  } else if (office == "NEFSC") {
    if (as.numeric(format(Sys.Date(), "%m")) %in% c(3, 4, 5)) {
      season <- "Spring"
    } else if (as.numeric(format(Sys.Date(), "%m")) %in% c(6, 7, 8)) {
      season <- "Summer"
    } else if (as.numeric(format(Sys.Date(), "%m")) %in% c(9, 10, 11)) {
      season <- "Fall"
    } else if (as.numeric(format(Sys.Date(), "%m")) %in% c(12, 1, 2)) {
      season <- "Winter"
    }
    if (type == "RT") {
      title <- paste0("Research Track Assessment for ", species, " (", spp_latin, ") ", season, " ", year)
    } else {
      title <- paste0("Management Track Assessment of ", species, " (", spp_latin, ") ", season, " ", year)
    }
  } else if (office == "NWFSC") {
    if (is.null(region)) {
      title <- paste0("Status of the ", species, " stock along the U.S. West Coast in ", year)
    } else {
      # region in NW should be specified as a state
      title <- paste0("Status of the ", species, " stock in U.S. waters off the coast of ", region, " in ", year)
    }
  } else if (office == "PIFSC") {
    if (is.null(region)) {
      title <- paste0("Stock Assessment for ", species, " (", spp_latin, ") along the main Hawaiian Islands in ", year)
    } else {
      title <- paste0("Stock Assessment for ", species, " (", spp_latin, ") on ", region, " in ", year)
    }
  } else if (office == "SEFSC") {
    title <- paste0("SEDAR XX Assessment Report for ", species, " (", spp_latin, ") in the ", region, " in ", year)
  } else if (office == "SWFSC") {
    if (is.null(region)) {
      title <- paste0("Status of the ", species, " stock along the U.S. West Coast in ", year)
    } else {
      # region in NW should be specified as a state
      title <- paste0("Status of the ", species, " stock in U.S. waters off the coast of ", region, " in ", year)
    }
  } else {
    warning("office (FSC) is not defined. Please define which office you are associated with.")
  }

  # Cohesive title for any stock assessment
  # if(type %in% c('OA', 'UP')){
  #    title = paste0("Stock Assessment Update for the ", species, " Stock in the ", region, " ", year)
  # } else {
  #   title = paste0("Stock Assessment Report for the ", species, " Stock in the ", region, " ", year)
  # }

  return(title)
}
