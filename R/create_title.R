#' Write Stock Assessment Report Title
#'
#' @inheritParams create_template
#' @param complex TRUE/FALSE; Is this a species complex? Default
#'  is false.
#'
#' @return Return a string containing a title for a NOAA Fisheries stock 
#' assessment report.
#' @export
#'
#' @examples create_title(
#'   type = "SAR", office = "SEFSC", species = "Red Snapper", 
#'   spp_latin = "Lutjanus campechanus", region = "South Atlantic",
#'   year = 2024
#' )
create_title <- function(
    type = "SAR",
    office = "",
    species = "species",
    spp_latin = NULL,
    region = NULL,
    year = format(Sys.Date(), "%Y"),
    complex = NULL) {
  # Species latin name with italics latex fxn
  spp_latin <- paste("\\textit{", spp_latin, "}", sep = "")

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
    if (type == "SAR" | is.null(type)) {
      title <- paste0("Management Track Assessment of ", species, ifelse(is.null(spp_latin), " ", glue::glue(" ({spp_latin}) ")), season, " ", year)
    }
  } else if (office == "NWFSC") {
    if (is.null(region)) {
      title <- paste0("Status of the ", species, " stock along the U.S. West Coast in ", year)
    } else if (grepl("coast", tolower(region))) {
      title <- paste0("Status of the ", species, " stock off the ", region, " in ", year)
    } else {
      # region in NW should be specified as a state
      title <- paste0("Status of the ", species, " stock in U.S. waters off the coast of ", region, " in ", year)
    }
  } else if (office == "PIFSC") {
    if (is.null(region)) {
      title <- paste0("Stock Assessment for ", species, ifelse(is.null(spp_latin), " ", glue::glue(" ({spp_latin}) ")), "along the main Hawaiian Islands in ", year)
    } else {
      title <- paste0("Stock Assessment for ", species, ifelse(is.null(spp_latin), " ", glue::glue(" ({spp_latin}) ")), "on ", region, " in ", year)
    }
  } else if (office == "SEFSC") {
    if (is.null(region)) {
      title <- paste0("SEDAR XX Assessment Report for ", species, ifelse(is.null(spp_latin), " ", glue::glue(" ({spp_latin}) ")), "in ", year)
    } else {
      title <- paste0("SEDAR XX Assessment Report for ", species, ifelse(is.null(spp_latin), " ", glue::glue(" ({spp_latin}) ")), "in the ", region, " in ", year)
    }
  } else if (office == "SWFSC") {
    if (is.null(region)) {
      title <- paste0("Status of the ", species, " stock along the U.S. West Coast in ", year)
    } else {
      # region in NW should be specified as a state
      title <- paste0("Status of the ", species, " stock in U.S. waters off the coast of ", region, " in ", year)
    }
  } else {
    if (species == "species" | is.null(region)) {
      title <- "Stock Assessment Report Template"
    } else {
      title <- paste0("Stock Assessment Report for the ", species, " Stock in ", year)
    }
    # warning("office (FSC) is not defined. Please define which office you are associated with.")
  }

  # Cohesive title for any stock assessment
  # if(type %in% c('OA', 'UP')){
  #    title = paste0("Stock Assessment Update for the ", species, " Stock in the ", region, " ", year)
  # } else {
  #   title = paste0("Stock Assessment Report for the ", species, " Stock in the ", region, " ", year)
  # }

  title
}
