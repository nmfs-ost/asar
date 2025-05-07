#' Generate Citation for Stock Assessment Report
#'
#' @inheritParams create_template
#'
#' @return Generate a citation for use in publications and other
#' references associated with the stock assessment report produced
#' with `asar`.
#' @export
#'
#' @examples create_citation(
#'   title = "SA Report for Jellyfish",
#'   author = c("John Snow", "Danny Phantom", "Patrick Star"),
#'   year = 2024, office = "NEFSC"
#' )
#'
create_citation <- function(
    author,
    title,
    year,
    office) {
  
  off_title <- "NOAA Fisheries Science Center"
  
  if (missing(author)) message("Authorship is not defined.")
  
  
      # Pull affiliation of first author
      if (length(unlist(strsplit(author[1], " "))) == 3) {
        primauth_loc <- utils::read.csv(system.file("resources", "authorship.csv", package = "asar", mustWork = TRUE)) |>
          dplyr::filter(last == ifelse(length(unlist(strsplit(author[1], " "))) == 3,
                                       unlist(strsplit(author[1], " "))[3],
                                       unlist(strsplit(author[1], " "))[2]),
                        first == unlist(strsplit(author[1], " "))[1])
      if (nrow(primauth_loc) == 0) warning("Author is not found in the database, Please use add_author instead.")

      if (nrow(primauth_loc) == 1) {
        office_loc <- utils::read.csv(system.file("resources", "affiliation_info.csv", package = "asar", mustWork = TRUE)) |>
          dplyr::filter(affiliation == primauth_loc$office)
        # Check
        if (nrow(office_loc) > 1) {
          stop("There is more than one office being selected in this function. Please review 'generate_ciation.R'.")
        }
        
        loc_city <- office_loc$city
        loc_state <- office_loc$state
      } else {
        loc_city <- "[CITY]"
        loc_state <- "[STATE]"
      }
     
      # Author naming convention formatting
      author_spl <- unlist(strsplit(author[1], split = " "))
      author1 <- paste0(
        ifelse(length(author_spl) == 3, author_spl[3], author_spl[2]), ", ",
        substring(author_spl[[1]][1], 1, 1), ".",
        ifelse(length(author_spl) == 3, author_spl[2], "")
      )
    }
  if (length(author) > 1) { 
    author_list <- paste0(author1)
    for (i in 2:length(author)) {
      auth_extract <- unlist(strsplit(author[i], split = " "))
      auth_extract2 <- paste0(
        substring(auth_extract[1], 1, 1), ".",
        ifelse(length(auth_extract) == 3, auth_extract[2], ""), " ",
        ifelse(length(auth_extract) == 3, auth_extract[3], auth_extract[2])
      )
      author_list <- paste0(author_list, ", ", auth_extract2)
    } # close for loop
  }

  # Create citation string
  cit <- paste0(
    "{{< pagebreak >}} \n",
    "\n",
    "Please cite this publication as: \n",
    "\n",
  )
  if (office == "AFSC") {
    cit <- paste0(
      cit,
      author_list, ". ", year, ". ",
      title,
      ". North Pacific Fishery Management Council, Anchorage, AK. Available from ",
      "https://www.npfmc.org/library/safe-reports/"
    )
  } else if (office == "NWFSC") {
    cit <- paste0(
      cit,
      author_list, ". ", title, ".", year,
      ". Prepared by [COMMITTEE]. [XX] p."
    )
  } else if (office == "PIFSC") {
    cit <- paste0(
      cit,
      author_list, ". ", year, ". ",
      title, ". NOAA Tech. Memo. [TECH MEMO NUMBER]",
      ", ", "[XX] p."
    )
  } else if (office == "SEFSC") {
    cit <- paste0(
      cit,
      "SEDAR. ", year, ". ", title, ". ",
      "SEDAR, North Charleston SC. [XX] pp. ",
      "available online at: http://sedarweb.org/"
    )
  } else if (office == "SWFSC") {
    cit <- paste0(
      cit,
      author_list, ", ", year, ". ", title,
      ". Pacific Fishery Management Council, Portland, OR. Available from https://www.pcouncil.org/stock-assessments-and-fishery-evaluation-safe-documents/."
    )
  } else { # this includes NEFSC
    cit <- paste0(
      cit,
      author_list, ". ", year, ". ",
      title, ". ", off_title, ", ",
      loc_city, ", ", loc_state, ". "
    )
  }

  # Add citation as .qmd to add into template
  cit
}
