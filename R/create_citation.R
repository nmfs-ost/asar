#' Generate Citation for Stock Assessment Report
#'
#' @param author author list- inherited from template function is using
#' @param title title of the report - inherited from template function if using
#' @param year year of the report for citation purposes
#' @param office NOAA line office that the report is being processed out of
#'
#' @return Generate a citation for use in publications and other references for
#'         the stock assessment report generated in the asar package.
#' @export
#'
#' @examples create_citation(
#'   title = "SA Report for Jellyfish",
#'   author = c("John Snow", "Danny Phantom", "Patrick Star"),
#'   year = 2024, office = "NEFSC"
#' )
#'
create_citation <- function(
    author = NULL,
    title = NULL,
    year = NULL,
    office = NULL) {
  if (office %in% c("NEFSC", "SEFSC", "NWFSC", "SWFSC", "PIFSC", "AFSC", "", NULL)) {
    off_title <- "NOAA Fisheries Science Center"
  }

  if (length(author) == 1) {
    if (author == "") {
      no_author <- TRUE
      message("Authorship is not defined.")
    } else {
      no_author <- FALSE
      # Pull affiliation of first author
      if (length(unlist(strsplit(author[1], " "))) == 3) {
        primauth_loc <- utils::read.csv(system.file("resources", "authorship.csv", package = "asar", mustWork = TRUE)) |>
          dplyr::filter(last == unlist(strsplit(author[1], " "))[3])
      } else {
        primauth_loc <- utils::read.csv(system.file("resources", "authorship.csv", package = "asar", mustWork = TRUE)) |>
          dplyr::filter(last == unlist(strsplit(author[1], " "))[2])
        if (nrow(primauth_loc) == 0) stop("Author is not found in the database, Please use add_author instead.")
      }

      # Check and fix if there is more than one author with the same last name
      if (nrow(primauth_loc) > 1) {
        primauth_loc <- utils::read.csv(system.file("resources", "authorship.csv", package = "asar", mustWork = TRUE)) |>
          dplyr::filter(last == unlist(strsplit(author[1], " "))[1])
      }

      office_loc <- utils::read.csv(system.file("resources", "affiliation_info.csv", package = "asar", mustWork = TRUE)) |>
        dplyr::filter(affiliation == primauth_loc$office)

      # Check
      if (nrow(office_loc) > 1) {
        stop("There is more than one office being selected in this function. Please review 'generate_ciation.R'.")
      }

      loc_city <- office_loc$city
      loc_state <- office_loc$state

      # Author naming convention formatting
      author_spl <- unlist(strsplit(author, split = " "))
      author_list <- paste0(
        ifelse(length(author_spl) == 3, author_spl[3], author_spl[2]), ", ",
        substring(author_spl[[1]][1], 1, 1), ".",
        ifelse(length(author_spl) == 3, author_spl[2], "")
      )
    }
  } else { # close if only one author
    no_author <- FALSE
    # Pull affiliation of first author
    if (length(unlist(strsplit(author[1], " "))) == 3) {
      primauth_loc <- utils::read.csv(system.file("resources", "authorship.csv", package = "asar", mustWork = TRUE)) |>
        dplyr::filter(last == unlist(strsplit(author[1], " "))[3])
    } else {
      primauth_loc <- utils::read.csv(system.file("resources", "authorship.csv", package = "asar", mustWork = TRUE)) |>
        dplyr::filter(last == unlist(strsplit(author[1], " "))[2])
    }

    # Check and fix if there is more than one author with the same last name
    if (nrow(primauth_loc) > 1) {
      primauth_loc <- utils::read.csv(system.file("resources", "authorship.csv", package = "asar", mustWork = TRUE)) |>
        dplyr::filter(last == unlist(strsplit(author[1], " "))[1])
    }

    office_loc <- utils::read.csv(system.file("resources", "affiliation_info.csv", package = "asar", mustWork = TRUE)) |>
      dplyr::filter(affiliation == primauth_loc$office)

    # Check
    if (nrow(office_loc) > 1) {
      stop("There is more than one office being selected in this function. Please review 'generate_ciation.R'.")
    }

    loc_city <- office_loc$city
    loc_state <- office_loc$state

    # Author naming convention formatting
    author1 <- unlist(strsplit(author[1], split = " "))
    author1 <- paste0(
      ifelse(length(author1) == 3, author1[3], author1[2]), ", ",
      substring(author1[1], 1, 1), ".",
      ifelse(length(author1) == 3, author1[2], "")
    )
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
  if (no_author) {
    cit <- paste0(
      "{{< pagebreak >}} \n",
      "\n",
      "Please cite this publication as \n",
      "\n",
      "AUTHOR NAME. YEAR. ",
      title, ". ", off_title, ", ",
      "CITY, STATE. "
    )
  } else if (office != "SEFSC") {
    cit <- paste0(
      "{{< pagebreak >}} \n",
      "\n",
      "Please cite this publication as \n",
      "\n",
      author_list, ". ", year, ". ",
      title, ". ", off_title, ", ",
      loc_city, ", ", loc_state, ". "
    )
  } else {
    cit <- paste0(
      "'{{< pagebreak >}}' \n",
      "\n",
      "Please cite this publication as \n",
      "\n",
      "SEDAR. ", year, ". ", title,
      "SEDAR, North Charleston SC. XXpp. ",
      "available online at: http://sedarweb.org/"
    )
  }

  # Add citation as .qmd to add into template
  cit
}
