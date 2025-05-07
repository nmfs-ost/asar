#' Generate Citation for Stock Assessment Report
#'
#' @inheritParams create_template
#'
#' @return Generate a citation for use in publications and other
#' references associated with the stock assessment report produced
#' with `asar`.
#' @export
#'
#' @examples
#' create_citation(
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
  # FIXME: off_title is not informative enough for me to know what the object
  #        should be storing, this comment is true for many object names
  # FIXME: Add an else{} statement to handle the case when the office is not in
  #        the list of NOAA Fisheries Science Centers. The function will
  #        currently fail if it tries to this object later because it won't
  #        exist.
  if (office %in% c("NEFSC", "SEFSC", "NWFSC", "SWFSC", "PIFSC", "AFSC", "", NULL)) {
    off_title <- "NOAA Fisheries Science Center"
  }

  # FIXED: Removed nested if{}else{} statement
  if (author[1] == "") {
    no_author <- TRUE
    message("Authorship is not defined.")
    # FIXED: Moved citation inside of the first if so no more need for
    #        no_author and the function can return the citation early
      cit <- paste0(
      "{{< pagebreak >}} \n",
      "\n",
      "Please cite this publication as: \n",
      "\n",
      "[AUTHOR NAME]. [YEAR]. ",
      title, ". ", off_title, ", ",
      "[CITY], [STATE]. "
    )
    return(cit)
  } else {
    no_author <- FALSE
    # FIXED: No stop() was used in the if part of the if{}else{} statement
    #        and user was not warned if the author is not found.
    # FIXED: Instead of creating the object twice, i.e., primauth_loc <- in
    #        the if statement and then again in the else statement you can
    #        just assign unlist(strsplit(author[1], " ")) to a variable and
    #        use that variable to determine the length of the object and
    #        which part to pull for the filter call. You assigned it to a
    #        variable later in the code so I just moved it up to here.
    # FIXED: the if statement for primauthor_loc for more than one row had
    #        incorrect dplyr::filter call where it was matching last not
    #        first. But, I removed the need for that by just searching for
    #        both first and last in the same call to dplyr::filter
    # FIXED: Just use first author, i.e., author[1], so you do not have to
    #        use an if{}else{}, because it doesn't matter if author is
    #        actually just of length one, it will still work.
    # FIXED: Just read in the authorship.csv file once because reading and
    #        writing files takes a long time relative to manipulation
    author_data_frame <- tibble::as_tibble_col(
      author,
      column_name = "input"
    ) |>
      tidyr::separate_wider_regex(
        cols = input,
        # Caitlin Allen Akselrud is the only non-hyphenated dual last name
        # and needs to be included as its own pattern.
        # The second pattern allows for first initials rather than first name
        patterns = c(first = "Caitlin |^[A-Z]. |.*[a-z] ", last = ".*$")
      ) |>
      tidyr::separate_wider_delim(
        cols = last,
        delim = ". ",
        names = c("mi", "last"),
        too_few = "align_end"
      ) |>
      dplyr::mutate(
        first = gsub(" ", "", first),
        mi = ifelse(is.na(mi), "", paste0(mi, "."))
      ) |>
      dplyr::left_join(
        y = utils::read.csv(
          system.file(
            "resources",
            "authorship.csv",
            package = "asar",
            mustWork = TRUE
          )
        ),
        by = c("first", "mi", "last")
      )
    # Pull affiliation of first author
    office_loc <- utils::read.csv(system.file("resources", "affiliation_info.csv", package = "asar", mustWork = TRUE)) |>
      dplyr::filter(affiliation == author_data_frame[["office"]][1])

    # Check
    # FIXME: This check should now be if the office location was not found.
    if (nrow(office_loc) > 1) {
      stop("There is more than one office being selected in this function. Please review 'generate_ciation.R'.")
    }

    # FIXED: Just use office_loc[["city"]] and office_loc[["state"]] instead
    #        of loc_city and loc_state

    # Author naming convention formatting
    # FIXED: Use APA style citations with spaces between first and middle
    #        initial.
    # FIXED: Author's first names with hyphens and multiple capital letters
    #        were being truncated to just the first letter.
    # FIXED: Moved this out of the if{}else{} statement so the code is not
    #        duplicated.
    # FIXED: The author list should use ", and" for the last addition if
    #        there are more than two authors and just "and" if there are two
    #        authors.
    author_list <- author_data_frame |>
      dplyr::mutate(
        first_initial = gsub("([A-Z])[a-z]+", "\\1.", first),
        bib = purrr::pmap(
          list(x = first_initial, y = mi, z = last),
          \(x, y, z) toBibtex(person(given = c(x, y), family = z))
        )
      ) |>
      dplyr::pull(bib) |>
      gsub(pattern = " $", replacement = "") |>
      glue::glue_collapse(sep = ", ", last = ", and ")
  } # close ifelse statement for no author

  # Create citation string
  # FIXME: Check the title earlier and make sure it either doesn't have a full
  #        stop or ensure it does have punctuation and remove the punctuation
  #        added here. Who is to say it doesn't end in a question mark?
  # FIXED: Removed "." after author list because it already ends in a full stop
  #        after the last initial.
  # FIXED: Added a full stop after the url for AFSC.
  # FIXED: Added a full stop after the url for SEFSC.
  # FIXED: The incorrect order of title then year for the NWFSC.
  # FIXED: The incorrect use of comma after author list.
  # FIXED: Removed space after full stop for NEFSC ending.
  cit <- paste0(
    "{{< pagebreak >}} \n",
    "\n",
    "Please cite this publication as: \n",
    "\n",
    ifelse(office == "SEFSC", "SEDAR.", author_list),
    " ", year, ". ", title, ".",
    # FIXME: Should change this to use office assigned to author and use grepl
    #        instead of office == because some offices have extra text NWFSC-
    #        then the office argument could be removed from the function.
    dplyr::case_when(
      office == "AFSC" ~ " North Pacific Fishery Management Council, Anchorage, AK. Available from https://www.npfmc.org/library/safe-reports/.",
      office == "NWFSC" ~ " Prepared by [COMMITTEE]. [XX] p.",
      office == "PIFSC" ~ " NOAA Tech. Memo. [TECH MEMO NUMBER], [XX] p.",
      office == "SEFSC" ~ " SEDAR, North Charleston SC. [XX] pp. available online at: http://sedarweb.org/.",
      office == "SWFSC" ~ " Pacific Fishery Management Council, Portland, OR. Available from https://www.pcouncil.org/stock-assessments-and-fishery-evaluation-safe-documents/.",
      TRUE ~ paste0(" ", off_title, ", ", office_loc[["city"]], ", ", office_loc[["state"]], ".")
    )
  )

  # Add citation as .qmd to add into template
  cit
}
