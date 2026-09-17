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
#' \dontrun{
#' create_citation(
#'   title = "SA Report for Jellyfish",
#'   authors = c("Danny Phantom" = "SWFSC-LJCA", "John Snow" = "AFSC-ABL", "Jane Doe" = "NWFSC-SWA"),
#'   year = 2024
#' )
#' }
#'
create_citation <- function(
  authors = NULL,
  title = "[TITLE]",
  year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y")
) {
  # Based on the following bib entry
  # @techreport{bredeck_2026,
  #   type           = {stockassessment}, % CSL maps 'type' in bibtex to 'genre'
  #   title            = {Status of Petrale Sole off the U.S. West Coast},
  #   author       = {Bredeck, Samantha},
  #   institution = {NOAA Fisheries OST},
  #   address     = {Silver Spring, MD},
  #   DOI           = {DOI-123-456}, % or URL
  #   URL           = {https://github.com/nmfs-ost/asar},
  #   year           = {2025},
  #   month       = {Sep}
  # }
  # Authors. Year. Title. Publishing office. # p. Accessible at [URL/DOI].

  # Check if authors is input - improved from previous fxn so did not fail
  if (is.null(authors) | any(authors == "")) {
    cli::cli_alert_warning("Authorship not defined.")
    cli::cli_alert_info("Did you forget to specify `authors`?")
    # Define default citation - needs authors editing
    citation <- paste0(
      "{{< pagebreak >}} \n",
      "\n",
      "Please cite this publication as: \n",
      "\n",
      "[AUTHOR NAME]. [YEAR]. ",
      title, ". National Marine Fisheries Service, ",
      "[CITY], [STATE]. \\pageref*{LastPage}{} pp. Accessible at [URL/DOI]."
    )
  } else {
    author_data_frame <- data.frame(office = authors)

    # Extract location of primary author
    primary_author_office <- asar::affiliation_info |>
      dplyr::filter(affiliation == author_data_frame$office[1])

    # Check
    if (nrow(primary_author_office) < 1) {
      cli::cli_alert_warning("No location found for primary author.")
      cli::cli_alert("Please edit the citation in the 'skeleton.qmd'.")
      cit <- paste0(
        "{{< pagebreak >}} \n",
        "\n",
        "Please cite this publication as: \n",
        "\n",
        "[AUTHOR NAME]. [YEAR]. ",
        title, ". National Marine Fisheries Service, ",
        "[CITY], [STATE]. \\pageref*{LastPage}{} pp."
      )
    } else {
      author_list <- format_citation_authors(names(authors))
    }

    # Authored by Sam Schiano with contributions from Kelli Johnson

    region_specific_part <- switch(primary_author_office[["office"]],
      "AFSC" = "North Pacific Fishery Management Council, Anchorage, AK. \\pageref*{LastPage}{} pp. Available from https://www.npfmc.org/library/safe-reports/",
      "NWFSC" = "Pacific Fishery Management Council, Portland, OR. \\pageref*{LastPage}{} pp. Available from https://www.pcouncil.org/stock-assessments-and-fishery-evaluation-safe-documents/",
      "SEFSC" = "SEDAR, North Charleston SC. \\pageref*{LastPage}{} pp. available online at: http://sedarweb.org/",
      "SWFSC" = "Pacific Fishery Management Council, Portland, OR. \\pageref*{LastPage}{} pp. Available from https://www.pcouncil.org/stock-assessments-and-fishery-evaluation-safe-documents/",
      "PIFSC" = "Pacific Islands Fisheries Science Center. [CITY] [STATE]. \\pageref*{LastPage}{} pp. Available at [URL/DOI]",
      "NEFSC" = {
        paste0(
          primary_author_office[["name"]], ", ",
          primary_author_office[["city"]], ", ",
          primary_author_office[["state"]], ". \\pageref*{LastPage}{} pp. Available at https://apps-nefsc.fisheries.noaa.gov/saw/sasi.php"
        )
      },
      {
        # Default
        paste0(
          "National Marine Fisheries Service, ",
          "[CITY], [STATE]. \\pageref*{LastPage}{} pp. Available at [URL/DOI]"
        )
      }
    )
    # Pull together parts of citation
    citation <- paste0(
      "{{< pagebreak >}} \n",
      "\n",
      "Please cite this publication as: \n",
      "\n",
      author_list,
      " ", year, ". ",
      glue::glue("{title}"), ". ",
      region_specific_part
    )
  }

  # Add citation as .qmd to add into template
  citation
}
