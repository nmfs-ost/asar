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
      "[CITY], [STATE]. \\pageref*{LastPage}{} pp."
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
      "AFSC" = {
        paste0(
          "North Pacific Fishery Management Council, Anchorage, AK. Available from ",
          "https://www.npfmc.org/library/safe-reports/"
        )
      },
      "NWFSC" = {
        paste0(
          "Prepared by [COMMITTEE]."
        )
      },
      "SEFSC" = {
        paste0(
          "SEDAR, North Charleston SC. [XX] pp. ",
          "available online at: http://sedarweb.org/"
        )
      },
      "SWFSC" = {
        paste0(
          "Pacific Fishery Management Council, Portland, OR. Available from https://www.pcouncil.org/stock-assessments-and-fishery-evaluation-safe-documents/."
        )
      },
      "PIFSC" = {
        paste0(
          "NOAA Tech. Memo. [TECH MEMO NUMBER]",
          ", "
        )
      },
      "NEFSC" = {
        paste0(
          primary_author_office[["name"]], ", ",
          primary_author_office[["city"]], ", ",
          primary_author_office[["state"]], ". "
        )
      },
      {
        # Default
        paste0(
          "National Marine Fisheries Service, ",
          "[CITY], [STATE]. "
        )
      }
    )
    # Pull together parts of citation
    citation <- paste0(
      "{{< pagebreak >}} \n",
      "\n",
      "Please cite this publication as: \n",
      "\n",
      ifelse(primary_author_office[["office"]] == "SEFSC", "SEDAR.", author_list),
      " ", year, ". ",
      glue::glue("{title}"), ". ",
      region_specific_part,
      " \\pageref*{LastPage}{} pp."
    )
  }

  # Add citation as .qmd to add into template
  citation
}
