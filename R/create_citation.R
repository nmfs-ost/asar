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
#'   author = c("John Snow", "Danny Phantom", "Patrick Star"),
#'   year = 2024
#' )
#' }
#'
create_citation <- function(
    author = NULL,
    title = "[TITLE]",
    year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y")) {
  # Check if author is input - improved from previous fxn so did not fail
    if (is.null(author) | any(author == "")) {
      cli::cli_alert("Authorship is not defined.")
      # Define default citation - needs author editing
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
      # Authored by Kelli Johnson in previous PR
      author_data_frame <- data.frame(office = author) |>
        tibble::rownames_to_column("input") |>
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
        )
        # dplyr::left_join(
        #   y = utils::read.csv(
        #     system.file(
        #       "resources",
        #       "authorship.csv",
        #       package = "asar",
        #       mustWork = TRUE
        #     )
        #   ),
        #   by = c("first", "mi", "last")
        # )

      # Extract location of primary author
      primary_author_office <- utils::read.csv(system.file("resources", "affiliation_info.csv", package = "asar", mustWork = TRUE)) |>
        dplyr::filter(affiliation == author_data_frame$office[1])

      # Check
      if (nrow(primary_author_office) < 1) {
        cli::cli_alert_warning("No location found for primary author. Please edit the citation found in the 'skeleton.qmd'.")
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
        # Author naming convention formatting
        author_list <- author_data_frame |>
          dplyr::mutate(
            first_initial = gsub("([A-Z])[a-z]+", "\\1.", first),
            bib = purrr::pmap(
              list(x = first_initial, y = mi, z = last),
              \(x, y, z) utils::toBibtex(utils::person(given = c(x, y), family = z))
            )
          ) |>
          dplyr::pull(bib) |>
          gsub(pattern = " $", replacement = "") |>
          glue::glue_collapse(sep = ", ", last = ", and ")
      }

      # Authored by Sam Schiano with contributions from Kelli Johnson

      region_specific_part <- switch(
        primary_author_office[["office"]],
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
          ". National Marine Fisheries Service, ",
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
        ifelse(primary_author_office[["office"]]=="SEFSC", "SEDAR.", author_list),
        " ", year, ". ",
        glue::glue("{title}"), ". ",
        region_specific_part,
        " \\pageref*{LastPage}{} pp."
      )
    }

  # Add citation as .qmd to add into template
  citation
}
