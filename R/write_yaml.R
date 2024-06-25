#---- Write YAML ----

#' Write YAML for output including various other dependent functions.
#' This is intended to be concatenated and put into a quarto file.
#'
#' @param office NOAA regional office, parameter will designate some situations that are region specific
#' @param species common name for the target species or complex in the assessment
#' @param complex is this stock assessment being conducted for a species complex?
#' @param spp_latin latin name for the target species if applicable
#' @param region Full name of region in which the species is evaluated if applicable; Note: if this is not specified for your center or for
#'        the species, do not use this variable.
#' @param year Year the assessment is being conducted, default is current year report is being rendered
#' @param type Type of report to build - default is SAR
#' @param author List of authors to include in the assessment; keep authorship order
#' @param format File type for the render (i.e. pdf, docx, html)
#' @param include_affiliation TRUE/FALSE; does the analyst want to include affiliations of the authors in the document?
#' @param simple_affiliation If including affiliation, adding just office name rather than full address; TRUE/FALSE, default is TRUE
#' @param parameters TRUE/FALSE include parameters in the YAML header
#' @param param_names List of parameter names that will be called in the document; parameters automatically included are office, region,
#'        and species listed in function parameters
#' @param param_values List of values associated with the order of parameter names; parameters automatically included are office, region,
#'        and species listed in function parameters
#'
#' @return NULL
#' @noRd
#'
#' @examples NULL
write_yaml <- function(office = NULL,
                       species = NULL,
                       complex = NULL,
                       spp_latin = NULL,
                       region = NULL,
                       year = NULL,
                       type = NULL,
                       author = NULL,
                       format = NULL,
                       include_affiliation = NULL,
                       simple_affiliation = NULL,
                       parameters = TRUE,
                       param_names = NULL,
                       param_values = NULL){

  # Create a report template file to render for the region and species
  # Create YAML header for document
  # Write title based on report type and region
  if (alt_title == FALSE) {
    title <- write_title(office = office, species = species, spp_latin = spp_latin, region = region, type = type, year = year)
  } else if (alt_title == TRUE) {
    if (!exists(title)) {
      stop("Alternate title not defined. Please define an alternative title in the parameter 'title'.")
    } else {
      title <- paste(title)
    }
  }

  # Pull authors and affiliations from national db
  # Parameters to add authorship to YAML
  # Read authorship file
  authors <- utils::read.csv(system.file("resources", "authorship.csv", package = "ASAR", mustWork = TRUE)) |>
    dplyr::mutate(
      mi = dplyr::case_when(
        mi == "" ~ NA,
        TRUE ~ mi
      ),
      name = dplyr::case_when(
        is.na(mi) ~ paste0(first, " ", last),
        TRUE ~ paste(first, mi, last, sep = " ")
      )
    ) |>
    dplyr::select(name, office) |>
    dplyr::filter(name %in% author)

  if (include_affiliation == TRUE) {
    affil <- utils::read.csv(system.file("resources", "affiliation_info.csv", package = "ASAR", mustWork = TRUE))
  }

  author_list <- list()
  if (include_affiliation == TRUE & simple_affiliation == FALSE) {
    for (i in 1:nrow(authors)) {
      auth <- authors[i, ]
      aff <- affil |>
        dplyr::filter(affiliation == auth$office)
      paste0(
        "  ", "- name: ", "'", auth$name, "'", "\n",
        "  ", "  ", "affiliations:", "\n",
        "  ", "  ", "  ", "- name: ", "'", "NOAA Fisheries ", aff$name, "'", "\n",
        "  ", "  ", "  ", "  ", "address: ", "'", aff$address, "'", "\n",
        "  ", "  ", "  ", "  ", "city: ", "'", aff$city, "'", "\n",
        "  ", "  ", "  ", "  ", "state: ", "'", aff$state, "'", "\n",
        "  ", "  ", "  ", "  ", "postal-code: ", "'", aff$postal.code, "'", "\n"
        # sep = " "
      ) -> author_list[[i]]
    }
  } else if (include_affiliation == TRUE & simple_affiliation == TRUE) {
    for (i in 1:nrow(authors)) {
      auth <- authors[i, ]
      aff <- affil |>
        dplyr::filter(affiliation == auth$office)
      paste0(
        "  ", "- name: ", "'", auth$name, "'", "\n",
        "  ", "  ", "affiliations: ", "'", aff$name, "'", "\n"
      ) -> author_list[[i]]
    }
  } else {
    for (i in 1:nrow(authors)) {
      auth <- authors[i, ]
      paste0("  ", "- ", "'", auth$name, "'", "\n") -> author_list[[i]]
    }
  }

  # Creating YAML
  yaml <- paste0(
    # start YAML notation
    "---", "\n",

    # Tile
    "title: ", "'", title, "'", "\n",

    # Author
    "author:", "\n"
  )
  # Add authors
  add_authors <- NULL
  for (i in 1:length(author_list)) {
    toad <- paste(author_list[[i]], sep = ",")
    add_authors <- paste0(add_authors, toad) # -> add_authors
  }
  yaml <- paste0(yaml, add_authors)

  # Add other parts
  yaml <- paste0(
    yaml,
    # Date
    "date: today", "\n"
  )

  # Formatting

  if (include_affiliation == TRUE) {
    yaml <- paste(yaml, "format: \n",
                  "  ", format, ": \n",
                  "  ", "  ", "toc: ", "true \n",
                  "  ", "  ", "keep-tex: ", "true \n",
                  "  ", "  ", "template-partials: \n",
                  # "  ", "  ", "  ", " - graphics.tex \n",
                  "  ", "  ", "  ", " - title.tex \n",
                  "  ", "  ", "include-in-header: \n",
                  "  ", "  ", "  ", " - in-header.tex \n",
                  sep = ""
    )
  } else if (include_affiliation == FALSE) {
    yaml <- paste0(
      yaml, "format: \n",
      "  ", format, ": \n",
      "  ", "  ", "toc: ", "true \n",
      "  ", "  ", "template-partials: \n",
      "  ", "  ", "  ", "- title.tex \n",
      "  ", "  ", "keep-tex: true \n"
    )
  }

  # Add lua filters for compliance
  # PLACEHOLDER: Uncomment once .lua text is built

  # yaml <- paste0(yaml,
  #                # "contributes:", "\n",
  #                "filters:", "\n",
  #                "  ", "  ", "- acronyms.lua", "\n",
  #                "  ", "  ", "- accessibility.lua", "\n")

  # Parameters
  # office, region, and species are default parameters
  yaml <- paste0(
    yaml, "params:", "\n",
    "  ", " ", "office: ", "'", office, "'", "\n",
    "  ", " ", "species: ", "'", species, "'", "\n",
    "  ", " ", "spp_latin: ", "'", spp_latin, "'", "\n"
  )
  if (!is.null(region)) {
    yaml <- paste0(yaml, "  ", " ", "region: ", "'", region, "'", "\n")
  }
  if (parameters == TRUE) {
    if (!is.null(param_names) & !is.null(param_values)) {
      add_params <- NULL
      for (i in 1:length(param_names)) {
        toad <- paste("  ", " ", param_names[i], ": ", "'", param_values[i], "'", "\n", sep = "")
        add_params <- paste0(add_params, toad)
      }
    } else {
      print("Please define parameter names (param_names) and values (param_values).")
    }

    yaml <- paste0(yaml, add_params)
  }

  # Add style guide
  # create_style_css(species = species, savedir = subdir)

  # yaml <- paste0(
  #   yaml,
  #   "css: styles.css", "\n"
  # )

  # Close yaml
  yaml <- paste0(yaml, "---")
}
