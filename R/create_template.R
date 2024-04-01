#' Create Stock Assessment Report Template
#'
#' @param new_template TRUE/FALSE; default is false otherwise if true, will pull the last saved stock assessment report skeleton
#' @param tempdir Directory for the templates/
#' @param format File type for the render (i.e. pdf, docx, html)
#' @param office Regional fisheries science center producing the report (AFSC, NEFSC, NWFSC, PIFSC, SEFSC, SWFSC)
#' @param region Full name of region in which the species is evaluated if applicable; Note: if this is not specified for your center or for
#'        the species, do not use this variable.
#' @param complex Is this a species complex? "YES" or "NO"
#' @param species Full common name for target species, split naming by a space and capitalize first letter(s)
#' @param spp_latin Latin name for the target species of this assessment
#' @param year Year the assessment is being conducted, default is current year report is being rendered
#' @param author List of authors to include in the assessment; keep authorship order
#' @param include_affiliation TRUE/FALSE; does the analyst want to include affiliations of the authors in the document?
#' @param simple_affiliation If including affiliation, adding just office name rather than full address; TRUE/FALSE, default is TRUE
#' @param alt_title TRUE/FALSE create an alternative title than the default
#' @param title Name of new title if default is not appropriate; Example, "Management Track Assessments Spring 2024"
#' @param parameters TRUE/FALSE; default TRUE - for parameterization of the script
#' @param param_names List of parameter names that will be called in the document; parameters automatically included are office, region,
#'        and species listed in function parameters
#' @param param_values List of values associated with the order of parameter names; parameters automatically included are office, region,
#'        and species listed in function parameters
#' @param resdir Directory where the model results file(s) are located
#' @param model_results Name of the model results file
#' @param model Type of assessment model that was used to assess the stock (i.e. "BAM", "SS", "AMAK", "ASAP", ect)
#' @param add_section TRUE/FALSE; is there an additional section that the analyst wants to add to the skeleton? Default is false
#' @param secdir Directory where the .qmd file is located (new file made by the analyst)
#' @param new_section File name of the new section
#' @param section_location Location where the section should be added relative to the base skeleton document
#' @param type Type of stock assessment report - terminology will vary by region (content already configured by region)
#' @param prev_year Year that previous assessment report was conducted in - for pulling previous assessment template
#' @param custom TRUE/FALSE Build custom sectioning for the template rather than the default for stock assessments in your region
#' @param custom_sections List of the sections you want to include in the custom template. Note: this only includes sections within
#'        'templates' > 'skeleton'. The section name can be used such as 'abstract' rather than the entire name '00_abstract.qmd'.
#'        If a new section is to be added, please also use parameters 'new_section', 'secdir', 'new_section', and 'section_location'
#'
#' @return Create template and pull skeleton for a stock assessment report.
#'         Function builds a YAML specific to the region and utilizes current
#'         resources and workflows from different U.S. Fishery Science Centers.
#'         General sections are called as child documents in this skeleton and
#'         each of the child documents should be edited separately.
#' @export
#'
#' @examples create_template(
#'   new_template = TRUE, format = "pdf",
#'   office = "NEFSC", region = "GB", species = "Bluefish", spp_latin = "bluishfihesi",
#'   year = 2024, author = c("John Snow", "Danny Phantom", "Patrick Star"),
#'   include_affiliation = TRUE, parameters = TRUE, param_names = c("fleet1", "fleet2", "model"),
#'   param_values = c("Commercial", "Recreational", "Woods Hole Assessment Model"),
#'   type = "RT"
#' )
create_template <- function(
    new_template = TRUE,
    tempdir = here::here(),
    format = c("pdf", "docx", "html", NULL),
    office = c("AFSC", "PIFSC", "NEFSC", "NWFSC", "SEFSC", "SWFSC"),
    region = NULL,
    complex = "NO",
    species = NULL,
    spp_latin = NULL,
    year = NULL,
    author = NULL,
    include_affiliation = FALSE,
    simple_affiliation = TRUE,
    alt_title = FALSE,
    title = NULL,
    parameters = TRUE,
    param_names = NULL,
    param_values = NULL,
    resdir = NULL,
    model_results = NULL,
    model = NULL,
    add_section = FALSE,
    secdir = NULL,
    new_section = NULL,
    section_location = NULL,
    type = c("OA", "UP", "RT", "FULL", "MT"),
    prev_year = NULL,
    custom = FALSE,
    custom_sections = NULL) {
  # If analyst forgets to add year, default will be the current year report is being produced
  if (is.null(year)) {
    year <- format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y")
  }

  # Name report
  report_name <- paste0(
    type,
    "_"
  )
  if (!is.null(region)) {
    report_name <- paste0(
      report_name,
      gsub("(\\b[A-Z])[^A-Z]+", "\\1", region)
    )
  }
  report_name <- paste0(
    report_name, "_",
    gsub(" ", "_", species),
    "_skeleton.qmd"
  )

  # Select parameter from list
  format <- match.arg(format, several.ok = FALSE)
  office <- match.arg(office, several.ok = FALSE)
  type <- match.arg(type, several.ok = FALSE)

  if (!is.null(region)) {
    dir.create(paste0("~/stock_assessment_templates", "/", office, "/", species, "/", region, "/", year), recursive = TRUE)
  } else {
    dir.create(paste0("~/stock_assessment_templates", "/", office, "/", species, "/", year), recursive = TRUE)
  }

  if (!is.null(region)) {
    subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", region, "/", year)
  } else {
    subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", year)
  }

  if (new_template == TRUE) {
    # Pull skeleton for sections
    current_folder <- file.path(find.package("ASAR"), "templates", "skeleton")
    new_folder <- subdir
    files_to_copy <- list.files(current_folder)
    file.copy(file.path(current_folder, files_to_copy), new_folder)

    # Part I
    # Create a report template file to render for the region and species
    # Create YAML header for document
    # Write title based on report type and region
    if (alt_title == FALSE) {
      title <- write_title(office = office, species = species, spp_latin = spp_latin, region = region, type = type)
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

    # Formating

    if (include_affiliation == TRUE) {
      yaml <- paste(yaml, "format: \n",
        "  ", format, ": \n",
        "  ", "  ", "keep-tex: ", "true \n",
        "  ", "  ", "template-partials: \n",
        "  ", "  ", "  ", " - graphics.tex \n",
        "  ", "  ", "  ", " - title.tex \n",
        "  ", "  ", "include-in-header: \n",
        "  ", "  ", "  ", " - in-header.tex \n",
        sep = ""
      )
    } else if (include_affiliation == FALSE) {
      yaml <- paste0(
        yaml, "format: \n",
        "  ", format, ": \n",
        "  ", "  ", "template-partials: \n",
        "  ", "  ", "  ", "- sadraft.tex \n",
        "  ", "  ", "keep-tex: true \n"
      )
    }

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

    print("__________Built YAML Header______________")
    # yaml_save <- capture.output(cat(yaml))
    # cat(yaml, file = here('template','yaml_header.qmd'))

    # Add chunk to load in assessment data
    ass_output <- chunkr(
      paste0("convert_output(output.file=", model_results, ", model=", model, ")"),
      eval = "false" # set false for testing this function in the template for now
    )

    # print("_______Standardized output data________")

    # Add page for citation of assessment report
    citation <- generate_citation(
      author = author,
      title = title,
      year = year,
      office = office
    )

    print("_______Generate Report Citaiton________")

    # Create report template

    if (custom == FALSE) {
      if (type == "OA" | type == "UP" | type == "MT") {
        sections <- paste_child(
          c(
            "01_executive_summary.qmd",
            "02_introduction.qmd"
          ),
          label = c(
            "executive_summary",
            "introduction"
          )
        )
      } else if (type == "RT" | type == "FULL") {
        sections <- paste_child(
          c(
            "01_executive_summary.qmd",
            "02_introduction.qmd",
            "03_data.qmd",
            "04_model.qmd",
            "05_results.qmd",
            "06_discussion.qmd",
            "07_acknowledgements.qmd",
            "08_references.qmd",
            "09_tables.qmd",
            "10_figures.qmd",
            "11_appendix.qmd"
          ),
          label = c(
            "executive_summary",
            "introduction",
            "data",
            "model",
            "results",
            "discussion",
            "acknowlesgements",
            "references",
            "tables",
            "figures",
            "appendix"
          )
        )
      } else {
        print("Type of assessment report is not defined")
      }
    } else {
      # Option for building custom template
      # Create custom template from existing skeleton sections
      if (add_section == FALSE) {
        section_list <- list()
        for (i in 1:length(custom_sections)) {
          grep(
            x = list.files(system.file("templates", "skeleton")),
            pattern = custom_sections[i],
            value = TRUE
          ) -> section_list[i]
        }
        sections <- paste_child(section_list,
          label = custom_sections
        )
      } else {
        # Create custom template using existing sections and new sections from analyst
        # Add sections from package options
        if (is.null(custom_sections)) {
          stop("Custom sectioning not defined.")
        }
        section_list <- list()
        if (custom == TRUE) {
          for (i in 1:length(custom_sections)) {
            grep(
              x = list.files(system.file("templates", "skeleton")),
              pattern = custom_sections[i],
              value = TRUE
            ) -> section_list[i]
          }
        }
        # Add new sections
        if (is.null(new_section) | is.null(section_location)) stop("New sections and locations not defined.")

        for (i in 1:length(new_section)) {
          add_sec_new[i] <- paste0(secdir, "/", new_section[i])
        }

        append(section_list, add_sec_new)

        sections <- paste_child(section_list,
          label = section_list
        )
      }
    }

    # Combine template sections
    report_template <- paste(yaml,
      ass_output,
      citation,
      sections,
      sep = "\n"
    )

    print("___Created desired report template______")

    # Save template as .qmd to render
    utils::capture.output(cat(report_template), file = paste0(subdir, "/", report_name), append = FALSE)

    print(cat(paste0(
      "Saved report template in directory: ", subdir, "\n",
      "To proceeed, please edit sections within the report template in order to produce a completed stock assessment report."
    )))
  } else {
    # Copy old template and rename for new year
    # Create copy of previous assessment
    if (!is.null(region)) {
      olddir <- here::here("inst", "templates", "archive", office, species, region, prev_year)
      invisible(file.copy(file.path(here::here("inst", "templates", "archive", office, species, region, prev_year), list.files(olddir)), subdir, recursive = FALSE))
    } else {
      olddir <- here::here("inst", "templates", "archive", office, species, prev_year)
      invisible(file.copy(file.path(here::here("inst", "templates", "archive", office, species, prev_year), list.files(olddir)), subdir, recursive = FALSE))
    }

    # Open previous skeleton
    skeleton <- list.files(subdir, pattern = "skeleton.qmd")
    file.show(file.path(paste0(subdir, "/", report_name))) # this opens the new file, but also restarts the session
  }
}
