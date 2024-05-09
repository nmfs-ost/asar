#' Create Stock Assessment Report Template. To see templates included in the base skeleton, please run 'list.files(system.file('templates','skeleton', package = 'ASAR'))' in the console.
#'
#' @param new_template TRUE/FALSE; default is false otherwise if true, will pull the last saved stock assessment report skeleton
#' @param tempdir Directory for the templates
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
#' @param new_section File name of the new section
#' @param section_location Location where the section should be added relative to the base skeleton document
#' @param type Type of stock assessment report - terminology will vary by region (content already configured by region)
#' @param prev_year Year that previous assessment report was conducted in - for pulling previous assessment template
#' @param custom TRUE/FALSE Build custom sectioning for the template rather than the default for stock assessments in your region
#' @param custom_sections List of the sections you want to include in the custom template. Note: this only includes sections within
#'        'templates' > 'skeleton'. The section name can be used such as 'abstract' rather than the entire name '00_abstract.qmd'.
#'        If a new section is to be added, please also use parameters 'ass_section', and 'section_location'
#'
#' @return Create template and pull skeleton for a stock assessment report.
#'         Function builds a YAML specific to the region and utilizes current
#'         resources and workflows from different U.S. Fishery Science Centers.
#'         General sections are called as child documents in this skeleton and
#'         each of the child documents should be edited separately.
#' @export
#'
#' @examples create_template(
#'   new_template = TRUE, format = "pdf", office = "NEFSC", region = "GB",
#'   species = "Atlantic Bluefish", spp_latin = "Pomatomus saltatrix", year = 2024,
#'   author = c("John Snow", "Danny Phantom", "Patrick Star"), include_affiliation = TRUE,
#'   parameters = TRUE, param_names = c("fleet1", "fleet2", "model"),
#'   param_values = c("Commercial", "Recreational", "Woods Hole Assessment Model"),
#'   type = "RT", model_results = "results.Rdata", model = "WHAM"
#' )
#'
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
    new_section = NULL,
    section_location = NULL,
    type = NULL,
    prev_year = NULL,
    custom = FALSE,
    custom_sections = NULL) {
  # If analyst forgets to add year, default will be the current year report is being produced
  if (is.null(year)) {
    year <- format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y")
  }

  # Name report
  if (!is.null(type)) {
    report_name <- paste0(
      type,
      "_"
    )
  }
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

  if (!is.null(region)) {
    subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", region, "/", year)
  } else {
    subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", year)
  }

  # if (!is.null(region)) {
  if (dir.exists(subdir) == FALSE) {
    dir.create(subdir, recursive = TRUE)
  }
  # }

  if (new_template == TRUE) {
    # Pull skeleton for sections
    current_folder <- system.file("templates", "skeleton", package = "ASAR")
    new_folder <- subdir
    files_to_copy <- list.files(current_folder)

    # Check if there are already files in the folder
    if (length(list.files(subdir)) > 0) {
      warning("There are files in this location.")
      question1 <- readline("The function wants to overwrite the files currently in your directory. Would you like to proceed? (Y/N)")

      if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
        file.copy(file.path(current_folder, files_to_copy), new_folder, overwrite = FALSE) |> suppressWarnings()
      } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
        print(paste0("Blank files for template sections were not copied into your directory. If you wish to update the template with new parameters or output files, please edit the ", report_name, " in your local folder."))
      }
    } else if (length(list.files(subdir)) == 0) {
      file.copy(file.path(current_folder, files_to_copy), new_folder, overwrite = FALSE)
    } else {
      stop("None of the arugments match statement commands. Needs developer fix.")
    }

    # Part I
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

    print("__________Built YAML Header______________")
    # yaml_save <- capture.output(cat(yaml))
    # cat(yaml, file = here('template','yaml_header.qmd'))

    # Add chunk to load in assessment data
    ass_output <- chunkr(
      paste0(
        "convert_output(output.file = ", "c('", paste(model_results, collapse = "', '"), "')",
        ", model = ", "'", model, "'",
        ", outdir = ", "'", resdir, "'", ")"
      ),
      label = "model_output",
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
      sections <- paste_child(
        c(
          "executive_summary.qmd",
          "introduction.qmd",
          "data.qmd",
          "model.qmd",
          "results.qmd",
          "discussion.qmd",
          "acknowledgements.qmd",
          "references.qmd",
          "tables.qmd",
          "figures.qmd",
          "appendix.qmd"
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
      # Option for building custom template
      # Create custom template from existing skeleton sections
      if (is.null(new_section)) {
        section_list <- add_base_section(custom_sections)
        sections <- paste_child(section_list,
          label = custom_sections
        )
      } else { # custom = TRUE
        # Create custom template using existing sections and new sections from analyst
        # Add sections from package options

        if (is.null(custom_sections)) {
          sec_list1 <- list(
            "executive_summary.qmd",
            "introduction.qmd",
            "data.qmd",
            "model.qmd",
            "results.qmd",
            "discussion.qmd",
            "acknowledgements.qmd",
            "references.qmd",
            "tables.qmd",
            "figures.qmd",
            "appendix.qmd"
          )
          sec_list2 <- add_section(
            sec_names = new_section,
            location = section_location,
            other_sections = sec_list1,
            subdir = subdir
          )
          # Create sections object to add into template
          sections <- paste_child(
            sec_list2,
            label = gsub(".qmd", "", unlist(sec_list2))
          )
        } else { # custom_sections explicit
          # Add selected sections from base
          sec_list1 <- add_base_section(custom_sections)
          # Create new sections as .qmd in folder
          sec_list2 <- add_section(
            sec_names = new_section,
            location = section_location,
            other_sections = sec_list1,
            subdir = subdir
          )
          # Create sections object to add into template
          sections <- paste_child(
            sec_list2,
            label = gsub(".qmd", "", unlist(sec_list2))
          )
        }
      }
    }

    # Combine template sections
    report_template <- paste(yaml,
      ass_output,
      citation,
      sections,
      sep = "\n"
    )

    print("___Created report template______")

    # Save template as .qmd to render
    utils::capture.output(cat(report_template), file = paste0(subdir, "/", report_name), append = FALSE)

    print(paste0(
      "Saved report template in directory: ", subdir, "\n",
      "To proceeed, please edit sections within the report template in order to produce a completed stock assessment report."
    ))

    # Open file for analyst
    file.show(file.path(paste0(subdir, "/", report_name))) # this opens the new file, but also restarts the session
  } else {
    # Copy old template and rename for new year
    # Create copy of previous assessment
    if (!is.null(region)) {
      olddir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", region, "/", prev_year)
      invisible(file.copy(file.path(olddir, list.files(olddir)), subdir, recursive = FALSE))
    } else {
      olddir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", prev_year)
      invisible(file.copy(file.path(olddir, list.files(olddir)), subdir, recursive = FALSE))
    }

    # Edit skeleton to update year and results file
    skeleton <- list.files(subdir, pattern = "skeleton.qmd")
    # Open previous skeleton
    file.show(file.path(paste0(subdir, "/", report_name)))

    svDialogs::dlg_message("Reminder: there are changes to be made when calling an old report. Please change the year in the citation and the location and name of the results file in the first chunk of the report.",
      type = "ok"
    )
  }
}
