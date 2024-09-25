#' Create Stock Assessment Report Template. To see templates included in the base skeleton, please run 'list.files(system.file('templates','skeleton', package = 'asar'))' in the console.
#'
#' @param new_template TRUE/FALSE; default is false otherwise if true, will pull the last saved stock assessment report skeleton
#' @param format File type for the render (i.e. pdf, docx, html)
#' @param office Regional fisheries science center producing the report (AFSC, NEFSC, NWFSC, PIFSC, SEFSC, SWFSC)
#' @param region Full name of region in which the species is evaluated if applicable; Note: if this is not specified for your center or for
#'        the species, do not use this variable.
#' @param complex Is this a species complex? "YES" or "NO"
#' @param species Full common name for target species, split naming by a space and capitalize first letter(s)
#' @param spp_latin Latin name for the target species of this assessment
#' @param year Year the assessment is being conducted, default is current year report is being rendered
#' @param file_dir location where the stock assessment files will be kept produced from this function. Default is set to working directory
#' @param author List of authors to include in the assessment; keep authorship order
#' @param add_author temporarily add an author that is not currently in the database. Follow the format of "First MI Last".
#'        Please leave a comment on the github issues page to be added.
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
#' @param model Type of assessment model that was used to assess the stock (i.e. "BAM", "SS3", "AMAK", "ASAP", ect)
#' @param new_section File name of the new section
#' @param section_location Location where the section should be added relative to the base skeleton document
#' @param type Type of report to build - default is SAR
#' @param prev_year Year that previous assessment report was conducted in - for pulling previous assessment template
#' @param custom TRUE/FALSE Build custom sectioning for the template rather than the default for stock assessments in your region
#' @param custom_sections List of the sections you want to include in the custom template. Note: this only includes sections within
#'        list.files(system.file("templates", "skeleton", package = "asar")). The section name can be used such as 'abstract' rather than the entire name '00_abstract.qmd'.
#'        If a new section is to be added, please also use parameters 'new_section', and 'section_location'
#' @param include_figures Determine if figures are included into the report
#' @param include_tables Indicate if tables are included into the report
#' @param add_image add outside image of species to the template
#' @param spp_image full directory and species image name to direct the program where to find and extract the image
#'
#' @return Create template and pull skeleton for a stock assessment report.
#'         Function builds a YAML specific to the region and utilizes current
#'         resources and workflows from different U.S. Fishery Science Centers.
#'         General sections are called as child documents in this skeleton and
#'         each of the child documents should be edited separately.
#' @export
#' @examples create_template()
#'
create_template <- function(
    new_template = TRUE,
    format = c("pdf", "docx", "html", NULL),
    office = c("AFSC", "PIFSC", "NEFSC", "NWFSC", "SEFSC", "SWFSC"),
    region = NULL,
    complex = "NO",
    species = NULL,
    spp_latin = NULL,
    year = NULL,
    file_dir = getwd(),
    author = "",
    add_author = NULL,
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
    type = "SAR",
    prev_year = NULL,
    custom = FALSE,
    custom_sections = NULL,
    include_figures = TRUE,
    include_tables = TRUE,
    add_image = FALSE,
    spp_image = NULL) {
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
  } else {
    report_name <- paste0(
      "type_"
    )
  }
  # Add region to name
  if (!is.null(region)) {
    report_name <- paste0(
      report_name,
      gsub("(\\b[A-Z])[^A-Z]+", "\\1", region),
      "_"
    )
  } else {
    report_name <- report_name
  }
  # Add species to name
  if (!is.null(species)) {
    report_name <- paste0(
      report_name,
      gsub(" ", "_", species),
      "_skeleton.qmd"
    )
  } else {
    report_name <- paste0(
      report_name, "species_skeleton.qmd"
    )
  }

  # Select parameter from list
  if (length(format) > 1) {
    format <- "pdf"
  } else {
    format <- match.arg(format, several.ok = FALSE)
  }
  if (!is.null(office) & length(office) == 1) {
    office <- match.arg(office, several.ok = FALSE)
  } else if (length(office) > 1) {
    office <- ""
  }

  if (is.null(office) | office == "") {
    subdir <- fs::path(file_dir,"stock_assessment_reports", "report")
  } else if (!is.null(region)) {
    subdir <- fs::path(file_dir, "stock_assessment_reports", office, species, region, year)
  } else {
    subdir <- fs::path(file_dir, "stock_assessment_reports", office, species, year)
  }

  # Supporting files folder
  supdir <- paste(subdir, "/support_files", sep = "")

  # if (!is.null(region)) {
  if (dir.exists(subdir) == FALSE) {
    dir.create(subdir, recursive = TRUE)
  }
  if (dir.exists(supdir) == FALSE) {
    dir.create(supdir, recursive = FALSE)
  }
  # }

  if (new_template) {
    if (is.null(type) | type == "SAR") {
      # Pull skeleton for sections
      current_folder <- system.file("templates", "skeleton", package = "asar")
      new_folder <- subdir
      files_to_copy <- list.files(current_folder)
      before_body_file <- system.file("resources", "formatting_files", "before-body.tex", package = "asar")
      # header_file <- system.file("resources", "formatting_files", "in-header.tex", package = "asar")
      # format_files <- list(before_body_file, header_file)
      if (add_image) {
        spp_image <- spp_image
      } else {
        spp_image <- system.file("resources", "spp_img", paste(gsub(" ", "_", species), ".png", sep = ""), package = "asar")
      }

      # Check if there are already files in the folder
      if (length(list.files(subdir)) < 2) {
        # copy quarto files
        file.copy(file.path(current_folder, files_to_copy), new_folder, overwrite = FALSE)
        # copy before-body tex
        file.copy(before_body_file, supdir, overwrite = FALSE) |> suppressWarnings()
        # customize titlepage tex
        create_titlepage_tex(office = office, subdir = supdir)
        # customize in-header tex
        create_inheader_tex(species = species, year = year, subdir = supdir)
        # Copy species image from package
        file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
        # Copy us doc logo
        file.copy(system.file("resources", "us_doc_logo.png", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
      } else {
        warning("There are files in this location.")
        question1 <- readline("The function wants to overwrite the files currently in your directory. Would you like to proceed? (Y/N)")

        if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
          # copy quarto files
          file.copy(file.path(current_folder, files_to_copy), new_folder, overwrite = TRUE) |> suppressWarnings()
          # copy before-body tex
          file.copy(before_body_file, supdir, overwrite = FALSE) |> suppressWarnings()
          # customize titlepage tex
          create_titlepage_tex(office = office, subdir = supdir)
          # customize in-header tex
          create_inheader_tex(species = species, year = year, subdir = supdir)
          # Copy species image from package
          file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
          # Copy us doc logo
          file.copy(system.file("resources", "us_doc_logo.png", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
        } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
          warning("Report template files were not copied into your directory. If you wish to update the template with new parameters or output files, please edit the ", report_name, " in your local folder.")
        }
      }

      # Create tables qmd
      if (include_tables) {
        if (!is.null(resdir) | !is.null(model_results) | !is.null(model)) {
          create_tables_doc(
            resdir = resdir,
            model_results = model_results,
            model = model,
            subdir = subdir
          )
        } else {
          tables_doc <- paste0(
            "### Tables \n \n",
            "Please refer to the `satf` package downloaded from remotes::install_github('nmfs-ost/satf') to add premade tables."
          )
          utils::capture.output(cat(tables_doc), file = fs::path(subdir, "tables.qmd"), append = FALSE)
          warning("Results file or model name not defined.")
        }
      } else {
        tables_doc <- paste0(
          "### Tables \n \n",
          "Please refer to the `satf` package downloaded from remotes::install_github('nmfs-ost/satf') to add premade figures"
        )
        utils::capture.output(cat(tables_doc), file = fs::path(subdir, "tables.qmd"), append = FALSE)
      }
      # Create figures qmd
      if (include_figures) {
        if (!is.null(resdir) | !is.null(model_results) | !is.null(model)) {
          create_figures_doc(
            resdir = resdir,
            model_results = model_results,
            model = model,
            subdir = subdir,
            year = year
          )
        } else {
          figures_doc <- paste0("## Figures \n")
          utils::capture.output(cat(figures_doc), file = fs::path(subdir, "figures.qmd"), append = FALSE)
          warning("Results file or model name not defined.")
        }
      } else {
        figures_doc <- paste0("## Figures \n")
        utils::capture.output(cat(figures_doc), file = fs::path(subdir, "figures.qmd"), append = FALSE)
      }

      # Part I
      # Create a report template file to render for the region and species
      # Create YAML header for document
      # Write title based on report type and region
      if (alt_title == FALSE) {
        title <- create_title(office = office, species = species, spp_latin = spp_latin, region = region, type = type, year = year)
      } else if (alt_title == TRUE) {
        if (!exists(title)) {
          stop("Alternate title not defined. Please define an alternative title in the parameter 'title'.")
        } else {
          title <- paste(alt_title)
        }
      }

      # Pull authors and affiliations from national db
      # Parameters to add authorship to YAML
      # Read authorship file
      authors <- utils::read.csv(system.file("resources", "authorship.csv", package = "asar", mustWork = TRUE)) |>
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

      if (include_affiliation) {
        affil <- utils::read.csv(system.file("resources", "affiliation_info.csv", package = "asar", mustWork = TRUE))
      }
      if (!is.null(add_author)) {
        authors <- rbind(authors, data.frame(name = add_author, office = rep(NA, length(add_author))))
      }

      author_list <- list()
      if (include_affiliation == TRUE & simple_affiliation == FALSE) {
        if (nrow(authors) > 0) {
          for (i in 1:nrow(authors)) {
            auth <- authors[i, ]
            aff <- affil |>
              dplyr::filter(affiliation == auth$office)
            if (is.na(auth$office)) {
              paste0(
                "  ", "- name: ", "'", auth$name, "'", "\n",
                "  ", "  ", "affiliations: ", "NO AFFILIATION", "\n"
              ) -> author_list[[i]]
            } else {
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
          }
        } else {
          paste0(
            "  ", "- name: ", "'FIRST LAST'", "\n",
            "  ", "  ", "affiliations: \n",
            "  ", "  ", "  ", "- name: 'NOAA Fisheries' \n",
            "  ", "  ", "  ", "  ", "address: 'ADDRESS' \n",
            "  ", "  ", "  ", "  ", "city: 'CITY' \n",
            "  ", "  ", "  ", "  ", "state: 'STATE' \n",
            "  ", "  ", "  ", "  ", "postal-code: 'POSTAL CODE' \n"
            # sep = " "
          ) -> author_list[[1]]
        }
      } else if (include_affiliation & simple_affiliation) {
        if (nrow(authors) > 0) {
          for (i in 1:nrow(authors)) {
            auth <- authors[i, ]
            aff <- affil |>
              dplyr::filter(affiliation == auth$office)
            if (!is.na(auth$office)) {
              paste0(
                "  ", "- name: ", "'", auth$name, "'", "\n",
                "  ", "  ", "affiliations: ", "'", aff$name, "'", "\n"
              ) -> author_list[[i]]
            } else {
              paste0(
                "  ", "- name: ", "'", auth$name, "'", "\n",
                "  ", "  ", "affiliations: ", "'NO AFFILIATION'", "\n"
              ) -> author_list[[i]]
            }
          }
        } else {
          paste0(
            "  ", "- name: ", "'FIRST LAST'", "\n",
            "  ", "  ", "affiliations: ", "NO AFFILIATION", "\n"
          ) -> author_list[[1]]
        }
      } else {
        if (nrow(authors) > 0) {
          for (i in 1:nrow(authors)) {
            auth <- authors[i, ]
            paste0("  ", "- ", "'", auth$name, "'", "\n") -> author_list[[i]]
          }
        } else {
          paste0("  ", "- 'FIRST LAST' \n") -> author_list[[1]]
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

      # Add species image on title page
      if (add_image) {
        # extract image name
        new_img <- sapply(strsplit(spp_image, "/"), utils::tail, 1)

        yaml <- paste0(
          yaml,
          # image as pulled in from above
          "cover: ", new_img, "\n"
        )
      } else if (spp_image == "") {
        yaml <- paste0(
          yaml,
          # image as pulled in from above
          "cover: ", spp_image, "\n"
        )
      } else {
        yaml <- paste0(
          yaml,
          # image as pulled in from above
          "cover: support_files/", gsub(" ", "_", species), ".png", "\n"
        )
      }

      # Formatting
      yaml <- paste0(
        yaml,
        format_quarto(format = format)
      )

      # Add lua filters for compliance
      # PLACEHOLDER: Uncomment once .lua text is built

      # yaml <- paste0(yaml,
      #                # "contributes:", "\n",
      #                "filters:", "\n",
      #                "  ", "  ", "- acronyms.lua", "\n",
      #                "  ", "  ", "- accessibility.lua", "\n")

      # Parameters
      # office, region, and species are default parameters
      if (parameters) {
        yaml <- paste0(
          yaml, "params:", "\n",
          "  ", " ", "office: ", "'", office, "'", "\n",
          "  ", " ", "species: ", "'", species, "'", "\n",
          "  ", " ", "spp_latin: ", "'", spp_latin, "'", "\n"
        )
        if (!is.null(region)) {
          yaml <- paste0(yaml, "  ", " ", "region: ", "'", region, "'", "\n")
        }
        # Add more parameters if indicated
        if (!is.null(param_names) & !is.null(param_values)) {
          # check there are the same number of names and values
          if (length(param_names) != length(param_values)) {
            print("Please define ALL parameter names (param_names) and values (param_values).")
          } else {
            add_params <- NULL
            for (i in 1:length(param_names)) {
              toad <- paste("  ", " ", param_names[i], ": ", "'", param_values[i], "'", "\n", sep = "")
              add_params <- paste0(add_params, toad)
            } # close loop
          } # close check
          yaml <- paste0(yaml, add_params)
        } # close if adding add'l params
      } # close if params to be included in template

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
      ass_output <- add_chunk(
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
      citation <- create_citation(
        author = author,
        title = title,
        year = year,
        office = office
      )

      print("_______Add Report Citation________")

      # Create report template

      if (custom == FALSE) {
        sections <- add_child(
          c(
            "executive_summary.qmd",
            "introduction.qmd",
            "data.qmd",
            "modeling_approach.qmd",
            "results.qmd",
            "projections.qmd",
            "discussion.qmd",
            "acknowledgments.qmd",
            "references.qmd",
            "tables.qmd",
            "figures.qmd",
            "appendix.qmd"
          ),
          label = c(
            "executive_summary",
            "introduction",
            "data",
            "modeling_approach",
            "results",
            "projections",
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
          sections <- add_child(section_list,
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
              "modeling_approach.qmd",
              "results.qmd",
              "projections.qmd",
              "discussion.qmd",
              "acknowledgments.qmd",
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
            sections <- add_child(
              sec_list2,
              label = gsub(".qmd", "", unlist(sec_list2))
            )
          } else { # custom_sections explicit

            # Add selected sections from base
            sec_list1 <- add_base_section(custom_sections)
            # Create new sections as .qmd in folder
            # check if sections are in custom_sections list
            if(any(stringr::str_replace(section_location,"^[a-z]+-","") %notin% custom_sections)) {
              stop("Defined customizations do not match one or all of the relative placement of a new section. Please review inputs.")
            }
            if(include_tables){
              sec_list1 <- c(sec_list1, "tables.qmd")
            }
            if(include_figures){
              sec_list1 <- c(sec_list1, "figures.qmd")
            }
            sec_list2 <- add_section(
              sec_names = new_section,
              location = section_location,
              other_sections = sec_list1,
              subdir = subdir
            )
            # Create sections object to add into template
            sections <- add_child(
              sec_list2,
              label = gsub(".qmd", "", unlist(sec_list2))
            )
          } # close if statement for very specific sectioning
        } # close if statement for extra custom
      } # close if statement for custom

      # Combine template sections
      report_template <- paste(yaml,
        ass_output,
        citation,
        sections,
        sep = "\n"
      )

      print("___Created report template______")

      ######## |###############################################################
      ##### NEFSC MT Template####
      ######## |###############################################################
    } else if (type == "NEMT") {
      # Pull skeleton for sections
      current_folder <- system.file("templates", "NEMT", package = "asar")
      new_folder <- subdir
      files_to_copy <- list.files(current_folder)

      # Check if there are already files in the folder
      if (length(list.files(subdir)) > 0) {
        warning("There are files in this location.")
        question1 <- readline("The function wants to overwrite the files currently in your directory. Would you like to proceed? (Y/N)")

        if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
          file.copy(file.path(current_folder, files_to_copy), new_folder, overwrite = FALSE) |> suppressWarnings()
        } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
          message("Blank files for template sections were not copied into your directory. If you wish to update the template with new parameters or output files, please edit the ", report_name, " in your local folder.")
        }
      } else if (length(list.files(subdir)) == 0) {
        file.copy(file.path(current_folder, files_to_copy), new_folder, overwrite = FALSE)
      } else {
        stop("None of the arugments match statement commands. Needs developer fix.")
      }

      # Part I
      # Create a report template file to render for the region and species
      # Create YAML header for document
      # yaml <- write_yaml(office = "NEFSC")

      print("__________Built YAML Header______________")
      # yaml_save <- capture.output(cat(yaml))
      # cat(yaml, file = here('template','yaml_header.qmd'))

      # Add chunk to load in assessment data
      ass_output <- add_chunk(
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
      citation <- create_citation(
        author = author,
        title = title,
        year = year,
        office = office
      )

      print("_______Add Report Citation________")

      # Create report template

      if (custom == FALSE) {
        sections <- add_child(
          c(
            "executive_summary.qmd",
            "introduction.qmd",
            "data.qmd",
            "modeling_approach.qmd",
            "results.qmd",
            "projections.qmd",
            "discussion.qmd",
            "acknowledgments.qmd",
            "references.qmd",
            "tables.qmd",
            "figures.qmd",
            "appendix.qmd"
          ),
          label = c(
            "executive_summary",
            "introduction",
            "data",
            "modeling_approach",
            "results",
            "projections",
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
          sections <- add_child(section_list,
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
              "modeling_approach.qmd",
              "results.qmd",
              "projections.qmd",
              "discussion.qmd",
              "acknowledgments.qmd",
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
            sections <- add_child(
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
            sections <- add_child(
              sec_list2,
              label = gsub(".qmd", "", unlist(sec_list2))
            )
          }
        }
      }

      # Combine template sections
      report_template <- paste( # yaml,
        ass_output,
        citation,
        sections,
        sep = "\n"
      )

      print("___Created report template______")
    }

    # Save template as .qmd to render
    utils::capture.output(cat(report_template), file = file.path(subdir, report_name), append = FALSE)
    # Print message
    message(
      "Saved report template in directory: ", subdir, "\n",
      "To proceeed, please edit sections within the report template in order to produce a completed stock assessment report."
    )
    # Open file for analyst
    file.show(file.path(subdir, report_name)) # this opens the new file, but also restarts the session
    # Open the file so path to other docs is clear
    # utils::browseURL(subdir)
  } else {
    # Copy old template and rename for new year
    # Create copy of previous assessment
    if (!is.null(region)) {
      olddir <- fs::path(file_dir, "stock_assessment_reports",office, species, region, prev_year)
      invisible(file.copy(file.path(olddir, list.files(olddir)), subdir, recursive = FALSE))
    } else {
      olddir <- fs::path(file_dir, "stock_assessment_reports", office, species, prev_year)
      invisible(file.copy(file.path(olddir, list.files(olddir)), subdir, recursive = FALSE))
    }

    # Edit skeleton to update year and results file
    skeleton <- list.files(subdir, pattern = "skeleton.qmd")
    # Open previous skeleton
    file.show(file.path(subdir, report_name))

    svDialogs::dlg_message("Reminder: there are changes to be made when calling an old report. Please change the year in the citation and the location and name of the results file in the first chunk of the report.",
      type = "ok"
    )
  }
}
