#' Create Stock Assessment Report Template
#'
#' To see templates included in the base skeleton, please run
#' 'list.files(system.file('templates','skeleton', package = 'asar'))'
#'  in the console.
#'
#' @param format Rendering format (pdf, html, or docx).
#' @param type Type of report to build. Default is SAR (a NOAA standard "Stock
#' Assessment Report").
#' @param office Regional Fisheries Science Center producing the
#'  report (i.e., AFSC, NEFSC, NWFSC, PIFSC, SEFSC, SWFSC).
#' @param region Full name of region in which the species is
#'  evaluated (if applicable). If the region is not specified for
#'   your center or species, do not use this variable.
#' @param species Full common name for target species. Split
#' naming with a space and capitalize first letter(s). Example:
#' "Dover sole".
#' @param spp_latin Latin name for the target species. Example:
#' "Pomatomus saltatrix".
#' @param year Year the assessment is being conducted. Default
#' is the year in which the report is rendered.
#' @param author A character vector of author names with their accompanying
#' affiliations. For example, a Jane Doe at the NWFSC Seattle, Washington office
#' would have an entry of c("Jane Doe"="NWFSC-SWA"). Information on NOAA offices
#' is found in a database located in the package: \code{system.file("resources",
#' "affiliation_info.csv", package = "asar")}. Keys to the office addresses
#' follow the naming convention of the office acronym (ex. NWFSC) with a dash
#' followed by the first initial of the city then the 2 letter abbreviation for
#' the state the office is located in. If the city has 2 or more words such as
#' Panama City, the first initial of each word is used in the key
#' (ex. Panama City, Florida = PCFL)
#' @param file_dir Location of stock assessment files produced
#' by this function. Default is the working directory.
#' @param title A custom title that is an alternative to the default title (composed
#' in asar::create_title()). Example: "Management Track Assessments Spring 2024".
#' @param model_results The name of the object in your environment that contains the data frame of converted model output from `asar::convert_output()`
#' @param spp_image File path to the species' image if not
#' using the image included in the project's repository.
#' @param bib_file File path to a .bib file used for citing references in
#' the report
#' @param new_template TRUE/FALSE; Create a new template? If true,
#' will pull the last saved stock assessment report skeleton.
#' Default is false.
#' @param rerender_skeleton Re-create the "skeleton.qmd" in your outline when
#'        changes to the main skeleton need to be made. This reproduces the
#'        yaml, output (if changed), preamble quantities, and restructures your
#'        sectioning in the skeleton if indicated. All files in your folder
#'        will remain as is.
#' @param custom TRUE/FALSE; Build custom sectioning for the
#' template, rather than the default for stock assessments in
#' your region? Default is false.
#' @param custom_sections List of existing sections to include in
#' the custom template. Note: this only includes sections within
#'  list.files(system.file("templates", "skeleton",
#'  package = "asar")). The name of the section, rather than the
#'  name of the file, can be used (e.g., 'abstract' rather than
#'  '00_abstract.qmd'). If adding a new section, also use
#'   parameters 'new_section' and 'section_location'.
#' @param new_section Names of section(s) (e.g., introduction, results) or
#' subsection(s) (e.g., a section within the introduction) that will be
#' added to the document. Please make a short list if >1 section/subsection
#' will be added. The template will be created as a quarto document, added
#' into the skeleton, and saved for reference.
#' @param section_location Where new section(s)/subsection(s) will be added to
#' the skeleton template. Please use the notation of 'placement-section'.
#' For example, 'in-introduction' signifies that the new content would
#' be created as a child document and added into the 02_introduction.qmd.
#' To add >1 (sub)section, make the location a list corresponding to the
#' order of (sub)section names listed in the 'new_section' parameter.
#' @param parameters TRUE/FALSE; For
#' parameterization of the script. Default is true.
#' @param param_names List of parameter names that will be called
#'  in the document. Parameters automatically included:
#'  office, region, species (each of which are listed as
#'  individual parameters for this function, above).
#' @param param_values List of values associated with the order
#'  of parameter names. Parameters automatically included:
#'  office, region, species (each of which are listed as
#'  individual parameters for this function, above).
#' @param type Type of report to build. Default is SAR (NOAA Fisheries
#' Stock Assessment Report).
#' @param custom TRUE/FALSE; Build custom sectioning for the
#' template, rather than the default for stock assessments in
#' your region? Default is false.
#' @param custom_sections List of existing sections to include in
#' the custom template. Note: this only includes sections within
#'  list.files(system.file("templates", "skeleton",
#'  package = "asar")). The name of the section, rather than the
#'  name of the file, can be used (e.g., 'abstract' rather than
#'  '00_abstract.qmd'). If adding a new section, also use
#'   parameters 'new_section' and 'section_location'.
#' @param spp_image File path to the species' image if not
#' using the image included in the project's repository.
#' @param bib_file File path to a .bib file used for citing references in
#' the report
#' @param rerender_skeleton Re-create the "skeleton.qmd" in your outline when
#'        changes to the main skeleton need to be made. This reproduces the
#'        yaml, output (if changed), preamble quantities, and restructures your
#'        sectioning in the skeleton if indicated. All files in your folder
#'        will remain as is.
#' @param ... Additional arguments passed into functions used in create_template
#' such as `create_citation()`, `format_quarto()`, `add_chunk()`, ect
#'
#' @return Create template and pull skeleton for a stock assessment report.
#'         Function builds a YAML specific to the region and utilizes current
#'         resources and workflows from different U.S. Fishery Science Centers.
#'         General sections are called as child documents in this skeleton and
#'         each of the child documents should be edited separately.
#' @export
#'
#' @examples
#' \dontrun{
#' create_template(
#'   new_section = "a_new_section",
#'   section_location = "before-introduction"
#' )
#'
#'
#' create_template(
#'   new_template = TRUE,
#'   format = "pdf",
#'   office = "NWFSC",
#'   species = "Dover sole",
#'   spp_latin = "Microstomus pacificus",
#'   year = 2010,
#'   author = c("John Snow" = "AFSC",
#'              "Danny Phantom" = "NEFSC",
#'              "Patrick Star" = "SEFSC-ML"),
#'   model_results = dover_sole_output,
#'   new_section = "an_additional_section",
#'   section_location = "after-introduction"
#' )
#'
#' asar::create_template(
#'   new_template = TRUE,
#'   format = "pdf",
#'   office = "PIFSC",
#'   species = "Striped marlin",
#'   spp_latin = "Kajikia audax",
#'   year = 2018,
#'   author = c("John Snow" = "AFSC"),
#'   new_section = c("a_new_section", "another_new_section"),
#'   section_location = c("before-introduction", "after-introduction"),
#'   custom = TRUE,
#'   custom_sections = c("executive_summary", "introduction")
#' )
#'
#' create_template(
#'   new_template = TRUE,
#'   format = "pdf",
#'   office = "NWFSC",
#'   region = "my_region",
#'   species = "Bluefish",
#'   spp_latin = "Pomatomus saltatrix",
#'   year = 2010,
#'   author = c("John Snow", "Danny Phantom", "Patrick Star"),
#'   title = "Management Track Assessments Spring 2024",
#'   parameters = TRUE,
#'   param_names = c("region", "year"),
#'   param_values = c("my_region", "2024"),
#'   model_results = bluefish_output,
#'   new_section = "an_additional_section",
#'   section_location = "before-discussion",
#'   type = "SAR",
#'   custom = TRUE,
#'   custom_sections = c("executive_summary", "introduction", "discussion"),
#'   spp_image = "dir/containing/spp_image"
#' )
#' }
#'
create_template <- function(
    format = "pdf",
    type = "SAR",
    office = c("AFSC", "PIFSC", "NEFSC", "NWFSC", "SEFSC", "SWFSC"),
    region = NULL,
    species = "species",
    spp_latin = NULL,
    year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y"),
    author = NULL,
    file_dir = getwd(),
    title = "[TITLE]",
    model_results = NULL,
    spp_image = "",
    bib_file = "asar_references.bib",
    new_template = TRUE,
    rerender_skeleton = FALSE,
    custom = FALSE,
    custom_sections = NULL,
    new_section = NULL,
    section_location = NULL,
    parameters = TRUE,
    param_names = NULL,
    param_values = NULL,
    ...) {

  # Check input type
  if (interactive()) {
    type <- switch(
      type,
      "Northeast Management Track" = "nemt",
      "Pacific Fishery Management Council" = "pfmc",
      "Stock Assessment and Fishery Evaluation" = "safe",
      "Stock Assessment Report" = "skeleton",
      "SAR" = "skeleton",
      "pfmc" = "pfmc",
      "nemt" = "nemt",
      "safe" = "safe",
      {
        type_fxn <- function() {
          selection <- utils::menu(
            title = "Unrecognized template type. Please select an option below: ",
            choices = c("Default", "PFMC", "NEMT", "SAFE")
          )
          type <- switch (
            as.character(selection),
            "1" = "skeleton",
            "2" = "pfmc",
            "3" = "nemt",
            "4" = "safe",
            {
              "skeleton"
            }
          )
          return(type)
        }
        type_fxn()
      }
    )
  } else {
    type <- "skeleton"
  }

  if (!is.null(office) & length(office) == 1) {
    office <- match.arg(office, several.ok = FALSE)
  } else if (length(office) > 1) {
    office <- ""
  }

  #### Rerender skeleton ----
  if (rerender_skeleton) {
    # TODO: set up situation where species, region can be changed
    report_name <- list.files(file_dir, pattern = "skeleton.qmd") # gsub(".qmd", "", list.files(file_dir, pattern = "skeleton.qmd"))
    if (length(report_name) == 0) cli::cli_abort("No skeleton quarto file found in the `file_dir` ({file_dir}).")
    if (length(report_name) > 1) cli::cli_abort("Multiple skeleton quarto files found in the `file_dir` ({file_dir}).")

    prev_report_name <- gsub("_skeleton.qmd", "", report_name)
    # Extract type
    type <- stringr::str_extract(prev_report_name, "^[A-Z]+")
    # Extract region
    region <- stringr::str_extract(prev_report_name, "(?<=_)[A-Z]+(?=_)")
    # report name without type and region
    report_name_1 <- gsub(glue::glue("{type}_"), "", prev_report_name)
    # Extract species
    species <- gsub("_",
                    " ",
                    gsub(glue::glue("{region}_"), "", report_name_1)
    )


    new_report_name <- paste0(
      type, "_",
      ifelse(is.null(region), "", paste(gsub("(\\b[A-Z])[^A-Z]+", "\\1", region), "_", sep = "")),
      ifelse(is.null(species), "species", stringr::str_replace_all(species, " ", "_")), "_",
      "skeleton.qmd"
    )
  } else {
    # Name report
    if (!is.null(type)) {
      report_name <- paste0(
        ifelse(type == "skeleton", "SAR", type),
        "_"
      )
    } else {
      report_name <- paste0(
        "type_"
      )
    }
    # Add region to name
    report_name <- ifelse(
      !is.null(region),
      paste0(
        report_name,
        gsub("(\\b[A-Z])[^A-Z]+", "\\1", region),
        "_"
      ),
      report_name
    )
    # Add species to name
    # TODO: can this be made into a switch?
    # report_name <- switch(
    #   species,
    #
    # )
    # if (!is.null(species)) {
      report_name <- paste0(
        report_name,
        gsub(" ", "_", species),
        "_skeleton.qmd"
      )
    # } else {
    #   report_name <- paste0(
    #     report_name, "species_skeleton.qmd"
    #   )
    # }
  } # close if rerender skeleton for naming

  # Select format
  # if (length(format) > 1) {
  #   format <- "pdf"
  # } else {
  #   format <- match.arg(format, several.ok = FALSE)
  # }
  if (grepl("^pdf$|^html$", tolower(format))) {
    format <- tolower(format)
  } else if (grepl("docx", tolower(format))) {
    cli::cli_alert_warning("The docx format is not currently supported by asar. Defaulting to pdf.",
                           wrap = TRUE)
    format <- "pdf"
  } else {
    cli::cli_alert("Format not compatible.")
    cli::cli_alert_info("You entered `format` = {format}")
    if (grepl("pdf", format)) {
      question1 <- readline("Did you mean `format` = 'pdf'? (y/n)")
      if (!interactive()) question1 <- "y"
      if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
        format <- "pdf"
      } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
        cli::cli_abort("Template processing stopped.")
      }
    } else if (grepl("html", format)) {
      question1 <- readline("Did you mean `format` = 'html'? (y/n)")
      if (!interactive()) question1 <- "y"
      if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
        format <- "html"
      } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
        cli::cli_abort("Template processing stopped.")
      }
    } else if (grepl("docx", format)) {
      question1 <- readline("Did you mean `format` = 'docx'? (y/n)")
      if (!interactive()) question1 <- "y"
      if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
        cli::cli_alert_warning("The docx format is not currently supported by asar. Defaulting to pdf.",
                               wrap = TRUE)
        format <- "pdf"
      } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
        cli::cli_abort("Template processing stopped.")
      }
    } else {
      cli::cli_abort("Format not recognized. Please use pdf, html, or docx.")
    }
  }

  # TODO: add switch here instead of if
  # if (!is.null(office) & length(office) == 1) {
  #   office <- match.arg(office, several.ok = FALSE)
  # } else if (length(office) > 1) {
  #   office <- ""
  # }

  # Create subdirectory for files
  subdir <- ifelse(
    grepl("/report", file_dir),
    fs::path(file_dir),
    fs::path(file_dir, "report")
  )

  # Supporting files folder
  supdir <- file.path(subdir, "support_files")

  # if (!is.null(region)) {
  if (dir.exists(subdir) == FALSE) {
    dir.create(subdir, recursive = TRUE)
  }
  if (dir.exists(supdir) == FALSE) {
    dir.create(supdir, recursive = FALSE)
  }
  # }

  #### New template ----
  if (new_template) {
    ##### Pull sections based on type ----
    # Pull skeleton for sections

    asar_folder <- system.file("templates", package = "asar")
    # copy files from specific type folder
    current_folder <- file.path(asar_folder, type)
    new_folder <- subdir

    ##### Identify files to copy ----
    if (!is.null(custom_sections)) {
      files_to_copy <- unlist(list.files(current_folder))[c(unlist(sapply(custom_sections, function(x) grep(x, list.files(current_folder)))))]
      # add acknowledgments sections if not selected manually
      if (!any(grepl("acknowledgments", files_to_copy))) {
        files_to_copy <- c(files_to_copy, unlist(list.files(current_folder))[10])
        custom_sections <- c(custom_sections, "acknowledgments")
      }
      # add references sections if not selected manually
      if (!any(grepl("references", files_to_copy))) {
        files_to_copy <- c(files_to_copy, unlist(list.files(current_folder))[11])
        custom_sections <- c(custom_sections, "references")
      }
    } else {
      files_to_copy <- list.files(current_folder)
    }

    before_body_file <- system.file("resources", "formatting_files", "before-body.tex", package = "asar")
    # header_file <- system.file("resources", "formatting_files", "in-header.tex", package = "asar")
    # format_files <- list(before_body_file, header_file)

    #### Links to files for yaml ----
    spp_image <- ifelse(spp_image == "",
                        system.file("resources", "spp_img", paste(gsub(" ", "_", species), ".png", sep = ""), package = "asar"),
                        spp_image
    )

    # Add bib file
    if (bib_file == "asar_references.bib") {
      bib_loc <- system.file("resources", "asar_references.bib", package = "asar")
      bib_name <- bib_file
    } else {
      # check if enter file exists
      # if (!file.exists(bib_file)) stop(".bib file not found.")
      cli::cli_alert_warning("Bibliography file {bib_file} not in the report directory.")
      cli::cli_alert_info("The file will not be read in on render if not in the same path as the skeleton file.")

      bib_loc <- bib_file # dirname(bib_file)
      bib_name <- stringr::str_extract(bib_file, "[^/]+$") # utils::tail(stringr::str_split(bib_file, "/")[[1]], n = 1)
    }

    #### Read in previous skeleton if rerender ----
    # Check if this is a rerender of the skeleton file
    if (rerender_skeleton) {
      # read format in skeleton & check if format is identified in the rerender call
      if (!file.exists(file.path(file_dir, list.files(file_dir, pattern = "skeleton.qmd")))) stop("No skeleton quarto file found in the working directory.")
      prev_skeleton <- readLines(file.path(file_dir, list.files(file_dir, pattern = "skeleton.qmd")))
      # extract previous format
      prev_format <- stringr::str_extract(
        prev_skeleton[grep("format:", prev_skeleton) + 1],
        "[a-z]+"
      )
      year <- as.numeric(stringr::str_extract(
        prev_skeleton[grep("title:", prev_skeleton)],
        "[0-9]+"
      ))
      # Add in species image if updated in rerender
      if (species != "species") {
        file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
      }
      # if it is previously html and the rerender species html then need to copy over html formatting
      if (tolower(prev_format) != "html" & tolower(format) == "html") {
        if (!file.exists(file.path(file_dir, "support_files", "theme.scss"))) file.copy(system.file("resources", "formatting_files", "theme.scss", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
      }
      if (tolower(prev_format != "pdf" & tolower(format) == "pdf")) {
        if (is.null(species)) {
          species <- tolower(stringr::str_extract(
            prev_skeleton[grep("species: ", prev_skeleton)],
            "(?<=')[^']+(?=')"
          ))
        }
        if (is.null(office)) {
          office <- stringr::str_extract(
            prev_skeleton[grep("office: ", prev_skeleton)],
            "(?<=')[^']+(?=')"
          )
        }
        # year - default to current year
        cli::cli_alert_warning("Undefined year.")
        cli::cli_alert_info("Please identify year in your arguments or manually change it in the skeleton if value is incorrect.",
                            wrap = TRUE)
        # copy before-body tex
        if (!file.exists(file_dir, "support_files", "before-body.tex")) file.copy(before_body_file, supdir, overwrite = FALSE) |> suppressWarnings()
        # customize titlepage tex
        if (!file.exists(file_dir, "support_files", "_titlepage.tex") | !is.null(species)) create_titlepage_tex(office = office, subdir = supdir, species = species)
        # customize in-header tex
        if (!file.exists(file_dir, "support_files", "in-header.tex") | !is.null(species)) create_inheader_tex(species = species, year = year, subdir = supdir)
        # copy new spp image if updated
        if (!is.null(species)) file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
      }
    } else {
      #### Copy template files to report folder ----
      # Check if there are already files in the folder
      if (length(list.files(subdir)) < 2) {
        # copy quarto files
        file.copy(file.path(current_folder, files_to_copy), new_folder, overwrite = FALSE)
        # copy before-body tex
        file.copy(before_body_file, supdir, overwrite = FALSE) |> suppressWarnings()
        # customize titlepage tex
        create_titlepage_tex(office = office, subdir = supdir, species = species)
        # customize in-header tex
        create_inheader_tex(species = species, year = year, subdir = supdir)
        # Copy species image from package
        file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
        # Copy bib file
        file.copy(bib_loc, subdir, overwrite = TRUE) |> suppressWarnings()
        # Copy us doc logo
        file.copy(system.file("resources", "us_doc_logo.png", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
        # Copy glossary
        file.copy(system.file("glossary", "report_glossary.tex", package = "asar"), subdir, overwrite = FALSE) |> suppressWarnings()
        # Copy html format file if applicable
        if (tolower(format) == "html") file.copy(system.file("resources", "formatting_files", "theme.scss", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
        # Copy over glossary and associated tex file
        if (tolower(type) == "pfmc") {
          file.copy(system.file("resources", "formatting_files", "sa4ss_glossaries.tex", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
          file.copy(system.file("resources", "formatting_files", "pfmc.tex", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
        }

        # show message and make README stating model_results info
        # Message is deprecated because model results are loaded from the environment rather than a file
        # if (!is.null(model_results)){
        #   # if resdir = null, change it to getwd() so mod_time can execute file.info()
        #   if (is.null(resdir)){
        #     resdir <- getwd()
        #     resdir_null = TRUE
        #   } else {
        #     resdir_null = FALSE
        #   }
        #   mod_time <- as.character(file.info(file.path(resdir, model_results), extra_cols = F)$ctime)
        #   mod_msg <- paste("Report is based upon model output from", model_results, "stored in folder", resdir,
        #                    "that was last modified on:", mod_time)
        #   message(mod_msg)
        #   writeLines(mod_msg, fs::path(subdir, "model_results_metadata.md"))
        #   # change resdir back to null if originally null
        #   if(resdir_null == TRUE){
        #     resdir <- NULL
        #   }
        # }
      } else {
        cli::cli_alert_warning("There are files in this location.")
        question1 <- readline("The function wants to overwrite the files currently in your directory. Would you like to proceed? (Y/N)")

        # answer question1 as y if session isn't interactive
          if (!interactive()){
            question1 <- "y"
          }

          if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
            # remove old skeleton if present
            if (any(grepl("_skeleton.qmd", list.files(subdir)))) {
              file.remove(file.path(subdir, (list.files(subdir)[grep("_skeleton.qmd", list.files(subdir))])))
            }
            # copy quarto files
            file.copy(file.path(current_folder, files_to_copy), new_folder, overwrite = TRUE) |> suppressWarnings()
            # copy before-body tex
            file.copy(before_body_file, supdir, overwrite = FALSE) |> suppressWarnings()
            # customize titlepage tex
            create_titlepage_tex(office = office, subdir = supdir, species = species)
            # customize in-header tex
            create_inheader_tex(species = species, year = year, subdir = supdir)
            # Copy species image from package
            file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
            # Copy bib file
            file.copy(bib_loc, subdir, overwrite = TRUE) |> suppressWarnings()
            # Copy us doc logo
            file.copy(system.file("resources", "us_doc_logo.png", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
            # Copy glossary
            file.copy(system.file("glossary", "report_glossary.tex", package = "asar"), subdir, overwrite = FALSE) |> suppressWarnings()
            # Copy html format file if applicable
            if (tolower(format) == "html") file.copy(system.file("resources", "formatting_files", "theme.scss", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
          } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
            cli::cli_alert_warning("Report template files were not copied into your directory.")
            cli::cli_alert_info("If you wish to update the template with new parameters or output files, please edit the {report_name} in your local folder.",
                                wrap = TRUE)
          }
        } # close check for previous files & respective copying
        prev_skeleton <- NULL
      } # close if rerender

      # created tables doc
      if (!rerender_skeleton) {
        tables_doc <- paste0(
          "# Tables \n \n",
          "Please refer to the `stockplotr` package downloaded from remotes::install_github('nmfs-ost/stockplotr') to add premade tables."
        )
        utils::capture.output(cat(tables_doc), file = fs::path(subdir, "08_tables.qmd"), append = FALSE)
      }

      # Create figures qmd
      if (!rerender_skeleton) {
        figures_doc <- paste0(
          "# Figures \n \n",
          "Please refer to the `stockplotr` package downloaded from remotes::install_github('nmfs-ost/stockplotr') to add premade figures."
        )
        utils::capture.output(cat(figures_doc), file = fs::path(subdir, "09_figures.qmd"), append = FALSE)
      }

      # Part I
      # Create a report template file to render for the region and species
      # Create YAML header for document
      # Write title based on report type and region
      if (title == "[TITLE]") {
        # TODO: update below so title gets updated if new input is added such as region/species/office
        if (rerender_skeleton) {
          title <- sub("title: ", "", prev_skeleton[grep("title:", prev_skeleton)])
          if (title == "'Stock Assessment Report Template' " & (!is.null(office) | !is.null(species) | !is.null(region))) {
            title <- create_title(
              office = office,
              species = species,
              spp_latin = spp_latin,
              region = region,
              type = type,
              year = year
            )
          }
        } else {
          title <- create_title(
            office = office,
            species = species,
            spp_latin = spp_latin,
            region = region,
            type = type,
            year = year)
        }
      }

      # Authors and affiliations
      # Parameters to add authorship to YAML
      author_list <- add_authors(
        prev_skeleton = ifelse(rerender_skeleton, prev_skeleton, NULL),
        author = author, # need to put this in case there is a rerender otherwise it would not use the correct argument
        rerender_skeleton = rerender_skeleton
      )

      # Create yaml
      yaml <- create_yaml(
        prev_format = prev_format,
        format = format,
        prev_skeleton = prev_skeleton,
        author_list = author_list,
        title = title,
        rerender_skeleton = rerender_skeleton,
        office = office,
        spp_image = spp_image,
        species = species,
        spp_latin = spp_latin,
        region = region,
        parameters = parameters,
        param_names = param_names,
        param_values = param_values,
        bib_name = bib_name,
        bib_file = bib_file,
        year = year,
        type = type
      )

      if (!rerender_skeleton) cli::cli_alert_success("Built YAML header.")

      ##### Params chunk ----
      params_chunk <- add_chunk(
        paste0(
          "# Parameters \n",
          "spp <- params$species \n",
          "SPP <- params$spp \n",
          "species <- params$species \n",
          "spp_latin <- params$spp_latin \n",
          "office <- params$office",
          if (!is.null(param_names)) {
            paste0("\n",
            paste0(param_names, " <- ", "params$", param_names, collapse = " \n")
            )
          }
        ),
        label = "doc_parameters"
      )
      
      ##### Preamble ----
      # Add preamble
      # add in quantities and output data R chunk
      # Reassign model_results as output and save into environment for user
      # assign("output", model_results, envir = .GlobalEnv)
      df_name <- deparse(substitute(model_results))

      # standard preamble
      preamble <- add_chunk(
        paste0(
          # "# load converted output from asar::convert_output() \n",
          # "output <- utils::read.csv('",
          # TODO: replace resdir with substitute object; was removed as arg
          # paste0(resdir, "/", model_results),
          # "') \n",
          # "output <- ", df_name, "\n",
          "# Call reference points and quantities below \n",
          "output <- ", df_name, " |> \n",
          "  ", "dplyr::mutate(estimate = as.numeric(estimate), \n",
          "  ", "  ", "uncertainty = as.numeric(uncertainty)) \n",
          # "start_year <- as.numeric(min(output$year, na.rm = TRUE)) \n",
          " start_year <- output |> \n",
          "  ", "dplyr::filter(era == 'time') |> \n",
          "  ", "dplyr::summarise(min_year = min(year)) |> \n",
          "  ", "dplyr::pull(min_year) |> \n",
          "  ",   "as.numeric() \n",
          # change end year in the fxn to ifelse where is.null(year)
          # "end_year <- (output |> \n",
          # "  ", "dplyr::filter(!(year %in% c('Virg', 'Init', 'S/Rcurve', 'INIT')), \n",
          # "  ", "  ", "!is.na(year)) |> \n",
          # "  ", "dplyr::mutate(year = as.numeric(year)) |> \n",
          # "  ", "dplyr::summarize(max_val = max(year)) |> \n",
          # "  ", "dplyr::pull(max_val))-10", "\n",
          "end_year <- output |> \n",
          "  ", "dplyr::filter(era == 'time') |> \n",
          "  ", "dplyr::summarise(max_year = max(year)) |> \n",
          "  ", "dplyr::pull(max_year) |> \n",
          "  ", "as.numeric() \n",
          # is there a better way to identify this?
          # "end_data_year <- end_year - 1", "\n",
          # for quantities - don't want any values that are split by factor
          "# subset output to remove quantities that are split by factor \n",
          "output2 <- output |> \n",
          "  ", "dplyr::filter(is.na(season), \n",
          "  ", "  ", "is.na(fleet), \n",
          "  ", "  ", "is.na(sex), \n",
          "  ", "  ", "is.na(area), \n",
          "  ", "  ", "is.na(growth_pattern), \n",
          "  ", "  ", "is.na(subseason), \n",
          "  ", "  ", "is.na(age))", "\n",
          "# terminal fishing mortality \n",
          "Fend <- output2 |> ", "\n",
          "  ", "dplyr::filter(c(label == 'fishing_mortality' & year == end_year) | c(label == 'terminal_fishing_mortality' & is.na(year))) |>", "\n",
          "  ", "dplyr::pull(estimate) |>", "\n",
          "  ", "unique()", "\n",
          "# fishing mortality at msy \n",
          "# please change target if desired \n",
          "Ftarg <- output2 |>", "\n",
          "  ", "dplyr::filter(grepl('f_target', label) | grepl('f_msy', label) | c(grepl('fishing_mortality_msy', label) & is.na(year))) |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# Terminal year F respective to F target \n",
          "F_Ftarg <- Fend / Ftarg", "\n",
          "# terminal year biomass \n",
          "Bend <- output2 |>", "\n",
          "  ", "dplyr::filter(grepl('mature_biomass', label) | grepl('^biomass$', label),", "\n",
          "  ", "  ", "year == end_year) |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# target biomass (msy) \n",
          "# please change target if desired \n",
          "Btarg <- output2 |>", "\n",
          "  ", "dplyr::filter(c(grepl('biomass', label) & grepl('target', label) & estimate >1) | label == 'biomass_msy') |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# total catch in the last year \n",
          # "total_catch <- output |>", "\n",
          # "  ", "dplyr::filter(grepl('^catch$', label), \n",
          # "  ", "year == end_year,", "\n",
          # "  ", "  ", "is.na(fleet),", "\n",
          # "  ", "  ", "is.na(age),", "\n",
          # "  ", "  ", "is.na(area),", "\n",
          # "  ", "  ", "is.na(growth_pattern)) |>", "\n",
          # "  ", "dplyr::pull(estimate)", "\n",
          "total_catch <- output |> \n",
          "  ","dplyr::filter(grepl('^catch$', label), \n",
          "  ", "  ","year == end_year) |> \n",
          "  ", "dplyr::group_by(year) |> \n",
          "  ", "dplyr::summarise(total_catch  = sum(estimate)) |> \n",
          "  ", "dplyr::ungroup() |> \n",
          "  ", "dplyr::pull(total_catch) \n",
          # chk_c <- dplyr::filter(output2, grepl("^catch$", label), year == end_year) |>
          #   dplyr::group_by(year) |>
          #   dplyr::summarise(total_catch = sum(estimate))
          "# total landings in the last year \n",
          # "total_landings <- output |>", "\n",
          # "  ", "dplyr::filter(grepl('landings_weight', label), year == end_year,", "\n",
          # "  ", "  ", "is.na(fleet),", "\n",
          # "  ", "  ", "is.na(age)) |>", "\n",
          # "  ", "dplyr::pull(estimate)", "\n",
          "total_landings <- output |>", "\n",
          "  ","dplyr::filter(grepl('landings_observed', label), year == end_year) |>", "\n", # temp remove grepl('landings_weight', label) |
          "  ","dplyr::group_by(year) |>", "\n",
          "  ","dplyr::summarise(total_land  = sum(estimate)) |>", "\n",
          "  ","dplyr::ungroup() |>", "\n",
          "  ","dplyr::pull(total_land)", "\n",
          "# spawning biomass in the last year\n", "\n",
          "SBend <- output2 |>", "\n",
          "  ", "dplyr::filter(grepl('spawning_biomass', label), year == end_year) |>", "\n",
          "  ", "dplyr::pull(estimate) |>", "\n",
          "  ", "unique()", "\n",
          "# overall natural mortality or at age \n",
          "M <- output |>", "\n",
          "  ", "dplyr::filter(grepl('natural_mortality', label)) |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# Biomass at msy \n",
          "# to change to another reference point, replace msy in the following lines with other label \n",
          "Bmsy <- output2 |>", "\n",
          "  ", "dplyr::filter(c(grepl('^biomass', label) & grepl('msy', label) & estimate >1) | grepl('^biomass_msy$', label)) |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# target spawning biomass(msy) \n",
          "# please change target if desired \n",
          "SBmsy <- output2 |>", "\n",
          "  ", "dplyr::filter(c(grepl('spawning_biomass', label) & grepl('msy$', label) & estimate > 1) | label == 'spawning_biomass_msy$') |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# steepness \n",
          "h <- output |> ", "\n",
          "  ", "dplyr::filter(grepl('steep', label)) |> ", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# recruitment \n",
          "R0 <- output |> ", "\n",
          "  ", "dplyr::filter(grepl('R0$', label) | grepl('recruitment_virgin', label)) |> ", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# female SB (placeholder)", "\n"
        ),
        label = "output_and_quantities",
        chunk_option = c("echo: false", "warning: false", ifelse(is.null(model_results), "eval: false", "eval: true"))
      )

      # extract old preamble if don't want to change
      if (rerender_skeleton) {
        question1 <- readline("Do you want to keep the current preamble? (Y/N)")

        # answer question1 as y if session isn't interactive
        if (!interactive()){
          question1 <- "y"
        }
        if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
          start_line <- grep("output_and_quantities", prev_skeleton) - 1
          # find next trailing "```"` in case it was edited at the end
          end_line <- grep("```", prev_skeleton)[grep("```", prev_skeleton) > start_line][1]
          # preamble <- paste(prev_skeleton[start_line:end_line], collapse = "\n")
          preamble <- prev_skeleton[start_line:end_line]

          if (!is.null(model_results)) {
            prev_results_line <- grep("output <- ", preamble)[1]
            prev_results <- stringr::str_replace(
              preamble[prev_results_line],
              "(?<=output\\s{0,5}<-).*",
              deparse(substitute(model_results))
            )
            # add back in pipe
            prev_results <- paste0(prev_results, " |>")
            preamble <- append(preamble, prev_results, after = prev_results_line)[-prev_results_line]

            # change chunk eval to true
            if (any(grepl("eval: false", preamble))) {
              chunk_eval_line <- grep("eval: ", preamble)
              eval_line_new <- stringr::str_replace(
                preamble[chunk_eval_line],
                "eval: false",
                "eval: true"
              )
              preamble <- paste(
                append(
                  preamble,
                  eval_line_new,
                  after = chunk_eval_line)[-chunk_eval_line],
                collapse = "\n"
              )
            }
            preamble <- paste(preamble, collapse = "\n")

            # if (!grepl(".csv", model_results)) warning("Model results are not in csv format - Will not work on render")
          } else {
            cli::cli_alert_info("Preamble maintained.")
            cli::cli_alert_info("Model results not updated.")
            preamble <- paste(preamble, collapse = "\n")
          }
        } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
          cli::cli_alert_warning("Report template files were not copied into your directory.")
          cli::cli_alert_info("If you wish to update the template with new parameters or output files, please edit the {report_name} in your local folder.",
                              wrap = TRUE)
        }
      } # close if rerender

    ##### Disclaimer ----
    disclaimer <- "{{< pagebreak >}}\n\n## Disclaimer {.unnumbered .unlisted}\n\nThese materials do not constitute a formal publication and are for information only. They are in a pre-review, pre-decisional state and should not be formally cited or reproduced. They are to be considered provisional and do not represent any determination or policy of NOAA or the Department of Commerce.\n"

    ##### Citation ----
    # Add page for citation of assessment report
    if (rerender_skeleton) {
      if (!is.null(title) | !is.null(species) | !is.null(year) | !is.null(author)) {
        citation_line <- grep("Please cite this publication as:", prev_skeleton) + 2
        citation <- glue::glue("{{< pagebreak >}} \n\n Please cite this publication as: \n\n {prev_skeleton[citation_line]}\n\n")
      } else {
        author <- grep("  - name: ", prev_skeleton)
        citation <- create_citation(
          author = author,
          ...
        )
        cli::cli_alert_success("Added report citation.")
      }
    } else {
      citation <- create_citation(
        author = author,
        title = title,
        year = year
      )
      cli::cli_alert_success("Added report citation.")
    }


    ##### Create report outline ----
      # Include tables and figures in template
      # at this point, files_to_copy is the most updated outline

    ###### Rerender & not custom ----
    if (rerender_skeleton & custom == FALSE) {
      # identify all previous sections
      sections <- stringr::str_extract_all(
        prev_skeleton,
        "(?<=['`])[^']+\\.qmd(?=['`])"
      ) |>
        unlist() |>
        purrr::discard(~ .x == "")
      # add sections as list
      sections <- add_child(
        sections,
        label = gsub(".qmd", "", unlist(sections))
      )
    } else if (custom == FALSE) {
      sections <- add_child(
        sort(c(files_to_copy, "08_tables.qmd", "09_figures.qmd")),
        # TODO: need to remove the numbers proceeding the names as well
        label = stringr::str_extract(sort(c(files_to_copy, "08_tables.qmd", "09_figures.qmd")), "(?<=_).+(?=\\.qmd$)")
      )
    } else {
      ###### Rerender & custom ----
      # Option for building custom template
      # Create custom template from existing skeleton sections
      if (is.null(new_section)) {
        section_list <- add_base_section(files_to_copy)
        sections <- add_child(section_list,
                              label = custom_sections # this might need to be changed
        )
        # Create sections object to add into template
        sections <- add_child(
          sections,
          label = stringr::str_extract(unlist(sections), "(?<=_).+(?=\\.qmd$)")
        )

      } else { # custom = TRUE
        # Create custom template using existing sections and new sections from analyst
        # Add sections from package options

        if (is.null(custom_sections)) {
          # TODO: type - this needs to just pull all files from folder that
          # it was copying from when custom sections is null -- DONE

          sec_list1 <- files_to_copy
          sec_list2 <- add_section(
            new_section = new_section,
            section_location = section_location,
            custom_sections = sec_list1,
            subdir = subdir
          )

          # Create sections object to add into template
          sections <- add_child(
            sort(c(sec_list2, "08_tables.qmd", "09_figures.qmd")),
            label = stringr::str_extract(sort(c(files_to_copy, "08_tables.qmd", "09_figures.qmd")), "(?<=_).+(?=\\.qmd$)")
          )
        } else { # custom_sections explicit

          # Add selected sections from base
          sec_list1 <- add_base_section(files_to_copy)
          # Create new sections as .qmd in folder
          # check if sections are in custom_sections list
          if (any(stringr::str_replace(section_location, "^[a-z]+-", "") %notin% custom_sections)) {
            cli::cli_abort("Defined customizations do not match one or all of the relative placement of a new section. Please review inputs.")
          }
          # reorder sec_list1 alphabetically so that 11_appendix goes to end of list
          sec_list1 <- sec_list1[order(names(stats::setNames(sec_list1, sec_list1)))]

          sec_list2 <- add_section(
            new_section = new_section,
            section_location = section_location,
            custom_sections = sec_list1,
            subdir = subdir
          )
          # Create sections object to add into template
          sections <- add_child(
            sec_list2,
            label = stringr::str_extract(unlist(sec_list2), "(?<=_).+(?=\\.qmd$)")
          )
        } # close if statement for very specific sectioning
      } # close if statement for extra custom
    } # close if statement for custom

    ###### Pull together skeleton ----
    report_template <- paste(
      yaml,
      "\\printnoidxglossaries \n",
      params_chunk,
      preamble,
      disclaimer,
      citation,
      sections,
      sep = "\n"
    )

    cli::cli_alert_success("Created report template.")

    ##### Save skeleton ----
    # Save template as .qmd to render
    utils::capture.output(cat(report_template), file = file.path(subdir, ifelse(rerender_skeleton, new_report_name, report_name)), append = FALSE)
    # Delete old skeleton
    if (length(grep("skeleton.qmd", list.files(file_dir, pattern = "skeleton.qmd"))) > 1) {
      question1 <- readline("Deleting previous skeleton file... Do you want to proceed? (Y/N)")

      # answer question1 as y if session isn't interactive
      if (!interactive()){
        question1 <- "y"
      }

      if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
        file.remove(file.path(file_dir, report_name))
      } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
        cli::cli_alert_info("Skeleton file retained.")
      }
    }

    ##### Final message ----
    # Print message
    if (rerender_skeleton) {
      cli::cli_alert_success("Updated report skeleton in directory {subdir}.")
    } else {
      cli::cli_alert_success("Saved report template in directory {subdir}.")
      cli::cli_alert_info("To proceeed, please edit sections within the report template in order to produce a completed stock assessment report.",
                          wrap = TRUE)
    }
    # Open file for analyst
    # file.show(file.path(subdir, report_name)) # this opens the new file, but also restarts the session
    # Open the file so path to other docs is clear
    # utils::browseURL(subdir)
  } else {
    #### Previous template call ----
    # Copy old template and rename for new year
    # Create copy of previous assessment
    if (!is.null(region)) {
      olddir <- fs::path(file_dir, "report")
      invisible(file.copy(file.path(olddir, list.files(olddir)), subdir, recursive = FALSE))
    } else {
      olddir <- fs::path(file_dir, "report")
      invisible(file.copy(file.path(olddir, list.files(olddir)), subdir, recursive = FALSE))
    }

    # Edit skeleton to update year and results file
    skeleton <- list.files(subdir, pattern = "skeleton.qmd")
    # Open previous skeleton
    # file.show(file.path(subdir, report_name))

    svDialogs::dlg_message("Reminder: Changes should be made when calling an old report. Please change 1) the year in the citation and 2) the location and name of the results file in the first chunk of the report.",
      type = "ok")
  }
}
