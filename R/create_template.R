#' Create Stock Assessment Report Template
#'
#' Generates a set of Quarto files (.qmd) that set up a stock assessment report
#' with supporting files. Function builds a YAML specific to the region and
#' utilizes current resources and workflows from different NOAA Fishery Science
#' Centers. Automates authorship, bibliography, and other report components.
#'
#' @param format Report rendering format. Note: "docx" is currently unsupported
#' and will default to "pdf".
#'
#' Default: "pdf"
#'
#' Options: "pdf", "html"
#'
#' @param type Report template type.
#'
#' Default: "sar" (a NOAA standard "Stock Assessment Report")
#'
#' Options: "sar" (Stock Assessment Report), "nemt" (Northeast Management Track), "pfmc" (Pacific Fishery Management Council), "safe" (Stock Assessment and Fishery Evaluation)
#'
#' @param office Regional Fisheries Science Center producing the report.
#'
#' Default: NULL
#'
#' Options: "AFSC", "NEFSC", "NWFSC", "PIFSC", "SEFSC", "SWFSC"
#'
#' @param region Full name of the stock's sub-region, if applicable.
#' If the region is not specified for your center or species, leave default.
#' Example: "US West Coast".
#'
#' Default: NULL
#'
#' @param species Common name of target species. Split multi-word names
#' with space and capitalize first letter(s). Example: "Dover sole".
#'
#' Default: "species"
#'
#' @param spp_latin Latin name of target species. Example: "Pomatomus saltatrix".
#'
#' Default: NULL
#'
#' @param year Year the assessment is conducted.
#'
#' Default: the year in which the report is rendered.
#'
#' @param authors A character vector of author names and affiliations.
#' For example, a Jane Doe at the NWFSC Seattle, Washington office
#' would have an entry of c("Jane Doe"="NWFSC-SWA"). Information on NOAA offices
#' can be found with: \code{asar::affiliation_info}. Keys to the office addresses
#' follow the naming convention of: office acronym (ex. NWFSC), a hyphen (-),
#' the first initial of the city, and then the two-letter abbreviation for
#' the state the office is located in. If the city has two or more words (e.g.,
#' Panama City), the first initial of each word is used in the key
#' (ex. Panama City, Florida = PCFL).
#'
#' Default: NULL
#'
#' Options: See \code{asar::affiliation_info}.
#'
#' @param file_dir Directory where report files will be created.
#'
#' Default: the working directory (`getwd()`).
#'
#' @param title Custom report title superceding the default composed in
#' \code{asar::create_title()}. Example: "Management Track Assessments Spring
#' 2024".
#'
#' Default: \verb{[TITLE]}. If species and region are provided, a title will be generated based on the report type, species, and region.
#'
#' @param model_results Filepath to the standardized, converted model output
#' .rda file generated with `stockplotr::convert_output()`, relative to the
#' skeleton .qmd file that will be created within the 'report' folder.
#'
#' Default: NULL
#'
#' @param tables_dir The location of the "tables" folder, which contains tables
#' files
#'
#' Default: the working directory
#'
#' @param figures_dir The location of the "figures" folder, which contains
#' figures files
#'
#' Default: the working directory
#'
#' @param spp_image Filepath to a custom species image to be used on the
#' report cover. Supported file extension is .png.
#' If empty, searches `asar` resources for a matching species name.
#'
#' Default: NULL
#'
#' @param bib_file File path to an existing additional bibliography file (`.bib`) used for citing references in
#' the report. By default, all bibliography files are sourced from the \pkg{journals} package and 
#' references for all NMFS stock assessment reports are provided. To see a full 
#' list of journals included in these files, please visit the
#' [{journals} README](https://github.com/nmfs-ost/journals/blob/main/README.md) 
#' or see the description at the top of each bib file. It is 
#' recommended to open these files in a text editor rather than R.
#'
#' Default: NULL
#'
#' @param new_template TRUE/FALSE; Create a new template? If true,
#' will pull the last saved stock assessment report skeleton.
#'
#' Default: FALSE
#'
#' @param custom_sections List of existing sections to include in a custom
#' template (rather than the default for stock assessments in your region).
#' If adding a new section, also use arguments 'new_section' and 'section_location'.
#'
#' Default: NULL
#'
#' Options: sections within
#' \code{list.files(system.file("templates", "skeleton", package = "asar"))}.
#' The name of the section, rather than the name of the file, can be used
#' (e.g., 'abstract' rather than '00_abstract.qmd').
#'
#' @param new_section Names of section(s) (e.g., "Special Section") or
#' subsection(s) (e.g., a section within the introduction) that will be
#' added to the document. Please make a short list if >1 section/subsection
#' will be added. The template will be created as a quarto document, added
#' into the skeleton, and saved for reference.
#'
#' Default: NULL
#'
#' @param section_location Where new section(s)/subsection(s) will be added to
#' the skeleton template. Please use the notation of 'placement-section'.
#' For example, 'in-introduction' signifies that the new content would
#' be created as a child document and added into the 02_introduction.qmd.
#' To add >1 (sub)section, make the location a list corresponding to the
#' order of (sub)section names listed in the 'new_section' parameter.
#'
#' Default: NULL
#'
#' @param custom_params Character vector of additional custom parameter
#' names and values to include in the skeleton YAML. For example, a
#' parameter "year2" and its value "2026" would have an entry of
#' `c("year2" = "2026")`. Parameters automatically included: office, region,
#' species (each of which are listed as individual parameters for this
#' function, above).
#'
#' Default: NULL
#'
#' @param ... Additional arguments passed into functions used in create_template
#' such as `create_citation()` or `create_yaml()`.
#'
#' @returns Path to the created `report/` directory containing files
#' needed to produce the stock assessment report. Side effects include
#' the creation of a directory structure, `.qmd` files, and support
#' files (e.g., images, `.bib`, `.tex`).
#'
#' @details The function creates a `report/` subdirectory within `file_dir`.
#' The primary file is a "skeleton" Quarto document that calls various
#' sections as child documents. The skeleton will be named based on arguments
#' provided to \code{create_template()}. For instance, in example 2, below,
#' the filename would be 'sar_Dover_sole_skeleton.qmd'.
#'
#' The skeleton contains several sections that should require little to no
#' editing by the user. These sections include: the yaml, Parameters R chunk,
#' Preamble R chunk, Disclaimer, and Citations.
#'
#' Report content is called as child documents in this skeleton. Each child
#' document (e.g., '01_executive_summary.qmd', '02_introduction.qmd') should
#' be edited separately.
#'
#' To see report templates included in the base skeleton, run
#' \code{list.files(system.file('templates','skeleton', package = 'asar'))}.
#'
#' For help with editing any of the sections in the skeleton, please see the
#' cheatsheet, tutorial, and other resources available at \url{https://nmfs-ost.github.io/asar/}.
#'
#' @seealso [add_authors()], [add_base_section()], [add_child()], [add_chunk()], [add_base_section()], [add_section()], [create_citation()], [create_figures_doc()], [create_tables_doc()], [create_title()], [create_yaml()], [format_quarto()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_template(
#'   new_section = "a_new_section",
#'   section_location = "before-introduction"
#' )
#'
#' create_template(
#'   new_template = TRUE,
#'   format = "pdf",
#'   office = "NWFSC",
#'   species = "Dover sole",
#'   spp_latin = "Microstomus pacificus",
#'   year = 2010,
#'   authors = c(
#'     "John Snow" = "AFSC",
#'     "Danny Phantom" = "NEFSC",
#'     "Patrick Star" = "SEFSC-ML"
#'   ),
#'   model_results = here::here("folder", "std_output.rda"),
#'   figures_dir = here::here(),
#'   tables_dir = here::here("tables_folder_location"),
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
#'   authors = c("John Snow" = "AFSC"),
#'   new_section = c("a_new_section", "another_new_section"),
#'   section_location = c("before-introduction", "after-introduction"),
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
#'   authors = c("John Snow" = "NEFSC", "Danny Phantom" = "SWFSC", "Patrick Star" = "SEFSC-ML"),
#'   title = "Management Track Assessments Spring 2024",
#'   custom_params = c("region2" = "North Coast", "year2" = "2026"),
#'   model_results = here::here("folder", "std_output.rda"),
#'   new_section = "an_additional_section",
#'   section_location = "before-discussion",
#'   type = "sar",
#'   custom_sections = c("executive_summary", "introduction", "discussion"),
#'   spp_image = "dir/containing/spp_image"
#' )
#' }
#'
create_template <- function(
  format = "pdf",
  type = "sar",
  office = NULL,
  region = NULL,
  species = "species",
  spp_latin = NULL,
  year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y"),
  authors = NULL,
  file_dir = getwd(),
  title = "[TITLE]",
  model_results = NULL,
  tables_dir = getwd(),
  figures_dir = getwd(),
  spp_image = NULL,
  bib_file = NULL,
  new_template = TRUE,
  custom_sections = NULL,
  new_section = NULL,
  section_location = NULL,
  custom_params = NULL,
  ...
) {
  type_map <- c(
    "Northeast Management Track" = "nemt",
    "Pacific Fishery Management Council" = "pfmc",
    "Stock Assessment and Fishery Evaluation" = "safe",
    "Stock Assessment Report" = "skeleton",
    "sar" = "skeleton",
    "pfmc" = "pfmc",
    "nemt" = "nemt",
    "safe" = "safe",
    "skeleton" = "skeleton"
  )

  if (type %in% names(type_map)) {
    type <- unname(type_map[type])
  } else if (isTRUE(interactive) || (is.function(interactive) && interactive())) {
    selection <- utils::menu(
      title = "Unrecognized template type. Please select an option below: ",
      choices = c("Default", "Pacific Fisheries Management Council", "Northeast Management Track", "SAFE")
    )
    type <- switch(as.character(selection),
      "2" = "pfmc",
      "3" = "nemt",
      "4" = "safe",
      "skeleton"
    )
  } else {
    type <- "skeleton"
  }

  if (!is.null(office) & length(office) == 1) {
    office <- match.arg(office, choices = c("AFSC", "PIFSC", "NEFSC", "NWFSC", "SEFSC", "SWFSC"), several.ok = FALSE)
  } else if (length(office) > 1 | is.null(office)) {
    office <- ""
  }
  
  # Name report
  if (!is.null(type)) {
    report_name <- paste0(
      ifelse(type == "skeleton", "sar", type),
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
      toupper(stringr::str_c(stringr::str_extract_all(region, "\\b[A-Za-z]")[[1]], collapse = "")),
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

  # Select format
  if (grepl("^pdf$|^html$", tolower(format))) {
    format <- tolower(format)
  } else if (grepl("docx", tolower(format))) {
    cli::cli_alert_warning("The docx format is not currently supported by asar. Defaulting to pdf.",
      wrap = TRUE
    )
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
          wrap = TRUE
        )
        format <- "pdf"
      } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
        cli::cli_abort("Template processing stopped.")
      }
    } else {
      cli::cli_abort("Format not recognized. Please use pdf, html, or docx.")
    }
  }

  # Create subdirectory for files
  subdir <- ifelse(
    grepl("/report", file_dir) || file_dir == "report",
    fs::path(file_dir),
    fs::path(file_dir, "report")
  )

  # Supporting files folder
  supdir <- file.path(subdir, "support_files")

  if (dir.exists(subdir) == FALSE) {
    dir.create(subdir, recursive = TRUE)
  }
  if (dir.exists(supdir) == FALSE) {
    dir.create(supdir, recursive = FALSE)
  }

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
    if (is.null(spp_image) && species == "species") {
      spp_image <- ""
    } else if (is.null(spp_image) && species != "species") {
      spp_image <- system.file("resources", "spp_img", paste(gsub(" ", "_", species), ".png", sep = ""), package = "asar")
    }

    # Add bib file
    bib_dir <- file.path(subdir, "bibliography_files")
    if (!dir.exists(bib_dir)) {
      dir.create(bib_dir, recursive = FALSE)
    }
    
    journals::download_bibs(bib_dir)
    bib_file_paths <- list.files(bib_dir, pattern = ".bib", full.names = TRUE)
    base_bib_file <- bib_file_paths[!grepl(".sty", bib_file_paths)]
    # Remove .sty file and copy to main report folder
    file.copy(list.files(bib_dir, pattern = ".sty", full.names = TRUE), subdir, overwrite = FALSE) |> suppressWarnings()
    file.remove(list.files(bib_dir, pattern = ".sty", full.names = TRUE))
    bib_name <- basename(base_bib_file)
    
    # append asar citation to first .bib
    asar_citation <- "
@Manual{asar_2026,
  title = {asar: Build NOAA Stock Assessment Report},
  author = {Samantha Schiano and Sophie Breitbart and Steve Saul},
  year = {2026},
  note = {R package version 2.2.0},
  url = {https://github.com/nmfs-ost/asar},
}"
    if (!is.na(base_bib_file[1]) && nzchar(base_bib_file[1])) {
      write(asar_citation, file = base_bib_file[1], append = TRUE)
    }
    
    # Add bib file if bib_file is not NULL
    if (!is.null(bib_file)) {
      file.copy(bib_file, bib_dir, overwrite = TRUE) |> suppressWarnings()
      bib_name <- c(bib_name, basename(bib_file))
    }
    
    #### Read in previous skeleton if rerender ----
    # Check if this is a rerender of the skeleton file
    #### Copy template files to report folder ----
    # Check if there are already files in the folder
    # Only files present should be:
    # 1. bibliography_files folder
    # 2. support_files folder
    # 3. journals-bibnames.sty
    # 4. ?
    if (length(list.files(subdir)) < 4) {
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
      # Copy us doc logo
      file.copy(system.file("resources", "us_doc_logo.png", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
      # Copy glossary
      file.copy(system.file("glossary", "report_glossary.tex", package = "asar"), subdir, overwrite = FALSE) |> suppressWarnings()
      # Copy html format file if applicable
      if (tolower(format) == "html") file.copy(system.file("resources", "formatting_files", "theme.scss", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
      # Copy over glossary and associated tex file
      if (tolower(type) == "pfmc") {
        # file.copy(system.file("resources", "formatting_files", "sa4ss_glossaries.tex", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
        file.copy(system.file("resources", "formatting_files", "pfmc.tex", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
      }
      # copy csl file
      file.copy(system.file("resources", "cjfas.csl", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
      # show message and make README stating model_results info
      if (!is.null(model_results)) {
        mod_time <- as.character(file.info(fs::path(model_results), extra_cols = FALSE)$ctime)
        mod_msg <- paste(
          "Report is based upon model output from", model_results,
          "that was last modified on:", mod_time
        )
        cli::cli_alert_info(mod_msg)
        writeLines(
          mod_msg,
          fs::path(
            subdir,
            paste0(
              gsub(".rda", "", basename(model_results)),
              "_metadata.md"
            )
          )
        )
      }
    } else {
      cli::cli_alert_warning("There are files in this location.")
      question1 <- readline("The function wants to overwrite the files currently in your directory. Would you like to proceed? (Y/N)")
      
      # answer question1 as y if session isn't interactive
      if (!interactive()) {
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
        # Copy us doc logo
        file.copy(system.file("resources", "us_doc_logo.png", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
        # Copy glossary
        file.copy(system.file("glossary", "report_glossary.tex", package = "asar"), subdir, overwrite = FALSE) |> suppressWarnings()
        # Copy html format file if applicable
        if (tolower(format) == "html") file.copy(system.file("resources", "formatting_files", "theme.scss", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
      } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
        cli::cli_alert_warning("Report template files were not copied into your directory.")
        cli::cli_alert_info("If you wish to update the template with new parameters or output files, please edit the {report_name} in your local folder.",
                            wrap = TRUE
        )
      }
    } # close check for previous files & respective copying

    # Handle legacy document order and migration
    # TODO: Not sure how setting this to rerender = FALSE always impacts this feature
    # Maybe this should go through deprecation in x amnt of time?
    fig_info <- migrate_legacy_docs(subdir, doc_type = "figures", rerender_skeleton = FALSE)
    tbl_info <- migrate_legacy_docs(subdir, doc_type = "tables", rerender_skeleton = FALSE)

    renamed_tables_doc <- FALSE
    if (can_rename_legacy_doc(tbl_info)) {
      from <- fs::path(subdir, tbl_info$legacy_name)
      to <- fs::path(subdir, tbl_info$current_name)
      if (!identical(from, to) && file.exists(from)) {
        renamed_tables_doc <- file.rename(from = from, to = to)
      }
    }

    renamed_figures_doc <- FALSE
    if (can_rename_legacy_doc(fig_info)) {
      from <- fs::path(subdir, fig_info$legacy_name)
      to <- fs::path(subdir, fig_info$current_name)
      if (!identical(from, to) && file.exists(from)) {
        renamed_figures_doc <- file.rename(from = from, to = to)
      }
    }

    if (renamed_figures_doc || renamed_tables_doc) {
      renamed_docs <- c(
        if (renamed_figures_doc) paste0("{.file ", fig_info$current_name, "}"),
        if (renamed_tables_doc) paste0("{.file ", tbl_info$current_name, "}")
      )
      cli::cli_alert_info("Detected legacy figure/table document order in the skeleton.")
      cli::cli_alert_info("asar switched to {toString(renamed_docs)}.")
    }

    # Created tables doc
    tables_doc_name <- switch(
      type,
      "nemt" = "06_tables.qmd",
      "safe" = "12_tables.qmd",
      "09_tables.qmd"
    )

    create_tables_doc(
      subdir = subdir,
      tables_dir = tables_dir
    )

    # Create figures qmd
    figures_doc_name <- switch(
      type,
      "nemt" = "05_figures.qmd",
      "safe" = "11_figures.qmd",
      "08_figures.qmd"
    )
      
    create_figures_doc(
      subdir = subdir,
      figures_dir = figures_dir
    )
    # rename figures doc
    if (figures_doc_name != "08_figures.qmd") {
      file.rename(
        from = fs::path(subdir, "08_figures.qmd"),
        to = fs::path(subdir, figures_doc_name)
      )
    }

    # Part I
    # Create a report template file to render for the region and species
    # Create YAML header for document
    # Write title based on report type and region
    # Extract region based on param if it was previously found
    if (title == "[TITLE]") {
      title <- create_title(
        office = office,
        species = species,
        spp_latin = spp_latin,
        region = region,
        type = type,
        year = year
      )
    }

    # Authors and affiliations
    # Parameters to add authorship to YAML
    author_list <- add_authors(
      # prev_skeleton = ifelse(rerender_skeleton, prev_skeleton, NULL),
      authors = authors, # need to put this in case there is a rerender otherwise it would not use the correct argument
      rerender_skeleton = FALSE
    )
    
    # Parameters
    parameters <- TRUE
    param_names <- custom_params |> names()
    param_values <- custom_params |> unname()

    # Create YAML -- put elements together
    yaml <- create_yaml(
      prev_format = prev_format,
      format = format,
      prev_skeleton = prev_skeleton,
      author_list = author_list,
      title = title,
      rerender_skeleton = FALSE,
      office = office,
      spp_image = spp_image,
      species = species,
      spp_latin = spp_latin,
      region = region,
      parameters = parameters,
      custom_params = custom_params,
      bib_name = bib_name,
      year = year,
      type = type
    )

    cli::cli_alert_success("Built YAML header.")

    ##### Params chunk ----
    params_chunk <- add_chunk(
      paste0(
        "# Parameters \n",
        "spp <- params$species \n",
        "SPP <- params$species \n",
        "species <- params$species \n",
        "spp_latin <- params$spp_latin \n",
        "office <- params$office",
        if (!is.null(region)) {
          paste0("\n", "region <- params$region")
        },
        if (!is.null(param_names)) {
          paste0(
            "\n",
            paste0(param_names, " <- ", "params$", param_names, collapse = " \n")
          )
        }
      ),
      label = "R_parameters"
    )

    params_chunk <- add_chunk(
      paste0(
        "# Parameters \n",
        "spp <- params$species \n",
        "SPP <- params$species \n",
        "species <- params$species \n",
        "spp_latin <- params$spp_latin \n",
        "office <- params$office",
        if (!is.null(region)) {
          paste0("\n", "region <- params$region")
        },
        if (!is.null(param_names)) {
          paste0(
            "\n",
            paste0(param_names, " <- ", "params$", param_names, collapse = " \n")
          )
        }
      ),
      label = "R_parameters"
    )

    ##### Preamble ----
    # Add preamble
    # add in quantities and output data R chunk
    # Reassign model_results as output and save into environment for user
    # assign("output", model_results, envir = .GlobalEnv)

    if (!is.null(model_results)) {
      # add model results according to documentation description
      load_method <- glue::glue("load({model_results}) \n")
    } else {
      load_method <- ""
      # df_name <- "NULL"
    }

    # standard preamble
    # copy preamble code into report folder
    file.copy(
      system.file("resources", "preamble.R", package = "asar"),
      subdir,
      overwrite = TRUE
    ) |> suppressWarnings()

    preamble <- add_chunk(
      paste0(
        "# load converted output from stockplotr::convert_output() \n",
        load_method, "\n",
        "# Call reference points and quantities below \n",
        "output <- out_new |> \n", # df_name
        "  ", "dplyr::mutate(estimate = as.numeric(estimate), \n",
        "  ", "  ", "uncertainty = as.numeric(uncertainty)) \n",
        "source(\"preamble.R\") \n",
        "# Available quantities\n",
        "start_year\n",
        "end_year\n",
        "Fend # terminal fishing mortality\n",

        # "# modify in source code if alternative target desired", "\n",
        "Ftarg # fishing mortality at msy\n",
        "F_Ftarg # Terminal year F respective to F target\n",
        "Bend # terminal year biomass\n",
        "Btarg # target biomass (msy)\n",
        "total_catch # total catch in the last year\n",
        "total_landings # total landings in the last year\n",
        "SBend # spawning biomass in the last year\n",
        "M # overall natural mortality or at age\n",
        "Bmsy # target spawning biomass(msy)\n",
        "h # steepness\n",
        "R0 # recruitment\n"
      ),
      label = "preamble",
      chunk_option = c("warning: false", ifelse(is.null(model_results), "eval: false", "eval: true"), "include: false")
    )

    ##### Disclaimer ----
    disclaimer <- "{{< pagebreak >}}\n\n## Disclaimer {.unnumbered .unlisted}\n\nThese materials do not constitute a formal publication and are for information only. They are in a pre-review, pre-decisional state and should not be formally cited or reproduced. They are to be considered provisional and do not represent any determination or policy of NOAA or the Department of Commerce.\n"

    ##### Citation ----
    # Add page for citation of assessment report
    citation <- create_citation(
      authors = authors,
      title = title,
      year = year
    )
    cli::cli_alert_success("Added report citation.")

    ##### Create report outline ----
    # Include tables and figures in template
    # at this point, files_to_copy is the most updated outline

    ###### Not custom ----
    # add check if user set custom sections
    if (!is.null(new_section) || !is.null(custom_sections)) custom <- TRUE
    if (is.null(custom_sections)) {
      sections <- add_child(
        sort(c(files_to_copy, tables_doc_name, figures_doc_name)),
        # TODO: need to remove the numbers proceeding the names as well
        label = stringr::str_extract(sort(c(files_to_copy, tables_doc_name, figures_doc_name)), "(?<=_).+(?=\\.qmd$)")
      )
    } else {
      sections <- custom_true(
        new_section = new_section,
        section_location = section_location,
        custom_sections = custom_sections,
        files_to_copy = files_to_copy,
        tables_doc_name = tables_doc_name,
        figures_doc_name = figures_doc_name,
        subdir = subdir
      )
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
    utils::capture.output(cat(report_template), file = file.path(subdir, report_name), append = FALSE)
    

    ##### Final message ----
    cli::cli_alert_success("Saved report template in directory {subdir}.")
    cli::cli_alert_info("To proceed, please edit sections within the report template in order to produce a completed stock assessment report.",
                        wrap = TRUE
    )
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
      type = "ok"
    )
  }
}
