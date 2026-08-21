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
#' @param bib_file File path to bibliography file (`.bib`) used for citing references in
#' the report
#'
#' Default: "asar_references.bib"
#'
#' @param new_template TRUE/FALSE; Create a new template? If true,
#' will pull the last saved stock assessment report skeleton.
#'
#' Default: FALSE
#'
#' @param rerender_skeleton TRUE/FALSE; Update the skeleton YAML and structure
#' (R parameters, preamble, and skeleton sectioning) if relevant or indicated.
#' All files in your folder, such as the `.qmd` child docs, will remain as is.
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
    bib_file = "asar_references.bib",
    new_template = TRUE,
    rerender_skeleton = FALSE,
    custom_sections = NULL,
    new_section = NULL,
    section_location = NULL,
    custom_params = NULL,
    ...
) {
  # Check input type
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
  
  # 1. Match if it exists in the mapping
  if (type %in% names(type_map)) {
    type <- unname(type_map[type])
    
    # 2. If unmatched and interactive, prompt the user
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
    
    # 3. If unmatched and non-interactive, revert to skeleton
  } else {
    type <- "skeleton"
  }
  
    if (!is.null(office) & length(office) == 1) {
    office <- match.arg(office, choices = c("AFSC", "PIFSC", "NEFSC", "NWFSC", "SEFSC", "SWFSC"), several.ok = FALSE)
  } else if (length(office) > 1 | is.null(office)) {
    office <- ""
  }
  
  #### Rerender skeleton ----
  if (rerender_skeleton) {
    report_name <- list.files(file_dir, pattern = "skeleton.qmd")
    if (length(report_name) == 0) cli::cli_abort("No skeleton quarto file found in the `file_dir` ({file_dir}).")
    if (length(report_name) > 1) cli::cli_abort("Multiple skeleton quarto files found in the `file_dir` ({file_dir}).")
    
    prev_report_name <- gsub("_skeleton.qmd", "", report_name)
    # Extract type
    type <- stringr::str_extract(tolower(prev_report_name), "^[a-z]+")
    # Extract region unless region is changed or updated
    prev_skeleton <- readLines(file.path(file_dir, list.files(file_dir, pattern = "skeleton.qmd")))
    if (is.null(region)) {
      region <- stringr::str_extract(
        prev_skeleton[grep("region: ", prev_skeleton)],
        "(?<=')[^']+(?=')"
      )
    }
    region_name <- ifelse(
      region != "NA",
      toupper(stringr::str_c(stringr::str_extract_all(region, "\\b[A-Za-z]")[[1]], collapse = "")),
      stringr::str_extract(prev_report_name, "(?<=_)[A-Z]+(?=_)")
    )
    report_name_1 <- gsub(
      glue::glue("{type}_"),
      "",
      prev_report_name
    )
    # Extract species unless species is renamed
    species <- ifelse(
      species != "species",
      species,
      gsub(
        "_",
        " ",
        gsub(glue::glue("{region_name}_"), "", report_name_1)
      )
    )
    
    new_report_name <- paste0(
      type, "_",
      ifelse(
        is.null(region) | is.na(region) | region == "NA",
        "",
        glue::glue("{region_name}_")
      ),
      ifelse(is.null(species), "species", stringr::str_replace_all(species, " ", "_")), "_",
      "skeleton.qmd"
    )
    if (type == "sar") type <- "skeleton"
  } else {
    # Name report
    if (!is.null(type)) {
      report_name <- paste0(
        ifelse(type == "skeleton", "sar", type),
        "_"
      )
    } else {
      report_name <- paste0("type_")
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
    report_name <- paste0(
      report_name,
      gsub(" ", "_", species),
      "_skeleton.qmd"
    )
  }
  
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
    asar_folder <- system.file("templates", package = "asar")
    current_folder <- ifelse(rerender_skeleton, subdir, file.path(asar_folder, type))
    new_folder <- subdir
    
    ##### Identify files to copy ----
    if (!is.null(custom_sections)) {
      files_to_copy <- unlist(list.files(current_folder))[c(unlist(sapply(custom_sections, function(x) grep(x, list.files(current_folder)))))]
      if (!any(grepl("acknowledgments", files_to_copy))) {
        files_to_copy <- c(files_to_copy, unlist(list.files(current_folder))[10])
        custom_sections <- c(custom_sections, "acknowledgments")
      }
      if (!any(grepl("references", files_to_copy))) {
        files_to_copy <- c(files_to_copy, unlist(list.files(current_folder))[11])
        custom_sections <- c(custom_sections, "references")
      }
    } else {
      if (rerender_skeleton) {
        files_to_copy <- stringr::str_extract(prev_skeleton[grep("knitr::knit_child", prev_skeleton)], "(?<=knit_child\\(').*?(?=\\')")
      } else {
        files_to_copy <- list.files(current_folder)
      }
    }
    
    before_body_file <- system.file("resources", "formatting_files", "before-body.tex", package = "asar")
    
    #### Links to files for yaml ----
    if (is.null(spp_image) && species == "species") {
      spp_image <- ""
    } else if (is.null(spp_image) && species != "species") {
      spp_image <- system.file("resources", "spp_img", paste(gsub(" ", "_", species), ".png", sep = ""), package = "asar")
    }
    
    # Add bib file
    if (bib_file == "asar_references.bib") {
      bib_loc <- system.file("resources", "asar_references.bib", package = "asar")
      bib_name <- bib_file
    } else {
      cli::cli_alert_warning("Bibliography file {bib_file} not in the report directory.")
      cli::cli_alert_info("The file will not be read in on render if not in the same path as the skeleton file.")
      
      bib_loc <- bib_file
      bib_name <- stringr::str_extract(bib_file, "[^/]+$")
    }
    
    #### Read in previous skeleton if rerender ----
    if (rerender_skeleton) {
      if (!file.exists(file.path(file_dir, list.files(file_dir, pattern = "skeleton.qmd")))) stop("No skeleton quarto file found in the working directory.")
      prev_skeleton <- readLines(file.path(file_dir, list.files(file_dir, pattern = "skeleton.qmd")))
      prev_format <- stringr::str_extract(
        prev_skeleton[grep("format:", prev_skeleton) + 1],
        "[a-z]+"
      )
      year <- ifelse(
        is.na(as.numeric(stringr::str_extract(
          prev_skeleton[grep("title:", prev_skeleton)],
          "[0-9]+"
        ))),
        year,
        as.numeric(stringr::str_extract(
          prev_skeleton[grep("title:", prev_skeleton)],
          "[0-9]+"
        ))
      )
      if (!is.null(spp_image)) {
        file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
        if (file.exists(spp_image)) {
          spp_image <- file.path("support_files", stringr::str_extract(spp_image, "(?<=/)[^/]+$"))
        }
      }
      if (tolower(prev_format) != "html" & tolower(format) == "html") {
        if (!file.exists(file.path(file_dir, "support_files", "theme.scss"))) file.copy(system.file("resources", "formatting_files", "theme.scss", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
      }
      if (tolower(prev_format) != "pdf" & tolower(format) == "pdf") {
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
        cli::cli_alert_warning("Undefined year.")
        cli::cli_alert_info("Please identify year in your arguments or manually change it in the skeleton if value is incorrect.",
                            wrap = TRUE
        )
        if (!file.exists(file_dir, "support_files", "before-body.tex")) file.copy(before_body_file, supdir, overwrite = FALSE) |> suppressWarnings()
        if (!file.exists(file_dir, "support_files", "_titlepage.tex") | !is.null(species)) create_titlepage_tex(office = office, subdir = supdir, species = species)
        if (!file.exists(file_dir, "support_files", "in-header.tex") | !is.null(species)) create_inheader_tex(species = species, year = year, subdir = supdir)
      }
    } else {
      #### Copy template files to report folder ----
      if (length(list.files(subdir)) < 2) {
        file.copy(file.path(current_folder, files_to_copy), new_folder, overwrite = FALSE)
        file.copy(before_body_file, supdir, overwrite = FALSE) |> suppressWarnings()
        create_titlepage_tex(office = office, subdir = supdir, species = species)
        create_inheader_tex(species = species, year = year, subdir = supdir)
        file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
        file.copy(bib_loc, subdir, overwrite = TRUE) |> suppressWarnings()
        file.copy(system.file("resources", "us_doc_logo.png", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
        file.copy(system.file("glossary", "report_glossary.tex", package = "asar"), subdir, overwrite = FALSE) |> suppressWarnings()
        if (tolower(format) == "html") file.copy(system.file("resources", "formatting_files", "theme.scss", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
        if (tolower(type) == "pfmc") {
          file.copy(system.file("resources", "formatting_files", "pfmc.tex", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
        }
        file.copy(system.file("resources", "cjfas.csl", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
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
        
        if (!interactive()) {
          question1 <- "y"
        }
        
        if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
          if (any(grepl("_skeleton.qmd", list.files(subdir)))) {
            file.remove(file.path(subdir, (list.files(subdir)[grep("_skeleton.qmd", list.files(subdir))])))
          }
          file.copy(file.path(current_folder, files_to_copy), new_folder, overwrite = TRUE) |> suppressWarnings()
          file.copy(before_body_file, supdir, overwrite = FALSE) |> suppressWarnings()
          create_titlepage_tex(office = office, subdir = supdir, species = species)
          create_inheader_tex(species = species, year = year, subdir = supdir)
          file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
          file.copy(bib_loc, subdir, overwrite = TRUE) |> suppressWarnings()
          file.copy(system.file("resources", "us_doc_logo.png", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
          file.copy(system.file("glossary", "report_glossary.tex", package = "asar"), subdir, overwrite = FALSE) |> suppressWarnings()
          if (tolower(format) == "html") file.copy(system.file("resources", "formatting_files", "theme.scss", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
        } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
          cli::cli_alert_warning("Report template files were not copied into your directory.")
          cli::cli_alert_info("If you wish to update the template with new parameters or output files, please edit the {report_name} in your local folder.",
                              wrap = TRUE
          )
        }
      }
    }
    
    # Handle legacy document order and migration
    fig_info <- migrate_legacy_docs(subdir, doc_type = "figures", rerender_skeleton = rerender_skeleton)
    tbl_info <- migrate_legacy_docs(subdir, doc_type = "tables", rerender_skeleton = rerender_skeleton)
    
    using_legacy_doc_order <- fig_info$using_legacy || tbl_info$using_legacy
    
    if (using_legacy_doc_order) {
      file.rename(from = fs::path(subdir, tbl_info$legacy_name), to = fs::path(subdir, tbl_info$current_name))
      file.rename(from = fs::path(subdir, fig_info$legacy_name), to = fs::path(subdir, fig_info$current_name))
      
      cli::cli_alert_info("Detected legacy figure/table document order in the skeleton. asar will switch to {.file {fig_info$current_name}} before {.file {tbl_info$current_name}}.")
    }
    
    # Created tables doc
    if (!rerender_skeleton) {
      tables_doc_name <- switch(type,
                                "nemt" = "06_tables.qmd",
                                "safe" = "12_tables.qmd",
                                "09_tables.qmd"
      )
      tables_doc <- ""
      utils::capture.output(cat(tables_doc),
                            file = fs::path(subdir, tables_doc_name),
                            append = FALSE
      ) |>
        suppressMessages() |>
        suppressWarnings()
      
      create_tables_doc(
        subdir = subdir,
        tables_dir = tables_dir
      )
    } else {
      tables_doc_name <- if (using_legacy_doc_order) {
        tbl_info$current_name
      } else {
        list.files(file_dir, pattern = "tables.qmd")
      }
    }
    
    # Create figures qmd
    if (!rerender_skeleton) {
      figures_doc_name <- switch(type,
                                 "nemt" = "05_figures.qmd",
                                 "safe" = "11_figures.qmd",
                                 "08_figures.qmd"
      )
      
      create_figures_doc(
        subdir = subdir,
        figures_dir = figures_dir,
        figures_doc_name = figures_doc_name
      )
      if (figures_doc_name != "08_figures.qmd") {
        file.rename(
          from = fs::path(subdir, "08_figures.qmd"),
          to = fs::path(subdir, figures_doc_name)
        )
      }
    } else {
      figures_doc_name <- if (using_legacy_doc_order) {
        fig_info$current_name
      } else {
        list.files(file_dir, pattern = "figures.qmd")
      }
    }
    
    # Part I: YAML & Preamble
    if (title == "[TITLE]") {
      if (rerender_skeleton) {
        old_title <- sub("title: ", "", prev_skeleton[grep("title:", prev_skeleton)])
        if (old_title == "'Stock Assessment Report Template'" || !is.null(office) || species != "species" || !is.null(region) || year != format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y") || !is.null(spp_latin)) {
          title <- create_title(
            office = office,
            species = species,
            spp_latin = spp_latin,
            region = region,
            type = type,
            year = ifelse(is.na(year), format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y"), year)
          )
        }
      } else {
        title <- create_title(
          office = office,
          species = species,
          spp_latin = spp_latin,
          region = region,
          type = type,
          year = year
        )
      }
    }
    
    author_list <- add_authors(
      prev_skeleton = ifelse(rerender_skeleton, prev_skeleton, NULL),
      authors = authors,
      rerender_skeleton = rerender_skeleton
    )
    
    parameters <- TRUE
    param_names <- custom_params |> names()
    param_values <- custom_params |> unname()
    
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
      custom_params = custom_params,
      bib_name = bib_name,
      bib_file = bib_file,
      year = year,
      type = type
    )
    
    if (!rerender_skeleton) cli::cli_alert_success("Built YAML header.")
    
    ##### Params chunk ----
    if (rerender_skeleton) {
      params_chunk_start <- grep("R_parameters", prev_skeleton) - 1
      if (!any(grepl("R_parameters", prev_skeleton)) & parameters) {
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
      } else if (parameters) {
        params_chunk_end <- grep("```", prev_skeleton)[which(grep("```", prev_skeleton) > params_chunk_start)][1]
        params_chunk <- prev_skeleton[params_chunk_start:params_chunk_end]
        if (!is.null(region) & !any(grepl("region <- params$region", params_chunk))) {
          params_chunk <- append(
            params_chunk,
            "region <- params$region",
            after = params_chunk_end - 1
          )
        }
        if (!is.null(param_values) & !is.null(param_names)) {
          for (i in length(param_values)) {
            add_param <- glue::glue("{param_names[i]} <- params${param_names[i]}")
            params_chunk <- append(
              params_chunk,
              add_param,
              after = params_chunk_end - 1
            )
          }
        }
      }
    } else {
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
    }
    
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
    if (!is.null(model_results)) {
      load_method <- glue::glue("load({deparse(substitute(model_results))}) \n")
    } else {
      load_method <- ""
    }
    
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
        "output <- out_new |> \n",
        "  ", "dplyr::mutate(estimate = as.numeric(estimate), \n",
        "  ", "  ", "uncertainty = as.numeric(uncertainty)) \n",
        "source(\"preamble.R\") \n",
        "# Available quantities\n",
        "start_year\n",
        "end_year\n",
        "Fend # terminal fishing mortality\n",
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
    
    if (rerender_skeleton) {
      question1 <- readline("Update the preamble to match entered arguments? (Y/N)")
      if (!interactive()) {
        question1 <- "n"
      }
      if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
        start_line <- grep("label: 'preamble'", prev_skeleton) - 1
        end_line <- grep("```", prev_skeleton)[grep("```", prev_skeleton) > start_line][1]
        preamble <- prev_skeleton[start_line:end_line]
        
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
          prev_results_line <- grep("output <- ", preamble)[1]
          prev_results <- stringr::str_replace(
            preamble[prev_results_line],
            "(?<=output\\s{0,5}<-).*",
            deparse(substitute(model_results))
          )
          prev_results <- paste0(prev_results, " |>")
          preamble <- append(preamble, prev_results, after = prev_results_line)[-prev_results_line]
          
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
                after = chunk_eval_line
              )[-chunk_eval_line],
              collapse = "\n"
            )
          }
          preamble <- paste(preamble, collapse = "\n")
        } else {
          cli::cli_alert_info("Preamble maintained.")
          cli::cli_alert_info("Model results not updated.")
          preamble <- paste(preamble, collapse = "\n")
        }
      } else if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
        cli::cli_alert_warning("Report template files were not copied into your directory.")
        cli::cli_alert_info("If you wish to update the template with new parameters or output files, please edit the {report_name} in your local folder.",
                            wrap = TRUE
        )
      }
    }
    
    ##### Disclaimer ----
    disclaimer <- "{{< pagebreak >}}\n\n## Disclaimer {.unnumbered .unlisted}\n\nThese materials do not constitute a formal publication and are for information only. They are in a pre-review, pre-decisional state and should not be formally cited or reproduced. They are to be considered provisional and do not represent any determination or policy of NOAA or the Department of Commerce.\n"
    
    ##### Citation ----
    if (rerender_skeleton) {
      citation <- prev_skeleton[grep("Please cite this publication as:", prev_skeleton) + 2]
      if (!is.null(authors)) {
        authors_in_skel <- prev_skeleton[grep("  - name: ", prev_skeleton)]
        authors_in_skel <- stringr::str_remove_all(authors_in_skel[seq(1, length(authors_in_skel), 2)], "^.*- name: '|'$")
        authors <- ifelse(
          authors_in_skel == "FIRST LAST",
          names(authors),
          c(authors_in_skel, names(authors))
        )
        
        cit_authors <- format_citation_authors(authors)
        
        if (authors_in_skel[1] == "FIRST LAST") {
          citation <- stringr::str_replace(
            citation,
            "\\[AUTHOR NAME\\].",
            cit_authors
          )
        } else {
          citation <- stringr::str_replace(
            citation,
            "^.*?(?=\\s\\d{4}\\.)",
            cit_authors
          )
        }
      }
      
      if (!is.null(species) | !is.null(region) | !is.null(spp_latin)) {
        citation <- stringr::str_replace(
          citation,
          "(?<=\\d{4}\\.\\s).*?(?=\\.\\sNOAA Fisheries)",
          title
        )
      }
      cli::cli_alert_success("Added report citation.")
    } else {
      citation <- create_citation(
        authors = authors,
        title = title,
        year = year
      )
      cli::cli_alert_success("Added report citation.")
    }
    
    ##### Create report outline ----
    if (!is.null(new_section) || !is.null(custom_sections)) custom <- TRUE
    
    if (rerender_skeleton & is.null(custom_sections)) {
      sections <- stringr::str_extract_all(
        prev_skeleton,
        "(?<=['`])[^']+\\.qmd(?=['`])"
      ) |>
        unlist() |>
        purrr::discard(~ .x == "")
      
      if (using_legacy_doc_order) {
        sections <- sections |>
          stringr::str_replace_all(tbl_info$legacy_name, tbl_info$current_name) |>
          stringr::str_replace_all(fig_info$legacy_name, fig_info$current_name)
        
        figure_position <- which(sections == fig_info$current_name)
        table_position <- which(sections == tbl_info$current_name)
        if (length(figure_position) == 1 && length(table_position) == 1 && figure_position > table_position) {
          sections <- sections[sections != fig_info$current_name]
          table_position <- which(sections == tbl_info$current_name)
          sections <- append(
            sections,
            fig_info$current_name,
            after = table_position - 1
          )
        }
      }
      
      sections <- add_child(
        sections,
        label = gsub(".qmd", "", unlist(sections))
      )
    } else if (is.null(custom_sections)) {
      sections <- add_child(
        sort(c(files_to_copy, tables_doc_name, figures_doc_name)),
        label = stringr::str_extract(sort(c(files_to_copy, tables_doc_name, figures_doc_name)), "(?<=_).+(?=\\.qmd$)")
      )
    } else {
      if (is.null(new_section)) {
        section_list <- add_base_section(files_to_copy)
        sections <- add_child(section_list,
                              label = stringr::str_extract(unlist(section_list), "(?<=_).+(?=\\.qmd$)")
        )
      } else {
        if (is.null(custom_sections)) {
          sec_list1 <- unique(c(files_to_copy, tables_doc_name, figures_doc_name))
          sec_list2 <- add_section(
            new_section = new_section,
            section_location = section_location,
            custom_sections = sec_list1,
            subdir = subdir
          )
          
          sections <- add_child(
            sec_list2,
            label = stringr::str_remove_all(unlist(sec_list2), "^\\d{2}[a-zA-Z]?_|\\.qmd$")
          )
        } else {
          sec_list1 <- unique(c(unlist(add_base_section(files_to_copy)), tables_doc_name, figures_doc_name))
          if (any(stringr::str_replace(section_location, "^[a-z]+-", "") %notin% custom_sections)) {
            cli::cli_abort("Defined customizations do not match one or all of the relative placement of a new section. Please review inputs.")
          }
          sec_list1 <- sec_list1[order(names(stats::setNames(sec_list1, sec_list1)))]
          
          sec_list2 <- add_section(
            new_section = new_section,
            section_location = section_location,
            custom_sections = sec_list1,
            subdir = subdir
          )
          sections <- add_child(
            sec_list2,
            label = stringr::str_remove_all(unlist(sec_list2), "^\\d{2}[a-zA-Z]?_|\\.qmd$")
          )
        }
      }
    }
    
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
    utils::capture.output(cat(report_template), file = file.path(subdir, ifelse(rerender_skeleton, new_report_name, report_name)), append = FALSE)
    if (length(grep("skeleton.qmd", list.files(file_dir, pattern = "skeleton.qmd"))) > 1) {
      question1 <- readline("Deleting previous skeleton file... Do you want to proceed? (Y/N)")
      
      if (!interactive()) {
        question1 <- "y"
      }
      
      if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
        file.remove(file.path(file_dir, report_name))
      } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
        cli::cli_alert_info("Skeleton file retained.")
      }
    }
    
    ##### Final message ----
    if (rerender_skeleton) {
      cli::cli_alert_success("Updated report skeleton in directory {subdir}.")
    } else {
      cli::cli_alert_success("Saved report template in directory {subdir}.")
      cli::cli_alert_info("To proceed, please edit sections within the report template in order to produce a completed stock assessment report.",
                          wrap = TRUE
      )
    }
  } else {
    #### Previous template call ----
    if (!is.null(region)) {
      olddir <- fs::path(file_dir, "report")
      invisible(file.copy(file.path(olddir, list.files(olddir)), subdir, recursive = FALSE))
    } else {
      olddir <- fs::path(file_dir, "report")
      invisible(file.copy(file.path(olddir, list.files(olddir)), subdir, recursive = FALSE))
    }
    
    skeleton <- list.files(subdir, pattern = "skeleton.qmd")
    
    svDialogs::dlg_message("Reminder: Changes should be made when calling an old report. Please change 1) the year in the citation and 2) the location and name of the results file in the first chunk of the report.",
                           type = "ok"
    )
  }
}