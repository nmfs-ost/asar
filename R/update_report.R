#' Call previous assessment report and update
#'
#' @inheritParams create_template
#' @param file_dir String of the path where the new report folder and files 
#' should be located.
#' @param previous_file_dir String of the path where the previous report files 
#' are located.
#'
#' @returns Creates a new folder of pre-filled assessment report files for the 
#' next assessment cycle.
#' @export
#'
#' @examples
update_report <- function(
    file_dir = getwd(),
    previous_file_dir, # required
    authors = NULL,
    model_results = NULL,
    year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y"),
    format = "pdf",
    region = NULL, # just in case this changes
    new_section = NULL,
    section_location = NULL
) {
  #### set up ----
  # Add "report" to previous report file path - user does not have to include this
  previous_file_dir <- glue::glue("{previous_file_dir}/report")
  
  # Identify report type
  type <- stringr::str_extract(
    list.files(previous_file_dir, pattern = "skeleton\\.qmd"),
    # find characters before the first _
    "(?<=^)[^_]+"
  )
  if (type == "SAR") type <- "skeleton"
  
  # Check if the previous report directory exists
  if (!dir.exists(file.path(previous_file_dir, "report"))) {
    stop("The previous report directory does not exist.")
  }
  
  # Create the report directory if it doesn't exist
  report_dir <- file.path(file_dir, "report")
  if (!dir.exists(report_dir)) {
    dir.create(report_dir)
  }
  
  #### copy files ----
  # Copy previous assessment files over
  prev_files <-list.files(
    previous_file_dir, 
    # qmd, bib files, glossary, and preamble
    pattern = "\\.qmd$|\\.bib$|report_glossary\\.tex$|preamble\\.R$"
  )
  file.copy(glue::glue("{previous_file_dir}/{prev_files}"), report_dir)
  # Copy support files
  prev_support_files <- list.files(file.path(previous_file_dir, 'support_files'), full.names = TRUE)
  # create supporting files folder
  supdir <- file.path(report_dir, 'support_files')
  if (!dir.exists(supdir)) {
    dir.create(supdir)
  }
  file.copy(
    prev_support_files, 
    supdir, 
    recursive = TRUE
  )
  # warning which files are not in the standard framework
  std_files <- list.files(file.path(system.file("templates", package = "asar"), type))
  # select "section" qmd from prev_files and remove skeleton, figures, and tables docs
  prev_file_outline <- prev_files[grepl("\\.qmd$", prev_files)]
  prev_file_outline <- prev_file_outline[!grepl("skeleton|figures|tables", prev_file_outline)]
  non_std_files <- setdiff(prev_file_outline, std_files)
  if (length(non_std_files) > 0) cli::cli_alert_info("Non-standard section files exist.")
  
  #### reset tables and figures docs ----
  
  if (any(grepl("figures\\.qmd$", prev_files))) {
    reset_figures <- readline("Figures document already exists. Do you want to reset it? [y/n]")
    if (!interactive()) {
      reset_figures <- "y"
    }
    if (regexpr(reset_figures, "y", ignore.case = TRUE) == 1) {
      # Remove previous file
      file.remove(
        stringr::str_match(prev_files, "figures")
      )
      # Create figures doc
      create_figures_doc(
        subdir = report_dir
      )
      cli::cli_alert_info("Figures document reset to default.")
    } else if (regexpr(reset_figures, "n", ignore.case = TRUE) == 1) {
      cli::cli_alert_info("Previous assessment figures qmd retained.")
    }
  }
  
  if (any(grepl("tables\\.qmd$", prev_files))) {
    reset_tables <- readline("Tables document already exists. Do you want to reset it? [y/n]")
    if (!interactive()) {
      reset_tables <- "y"
    }
    if (regexpr(reset_tables, "y", ignore.case = TRUE) == 1) {
      # Remove previous file
      file.remove(
        stringr::str_match(prev_files, "tables")
      )
      # Create tables doc
      create_tables_doc(
        subdir = report_dir
      )
      cli::cli_alert_info("Tables document reset to default.")
    } else if (regexpr(reset_tables, "n", ignore.case = TRUE) == 1) {
      cli::cli_alert_info("Previous assessment tables qmd retained.")
    }
  }
  
  #### update skeleton ----
  # part of skeleton:
  # yaml
  # disclaimer
  # citation
  # preamble
  # section chunks
  # Run create_template but with rerender_skeleton = TRUE
  # TODO: change once rerender is outside of create_template
  # TODO: reset author section in skeleton -- remove all previous authorship (does this work?)
  
  create_template(
    rerender_skeleton = TRUE,
    dir = file_dir,
    authors = authors,
    model_results = model_results,
    year = year,
    format = format,
    region = region,
    new_section = new_section,
    section_location = section_location
  )
  
}
