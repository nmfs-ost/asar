call_report <- function(
    file_dir = getwd(),
    previous_file_dir,
    author = NULL,
    model_results = NULL,
    year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y"),
    format = "pdf",
    region = NULL, # just in case this changes
    new_section = NULL,
    section_location = NULL
) {
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
  if (!dir.exists(previous_file_dir)) {
      stop("The previous report directory does not exist.")
  }
  
  # Create the report directory if it doesn't exist
  report_dir <- file.path(file_dir, "report")
  if (!dir.exists(report_dir)) {
      dir.create(report_dir)
  }
  
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
  
  
}