#' Call previous assessment report and update
#'
#' @inheritParams create_template
#' @inheritParams create_figures_dir
#' @inheritParams create_tables_dir
#' @param file_dir String of the path where the new report folder and files 
#' should be located. Required.
#' @param previous_file_dir String of the path where the previous report files 
#' are located. Required.
#' @param reset_tables_and_figures Logical indicating whether to reset tables 
#' and figures Quarto documents.
#' @param reset_authors Logical indicating whether to reset authorship for the 
#' report. Only will work if authors argument is also used.
#' @param custom_sections Logical. Indicate whether the previous report had a 
#' customized sectioning. If TRUE, the report outline will mimic the previous 
#' report. If FALSE, the report will include the full standard guidelines outline 
#' along with any custom sectioning from the previous report.
#'
#' Default: FALSE
#' @returns Creates a new folder of pre-filled assessment report files for the 
#' next assessment cycle.
#' 
#' @details
#' This function is designed so a report made from `asar::create_template()` can
#'  be called and updated with the follow:
#'  - year
#'  - model results
#'  - authorship
#'  - check standard structure against current structure
#' This function performs three major tasks (1) copies all files from the old 
#' directory to the new one, (2) updates the skeleton according the the input 
#' arguments using the `rerender_skeleton()` function, and (3) if desired, 
#' resets your figures and tables documents.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' update_report(
#'   previous_file_dir = "~/testing/goa_2023/report",
#'   file_dir = "~/testing/goa_2025",
#'   model_results = "../new_std_res.rda"
#' }
update_report <- function(
    file_dir = getwd(),
    previous_file_dir, # required
    authors = NULL,
    model_results = NULL,
    year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y"),
    format = "pdf",
    region = NULL,
    custom_sections = TRUE,
    new_section = NULL,
    section_location = NULL,
    figures_dir = getwd(),
    tables_dir = getwd(),
    reset_tables_and_figures = FALSE,
    reset_authors = FALSE
) {
  #### set up ----
  # Add "report" to previous report file path - user does not have to include this
  if (!grepl("report",previous_file_dir)) {
    previous_file_dir <- glue::glue("{previous_file_dir}/report")
    if (!dir.exists(previous_file_dir)) {
      stop("The previous report directory does not exist.")
    }
  }
  # check for skeleton file in previous folder
  if (!any(grepl("skeleton.qmd", list.files(previous_file_dir, full.names = FALSE)))) {
    cli::cli_abort("No skeleton file found. Please use `create_template` to generate a new template")
  }
  
  # Identify report type
  type <- stringr::str_extract(
    list.files(previous_file_dir, pattern = "skeleton\\.qmd"),
    # find characters before the first _
    "(?<=^)[^_]+"
  )
  if (tolower(type) == "sar") type <- "skeleton"
  
  # Create the report directory if it doesn't exist
  report_dir <- file.path(file_dir, "report")
  if (!dir.exists(report_dir)) {
    dir.create(report_dir)
  }
  
  #### copy files ----
  # Copy previous assessment files over
  prev_files <- list.files(
    previous_file_dir, 
    # qmd, bib files, glossary, and preamble
    pattern = "\\.qmd$|.bib$|report_glossary\\.tex$|preamble\\.R$|.sty$"
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
  # Copy bib folder
  prev_bib_files <- list.files(file.path(previous_file_dir, 'bibliography_files'), full.names = TRUE)
  # create new folder and copy into
  bibdir <- file.path(report_dir, "bibliography_files")
  if (!dir.exists(bibdir)) {
    dir.create(bibdir)
  }
  file.copy(
    prev_bib_files, 
    bibdir, 
    recursive = TRUE
  )

  #### Update skeleton ----
  # part of skeleton:
  # yaml
  # disclaimer
  # citation
  # preamble
  # section chunks
  # id the order of the files in the skeleton and copy over in that order
  prev_skeleton <- readLines(list.files(previous_file_dir, pattern = "skeleton\\.qmd", full.names = TRUE))
  files_to_copy <- stringr::str_extract(prev_skeleton[grep("knitr::knit_child", prev_skeleton)], "(?<=knit_child\\(').*?(?=\\')")
  
  if (!custom_sections) {
    # Warning which files are not in the standard framework
    std_files <- list.files(file.path(system.file("templates", package = "asar"), type))
    # select "section" qmd from prev_files and remove skeleton, figures, and tables docs
    non_std_files <- setdiff(files_to_copy, std_files)[-grep("skeleton|figures|tables", setdiff(files_to_copy, std_files))]
    # if (length(non_std_files) > 0) cli::cli_alert_info("Non-standard section files exist.")
    if (length(non_std_files) > 0) {
      cli::cli_alert_info("File structure out of date. Updating sections...")
      # add new section
      new_std_sections <- std_files[!std_files %in% files_to_copy]
      # Copy in the new_std_sections
      file.copy(
        file.path(system.file("templates", package = "asar"), type, new_std_sections),
        report_dir, 
        overwrite = FALSE
      )
      if (length(new_std_sections) > 1) cli::cli_alert_info("New sections present in outline resulting from a change in outlines from previous assessment to the standard guidelines. 
                                                          Please review your document.")
      # Find any added non-std sections
      # add new files
      for (i in seq_along(new_std_sections)) {
        sec_num <- stringr::str_extract(new_std_sections[i], "(?<=^)[0-9]+") |> as.numeric()
        # find the next section number in files_to_copy
        sec_nums <- as.numeric(stringr::str_extract(files_to_copy, "(?<=^)[0-9]+"))
        # find the index of the first non-na sec_nums > sec_num
        next_sec_index <- which(sec_nums > sec_num)[1]
        # add section before index in there is no na before it otherwise, add it before the na
        if (is.na(next_sec_index)) {
          index_append <- length(sec_nums)
        } else if (is.na(sec_nums[next_sec_index - 1])) {
          index_append <- next_sec_index - 2
        } else {
          index_append <- next_sec_index - 1
        }
        files_to_copy <- append(files_to_copy, new_std_sections[i], after = index_append)
      }
    }
  }

  # TODO: reset author section in skeleton -- remove all previous authorship (does this work?)
  # Update skeleton with new year, authors, model results, region, if added
  rerender_skeleton(
    file_dir = report_dir,
    authors = authors,
    model_results = model_results,
    year = year,
    format = format,
    region = region,
    custom_sections = files_to_copy,
    new_section = new_section,
    section_location = section_location
  )
  
  # Reset authorship
  # wondering if this should be a feature in rerender_skeleton instead
  if (reset_authors && !is.null(authors)) {
    curr_skeleton <- readLines(list.files(report_dir, pattern = "skeleton\\.qmd", full.names = TRUE))
    # format authorship
    author_list <- add_authors(
      authors = authors,
      rerender_skeleton = FALSE
    )
    # set author_list to lines delineated by "\n"
    author_list <- unlist(stringr::str_split(author_list, "\n"))
    # find location in skeleton for authors
    first_line <- grep("author:", curr_skeleton) + 1
    end_line <- grep("date:", curr_skeleton) - 1
    # replace previous authors sections with author_list
    author_updated_skeleton <- unlist(append(curr_skeleton[-(first_line:end_line)], unlist(author_list), after = first_line - 1))
    writeLines(author_updated_skeleton, list.files(report_dir, pattern = "skeleton\\.qmd", full.names = TRUE))
  }
  
  #### reset tables and figures docs ----
  if (reset_tables_and_figures) {
    # Remove previous file
    file.remove(
      file.path(
        report_dir,
        prev_files[grep("figures.qmd", prev_files)]
      )
    )
    # Create figures doc
    create_figures_doc(
      subdir = report_dir,
      figures_dir = figures_dir
    )
    cli::cli_alert_info("Figures document reset to default.")
    
    # Remove previous file
    file.remove(
      file.path(
        report_dir,
        prev_files[grep("tables.qmd", prev_files)]
      )
    )
    # Create tables doc
    create_tables_doc(
      subdir = report_dir
    )
    cli::cli_alert_info("Tables document reset to default.")
    
    # Rename tables and figs docs if different type
    tables_doc_name <- switch(
      type,
      "nemt" = "06_tables.qmd",
      "safe" = "12_tables.qmd",
      "09_tables.qmd"
    )
    figures_doc_name <- switch(
      type,
      "nemt" = "05_figures.qmd",
      "safe" = "11_figures.qmd",
      "08_figures.qmd"
    )
    # rename figures doc
    if (figures_doc_name != "08_figures.qmd") {
      file.rename(
        from = fs::path(subdir, "08_figures.qmd"),
        to = fs::path(subdir, figures_doc_name)
      )
    }
    
    # rename tables doc
    if (tables_doc_name != "09_tables.qmd") {
      file.rename(
        from = fs::path(subdir, "09_tables.qmd"),
        to = fs::path(subdir, tables_doc_name)
      )
    } # close tables doc name if statement
  } # close reset_tables_and_figures
}
