update_text <- function(
    office = NULL,
    region = NULL,
    species = NULL,
    year = NULL) {
  # Kind of wrapper function for trackdown::download_file so all sections in assessment document are updated

  # Downloading version of google doc content onto local drive
  # Will overwrite current content - add warning box

  # Check if want to proceed bc this overwrites the local file(s)
  if (interactive() && isFALSE(force)) {
    response <- utils::menu(
      c("Yes", "No"),
      title = paste(
        "Downloading the file from Google Drive will overwrite local file.",
        "Do you want to proceed?"
      )
    )

    if (response == 1L) {
      select_change <- utils::menu(
        c("Yes", "No"),
        title = paste(
          "Downloading the file from Google Drive will overwrite local file.",
          "Do you want to proceed?"
        )
      )
    }

    if (response == 2L) {
      cli::cli_alert_danger("Process aborted")
      return(NULL)
    }
  }

  if (select_change == 1L) {

    # directory where templates were created
    if (!is.null(region)) {
      subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", region, "/", year)
    } else {
      subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", year)
    }
  }

  all_sections <- list.files(subdir, pattern = ".qmd")
  sections <- all_sections[!grepl("skeleton.qmd", all_sections)]

  for (i in sections) {
    if (!is.null(region)) {
      trackdown::download_file(
        file = i,
        gfile = i,
        shared_drive = paste("National Stock Assessment Archives", office, region, species, year, sep = "/")
      )
    } else {
      trackdown::download_file(
        file = i,
        gfile = i,
        shared_drive = paste("National Stock Assessment Archives", office, species, year, sep = "/")
      )
    }
  }
}
