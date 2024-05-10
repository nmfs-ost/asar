update_text <- function(
    office = NULL,
    region = NULL,
    species = NULL,
    year = NULL,
    force = FALSE) {
  # Kind of wrapper function for trackdown::download_file so all sections in assessment document are updated

  # Files in skeleton
  if (!is.null(region)) {
    subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", region, "/", year)
  } else {
    subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", year)
  }

  all_sections <- list.files(subdir, pattern = ".qmd")
  sections <- all_sections[!grepl("skeleton.qmd", all_sections)]

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
      select_change <- utils::select.list(
        # preselect = "ALL",
        multiple = TRUE,
        c(sections, "ALL", "NONE"),
        title = paste(
          "Which files do you want to update?"
        )
      )
    }

    if (select_change=="ALL"){
      select_change <- sections
    }

    if (select_change==NONE){
      cli::cli_alert_danger("No sections were selected for updates.")
      return(NULL)
    }

    if (response == 2L) {
      cli::cli_alert_danger("Process aborted")
      return(NULL)
    }
  }


  for (i in 1:length(select_change)) {
    gdoc <- gsub(".qmd","", select_change[i])
    if (!is.null(region)) {
      trackdown::download_file(
        file = paste(subdir,select_change[i], sep = "/"),
        gfile = gdoc,
        gpath = paste("National Stock Assessment Report Archive", office, region, species, year, sep = "/")
        # shared_drive = "National Stock Assessment Archives"
      )
    } else {
      trackdown::download_file(
        file = paste(subdir,select_change[i], sep = "/"),
        gfile = gdoc,
        gpath = paste("National Stock Assessment Report Archive", office, species, year, sep = "/")
        # shared_drive = "National Stock Assessment Report Archive"
      )
    }
  }
}
