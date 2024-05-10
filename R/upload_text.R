upload_text <- function(
    office = NULL,
    region = NULL,
    species = NULL,
    year = NULL) {

  # Files in skeleton
  if (!is.null(region)) {
    subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", region, "/", year)
  } else {
    subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", year)
  }

  all_sections <- list.files(subdir, pattern = ".qmd")
  sections <- all_sections[!grepl("skeleton.qmd", all_sections)]

  for (i in 1:length(sections)) {
    gdoc <- gsub(".qmd","", sections[i])
    if (!is.null(region)) {
      trackdown::upload_file(
        file = paste(subdir, sections[i], sep = "/"),
        gfile = gdoc,
        gpath = paste("National Stock Assessment Report Archive", office, region, species, year, sep = "/")
        # shared_drive = "National Stock Assessment Report Archive"
      )
    } else {
      upload_file(
        file = paste(subdir, sections[i], sep = "/"),
        gfile = gdoc,
        gpath = paste0("National Stock Assessment Report Archive", "/", office, "/", species, "/", year, "/"),
        # hide_code = TRUE,
        open = FALSE
        # shared_drive = "National Stock Assessment Archive/"
      )
    }
  }
}
