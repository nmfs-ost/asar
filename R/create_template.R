#' Create Stock Assessment Report Template
#'
#' @param new_template TRUE/FALSE; default is false otherwise if true, will pull the last saved stock assessment report skeleton
#' @param tempdir directory for the templates/
#' @param format file type for the render (i.e. pdf, docx, html)
#' @param office regional fisheries science center producing the report (AFSC, NEFSC, NWFSC, PIFSC, SEFSC, SWFSC)
#' @param region Full name of region in which the species is evaluated if applicable; Note: if this is not specified for your center or for the species, do not use this variable.
#' @param complex Is this a species complex? "YES" or "NO"
#' @param species full common name for target species, split naming by a space and capitalize first letter(s)
#' @param year year the assessment is being conducted, default is current year report is being rendered
#' @param author List of authors to include in the assessment
#' @param include_affiliation TRUE/FALSE; does the analyst want to include affiliations of the authors in the document?
#' @param simple_affiliation If including affiliation, adding just office name rather than full address; TRUE/FALSE, default is TRUE
#' @param alt_title TRUE/FALSE create an alternative title than the default
#' @param title Name of new title if default is not appropriate; Example, "Management Track Assessments Spring 2024"
#' @param parameters TRUE/FALSE; default TRUE - for parameterization of the script
#' @param param_names List of parameter names that will be called in the document; parameters automatically included are office, region, and species listed in function parameters
#' @param param_values List of values associated with the order of parameter names; parameters automatically included are office, region, and species listed in function parameters
#' @param resdir Directory where the model results file(s) are located
#' @param model_results Name of the model results file
#' @param model Type of assessment model that was used to assess the stock (i.e. "BAM", "SS", "AMAK", "ASAP", ect)
#' @param add_section TRUE/FALSE; is there an additional section that the analyst wants to add to the skeleton? Default is false
#' @param secdir Directory where the .qmd file is located (new file made by the analyst)
#' @param new_section File name of the new section
#' @param section_location Location where the section should be added relative to the base skeleton document
#' @param type Type of stock assessment report - terminology will vary by region (content already configured by region)
#'
#' @return Create template and pull skeleton for a stock assessment report.
#'         Function builds a YAML specific to the region and utilizes current
#'         resources and workflows from different U.S. Fishery Science Centers.
#'         General sections are called as child documents in this skeleton and
#'         each of the child documents should be edited separately.
#' @export
#'
#' @examples
create_template <- function(
    new_template = TRUE,
    tempdir = here::here(),
    format = c("pdf","docx","html", NULL),
    office = c("AFSC","PIFSC","NEFSC","NWFSC","SEFSC","SWFSC"),
    region = NULL,
    complex = "NO",
    species = NULL,
    year = NULL,
    author = NULL,
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
    add_section = FALSE,
    secdir = NULL,
    new_section = NULL,
    section_location = NULL,
    type = c("OA","UP","RT","FULL","MT")
    ){

  setwd(dir = tempdir)
  # If analyst forgets to add year, default will be the current year report is being produced
  if(is.null(year)){
    year <- format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"),"%Y")
  }

  # Name report
  report_name <- paste0(type,
                        "_")
  if(!is.null(region)){
    report_name <- paste0(report_name,
                          gsub("(\\b[A-Z])[^A-Z]+", "\\1", region))
  }
  report_name <- paste0(report_name, "_",
                        gsub(" ", "_", species),
                        "_skeleton.qmd")

  # Select parameter from list
  format <- match.arg(format, several.ok = FALSE)
  office <- match.arg(office, several.ok = FALSE)
  type <- match.arg(type, several.ok = FALSE)

  if(!is.null(region)){
    subdir <- here::here('inst', 'templates', 'archive', office, species, region, year)
  } else {
    subdir <- here::here('inst', 'templates', 'archive', office, species, year)
  }
  # Always creating new directory for each assessment since they will each change
  # Allow NOAA to keep a record of each assessment file
  # These will need to be cataloged into a cloud system somehow
  if(!dir.exists(here::here('inst','templates', 'archive', office, species))){
    dir.create(here::here('inst','templates', 'archive', office, species))
  }

  if(!is.null(region)){
    if(!dir.exists(here::here('inst','templates', 'archive', office, species, region))){
      dir.create(here::here('inst','templates', 'archive', office, species, region))
    }
    # Create new folder for current year
    if(!dir.exists(here::here('inst','templates', 'archive', office, species, region, year))){
      dir.create(here::here('inst','templates', 'archive', office, species, region, year))
    }
  } else {
    # Create new folder for current year
    if(!dir.exists(here::here('inst','templates', 'archive', office, species, year))){
      dir.create(here::here('inst','templates', 'archive', office, species, year))
    }
  }

  if(new_template==TRUE){
  # Pull skeleton for sections
  current_folder <- here::here('inst','templates','skeleton')
  new_folder <- subdir
  files_to_copy <- list.files(current_folder)
  file.copy(file.path(current_folder, files_to_copy), new_folder)

  # Part I
  # Create a report template file to render for the region and species
  # Create YAML header for document
  # Write title based on report type and region

  title <- write_title()

  # Pull authors and affiliations from national db
  # Parameters to add authorship to YAML
  # Read authorship file
  authors <- read.csv(here::here('inst', 'resources', 'authorship.csv')) |>
    dplyr::mutate(name = dplyr::case_when(is.na(mi) ~ paste(first, last, sep = " "),
                                          TRUE ~ paste(first, mi, last, sep = " "))) |>
    dplyr::select(name, office) |>
    dplyr::filter(name %in% author)
  if(include_affiliation==TRUE){
    affil <- read.csv(here::here('inst', 'resources', 'affiliation_info.csv'))
  }
  author_list <- list()

  if(include_affiliation==TRUE & simple_affiliation==FALSE){
    for(i in 1:length(authors)){
      auth <- authors[i,]
      aff <- affil |>
        dplyr::filter(affiliation==auth$office)
      paste0("  ", "- name: ", "'", auth$name, "'", "\n",
             "  ", "  ", "affiliations:", "\n",
             "  ", "  ", "  ", "- name: ", "'", "NOAA Fisheries ", aff$name, "'", "\n",
             "  ", "  ", "  ", "  ", "address: ", "'", aff$address, "'","\n",
             "  ", "  ", "  ", "  ", "city: ", "'", aff$city, "'","\n",
             "  ", "  ", "  ", "  ", "state: ", "'", aff$state, "'","\n",
             "  ", "  ", "  ", "  ", "postal-code: ", "'", aff$postal.code, "'","\n"
             # sep = " "
      ) -> author_list[[i]]
    }
  } else if(include_affiliation==TRUE & simple_affiliation==TRUE){
    for(i in 1:length(authors)){
      auth <- authors[i,]
      aff <- affil |>
        dplyr::filter(affiliation==auth$office)
      paste0("  ", "- name: ", "'", auth$name, "'", "\n",
             "  ", "  ", "affiliations: ", "'", aff$name, "'", "\n"
      ) -> author_list[[i]]
    }
  } else {
    for(i in 1:nrow(authors)){
      auth <- authors[i,]
      paste0("  ", "- ", "'", auth$name, "'", "\n") -> author_list[[i]]
    }
  }

  # Creating YAML
  yaml <- paste0(
    # start YAML notation
    "---", "\n",

    # Tile
    "title: ", "'", title, "'", "\n",

    # Author
    "author:", "\n")
  # Add authors
  add_authors <- NULL
  for (i in 1:length(author_list)) {
    toad <- paste(author_list[[i]], sep =",")
    add_authors <- paste0(add_authors, toad) # -> add_authors
  }
  yaml <- paste0(yaml, add_authors)

  # Add other parts
  yaml <- paste0(yaml,
                 # Date
                 "date: today", "\n"
                 )

  # Format
  if(include_affiliation==TRUE){

    yaml <- paste(yaml, "format: \n",
                  "  " , format, ": \n",
                  "  ", "  ", "keep-tex: ", "true \n",
                  "  " , "  ", "template-partials: \n",
                  "  ", "  ", "  ", " - title.tex \n",
                  "  ", "  ", "include-in-header: \n",
                  "  ", "  ", "  ", "text: | \n",
                  "  ", "  ", "  ", "  ", "\\usepackage[noblocks]{authblk} \n",
                  "  ", "  ", "  ", "  ", "\\renewcommand*{\\Authsep}{, } \n",
                  "  ", "  ", "  ", "  ", "\\renewcommand*{\\Authand}{, } \n",
                  "  ", "  ", "  ", "  ", "\\renewcommand*{\\Authands}{, } \n",
                  "  ", "  ", "  ", "  ", "\\renewcommand\\Affilfont{\\small} \n",
                  sep = "")

  } else if(include_affiliation==FALSE){

    yaml <- paste0(yaml, "format: \n",
                   "  ", format, ": \n",
                   "  ", "  ", "template-partials: \n",
                   "  ", "  ", "  ", "- sadraft.tex \n",
                   # This will need edits
                   report_format(...),
                   "  ", "  ", "keep-tex: true \n"
                   )

  }

  if(parameters==TRUE){
    if(!is.null(param_names) & !is.null(param_values)){
      # Parameters
      yaml <- paste0(yaml, "params:", "\n")

      add_params <- NULL
      for (i in 1:length(param_names)) {
        toad <- paste("  ", " ", param_names[i], ": ", "'",param_values[i], "'", "\n", sep="")
        add_params <- paste0(add_params, toad)
      }
    } else {
        print("Please define parameter names (param_names) and values (param_values).")
    }

    yaml <- paste0(yaml, add_params)
  }

  # Close yaml
  yaml <- paste0(yaml, "---")

  # yaml_save <- capture.output(cat(yaml))
  # cat(yaml, file = here('template','yaml_header.qmd'))

  # Add chunk to load in assessment data
  ass_output <- chunkr(
    "convert_output(x)"
  )

  # Create report template

  if(type=="OA" | type=="UP" | type=="MT"){
    sections <- paste(
      # Add executive summary
      paste_child("01_executive_summary.qmd"), "\n",
      "{{< pagebreak >}}",
      # Add introduction
      paste_child("02_introduction.qmd"), "\n",
      "{{< pagebreak >}}",
      sep = "\n"
      )
  } else if (type=="RT" | type=="FULL"){
    sections <- paste(
      # Add executive summary
      paste_child("01_executive_summary.qmd"),
      "{{< pagebreak >}}",
      # Add introduction
      paste_child("02_introduction.qmd"),
      "{{< pagebreak >}}",
      sep = "\n"
    )
  } else {
    print("Type of assessment report is not defined")
  }

  # Combine template sections
  report_template <- paste0(yaml, "\n",
                            ass_output, "\n",
                            sections)

  # Save template as .qmd to render
  utils::capture.output(cat(report_template), file = paste0(subdir, "/", report_name), append = FALSE)
  } else {
    # Copy old template and rename for new year
    # Create copy of previous assessment
    if(!is.null(region)){
      olddir <- here::here("templates", 'archive', office, species, region, prev_year)
      file.copy(here::here("templates", 'archive', office, species, region, prev_year, (list.files(olddir))), subdir, recursive = TRUE)
    } else {
      olddir <- here::here("templates", 'archive', office, species, prev_year)
      file.copy(here::here("templates", 'archive', office, species, prev_year, (list.files(olddir))), subdir, recursive = TRUE)
    }

    # Open previous skeleton
    skeleton <- list.files(subdir, pattern = "skeleton.qmd")
    utils::file.edit(subdir, pattern = skeleton)
  }
}
