#' Create Stock Assessment Report Template
#'
#' @param new_template TRUE/FALSE; default is false otherwise if true, will pull the last saved stock assessment report skeleton
#' @param tempdir directory for the templates/
#' @param format file type for the render (i.e. pdf, docx, html)
#' @param office regional fisheries science center producing the report (AFSC, NEFSC, NWFSC, PIFSC, SEFSC, SWFSC)
#' @param region Abbreviated region in which the species is evaluated if applicable; i.e. GOM, SA, BSAI, GOA. Note: if this is not specificed for your science center then do not use this variable. The specific location for the species should be added as a parameter in param_names and param_values
#' @param complex Is this a species complex? "YES" or "NO"
#' @param species full common name for target species, split naming by a space and capitalize first letter(s)
#' @param year year the assessment is being conducted, default is current year report is being rendered
#' @param new_author TRUE/FALSE; default is FALSE - pulls from list of authors in repo
#' @param author_name "First Last" name of author being added to the report
#' @param author_office Abbreviation of the science center of the author being added
#' @param include_affiliation TRUE/FALSE; does the analyst want to include affiliations of the authors in the document?
#' @param parameters TRUE/FALSE; default TRUE - for parameterization of the script
#' @param param_names List of parameter names that will be called in the document; example: c("office", "region", "spp")
#' @param param_values List of values associated with the order of parameter names; example: c("SEFSC", "Gulf of Mexico", "Red Snapper")
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
    new_author = FALSE,
    author_name = NULL,
    author_office = NULL,
    include_affiliation = FALSE,
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
  report_name <- paste0(type, "_", region, "_", gsub(" ", "_", species), "_skeleton.qmd")

  # Select parameter from list
  format <- match.arg(format, several.ok = FALSE)
  office <- match.arg(office, several.ok = FALSE)
  type <- match.arg(type, several.ok = FALSE)

  subdir <- here::here('inst', 'templates', 'archive', office, region, species, year)
  # Always creating new directory for each assessment since they will each change
  # Allow NOAA to keep a record of each assessment file
  # These will need to be cataloged into a cloud system somehow
  if(!dir.exists(here::here('inst','templates', 'archive', office, region))){
    dir.create(here::here('inst','templates', 'archive', office, region))
  }

  if(!dir.exists(here::here('inst','templates', 'archive', office, region, species))){
    dir.create(here::here('inst','templates', 'archive', office, region, species))
  }
  # Create new folder for current year
  if(!dir.exists(here::here('inst','templates', 'archive', office, region, species, year))){
    dir.create(here::here('inst','templates', 'archive', office, region, species, year))
    }

  if(new_template==TRUE){
  # Pull skeleton for sections
  current_folder <- here::here('inst','templates','skeleton')
  new_folder <- subdir
  files_to_copy <- list.files(current_folder)
  file.copy(file.path(current_folder, files_to_copy), new_folder)
  # Part I
  # Create a report template file to render for the region and species

  # Create .yml for document

  # Create title dependent on regional language
  if(office=="AFSC"){
    if(complex=="no"){
      title = paste0("Assessment of the ", species, " Stock in the ", region)
    } else {
      title = paste0("Assessment of the ", species, " Stock Complex in the ", region)
    }
  } else if(office=="NEFSC"){
    if(type=="RT"){
      title = paste0("Report of the ", species, " ", year, " Research Track Assessment ", "Working Group")
    } else {
      title = paste0("Management Track Assessment of ", species, " ", year)
    }

  } else if(office=="NWFSC"){

  } else if(office=="PIFSC"){

  } else if(office=="SEFSC"){

  } else if(office=="SWFSC"){

  } else {
    print("office (FSC) is not defined. Please define which office you are associated with.")
  }

  # Will only run if adding a new author to the resources
  # This needs a better call and fix for assessment reports

  if(new_author==TRUE){
    # author_first <- sapply(strsplit(author_name," "), `[`, 1)
    # author_last <- sapply(strsplit(author_name," "), `[`, 2)
    # office <- author_office
    new_author <- paste(author_name, author_office)
    write(new_author, file = here::here("inst", "resources", "authorship.txt"), append = TRUE)
  }

  # Parameters to add authorship to YAML
  # Read authorship file
  authors <- readr::read.delim(here::here('inst', 'resources', 'authorship.txt'), header=TRUE, sep=" ") |>
    dplyr::filter(region==office)
  affil <- qtl2::read.csv(here::here('inst', 'resources', 'affiliation_info.csv'))
  author_list <- list()

  if(include_affiliation==TRUE){
    for(i in 1:nrow(authors)){
      auth <- authors[i,]
      aff <- affil |>
        dplyr::filter(affiliation==auth$office)
      paste0("  ", "- name: ", "'", auth$first, " ", auth$last, "'", "\n",
             "  ", "  ", "affiliations:", "\n",
             "  ", "  ", "  ", "- name: ", "'", "NOAA Fisheries ", aff$name, "'", "\n",
             "  ", "  ", "  ", "  ", "address: ", "'", aff$address, "'","\n",
             "  ", "  ", "  ", "  ", "city: ", "'", aff$city, "'","\n",
             "  ", "  ", "  ", "  ", "state: ", "'", aff$state, "'","\n",
             "  ", "  ", "  ", "  ", "postal-code: ", "'", aff$postal.code, "'","\n"
             # sep = " "
      ) -> author_list[[i]]
    }
  } else {
    for(i in 1:nrow(authors)){
      auth <- authors[i,]
      paste0("  ", "- ", "'", auth$first, " ", auth$last, "'", "\n") -> author_list[[i]]
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

    # yaml <- paste0(yaml, "format: ", format, "\n")

    yaml <- paste(yaml, "format: \n",
                  "  " , format, ": \n",
                  "  ", "  ", "keep-tex: ", "true \n",
                  "  " , "  ", "template-partials: \n",
                  "  ", "  ", "  ", " - title.tex \n",
                  "  ", "  ", "  ", " - sadraft.tex \n",
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

  # Add report formatting based on report type (operational/research)
  # if(type=="OA" | type=="UP" | type=="MT"){
  #   # Add report formatting based on region
  #   if(office=="AFSC"){
  #
  #   } else if(office=="NEFSC"){
  #
  #   } else if(office=="NWFSC"){
  #     template <- paste0(
  #       "output: ",
  #       "  ", "sa"
  #     )
  #   } else if(office=="PIFSC"){
  #
  #   } else if(office=="SEFSC"){
  #
  #   } else if(office=="SWFSC"){
  #
  #   } else {
  #     print("office (FSC) is not defined. Please define which office you are associated with.")
  #   }
  # } else if (type=="RT" | type=="FULL"){
  #   # Add report formatting based on region
  #   if(office=="AFSC"){
  #
  #   } else if(office=="NEFSC"){
  #
  #   } else if(office=="NWFSC"){
  #
  #   } else if(office=="PIFSC"){
  #
  #   } else if(office=="SEFSC"){
  #
  #   } else if(office=="SWFSC"){
  #
  #   } else {
  #     print("office (FSC) is not defined. Please define which office you are associated with.")
  #   }
  # } else {
  #   print("Type of assessment report is not defined")
  # }

  # Close yaml
  yaml <- paste0(yaml, "---")

  # yaml_save <- capture.output(cat(yaml))
  # cat(yaml, file = here('template','yaml_header.qmd'))

  # Create report template

  if(type=="OA" | type=="UP" | type=="MT"){
    sections <- paste(
      # Add executive summary
      paste_child("00_executive_summary.qmd"),
      "{{< pagebreak >}}",
      # Add introduction
      paste_child("01_introduction.qmd"),
      "{{< pagebreak >}}",
      sep = "\n"
      )
  } else if (type=="RT" | type=="FULL"){
    sections <- paste(
      # Add executive summary
      paste_child("00_executive_summary.qmd"),
      "{{< pagebreak >}}",
      # Add introduction
      paste_child("01_introduction.qmd"),
      "{{< pagebreak >}}",
      sep = "\n"
    )
  } else {
    print("Type of assessment report is not defined")
  }

  # Combine template sections
  report_template <- paste0(yaml,
                            "\n",
                            sections)

  # Save template as .qmd to render
  utils::capture.output(cat(report_template), file = here::here("inst", "templates", 'archive', office, region, species, year, report_name))
  } else {
    # Copy old template and rename for new year
    # Create copy of previous assessment
    olddir <- here::here("templates", 'archive', office, region, species, prev_year)
    file.copy(here::here("templates", 'archive', office, region, species, prev_year, (list.files(olddir))), subdir, recursive = TRUE)

    # Open previous skeleton
    skeleton <- list.files(subdir, pattern = "skeleton.qmd")
    utils::file.edit(subdir, pattern = skeleton)
  }
}
