#' Create Stock Assessment Report Template
#'
#' To see templates included in the base skeleton, please run
#' 'list.files(system.file('templates','skeleton', package = 'asar'))'
#'  in the console.
#'
#' @param new_template TRUE/FALSE; Create a new template? If true,
#' will pull the last saved stock assessment report skeleton.
#' Default is false.
#' @param format Rendering format (pdf, html, or docx).
#' @param office Regional Fisheries Science Center producing the
#'  report (i.e., AFSC, NEFSC, NWFSC, PIFSC, SEFSC, SWFSC).
#' @param region Full name of region in which the species is
#'  evaluated (if applicable). If the region is not specified for
#'   your center or species, do not use this variable.
#' @param complex TRUE/FALSE; Is this a species complex? Default
#'  is false.
#' @param species Full common name for target species. Split
#' naming with a space and capitalize first letter(s). Example:
#' "Dover sole".
#' @param spp_latin Latin name for the target species. Example:
#' "Pomatomus saltatrix".
#' @param year Year the assessment is being conducted. Default
#' is the year in which the report is rendered.
#' @param file_dir Location of stock assessment files produced
#' by this function. Default is the working directory.
#' @param author Ordered list of authors included in the assessment.
#' @param add_author Author that is not currently in the database
#' and who should be temporarily added to the author list. Format
#' as "First MI Last".
#' Please leave a comment on the GitHub issues page to be added.
#' @param include_affiliation TRUE/FALSE; Does the analyst want to
#'  include the authors' affiliations in the document? Default is
#'  false.
#' @param simple_affiliation TRUE/FALSE; If including affiliations,
#'  should the office name function as the affiliation, rather
#'  than the full address? Default is true.
#' @param alt_title TRUE/FALSE; Use a title that is not the
#' default title (i.e., an alternative title)? Default is false.
#' @param title The alternative title. Example:
#' "Management Track Assessments Spring 2024".
#' @param parameters TRUE/FALSE; For
#' parameterization of the script. Default is true.
#' @param param_names List of parameter names that will be called
#'  in the document. Parameters automatically included:
#'  office, region, species (each of which are listed as
#'  individual parameters for this function, above).
#' @param param_values List of values associated with the order
#'  of parameter names. Parameters automatically included:
#'  office, region, species (each of which are listed as
#'  individual parameters for this function, above).
#' @param convert_output TRUE/FALSE; Convert the output file to
#' standard model format while creating report template? Default
#' is false.
#' @param fleet_names Deprecated: List of fleet names as described in BAM output
#'  file (abbreviations).
#' @param resdir Filepath of the directory storing the model
#'  results file(s). Examples where dover_sole_2024 is the project root
#'  for absolute and relative filepaths, respectively:
#'  "C:/Users/patrick.star/Documents/dover_sole_2024/models",
#'  "here::here("models")".
#' @param model_results The model results file. Before the stock
#' assessment output file has been converted to a standardized format
#'  with the function convert_output.R, the model results file may be
#'  a .sso or .rdata file. After conversion, this file will be a .csv file.
#' @param model Type of assessment model that was used to assess
#'  the stock (e.g., "BAM", "SS3", "AMAK", "ASAP", etc.).
#' @param new_section Names of section(s) (e.g., introduction, results) or
#' subsection(s) (e.g., a section within the introduction) that will be
#' added to the document. Please make a short list if >1 section/subsection
#' will be added. The template will be created as a quarto document, added
#' into the skeleton, and saved for reference.
#' @param section_location Where new section(s)/subsection(s) will be added to
#' the skeleton template. Please use the notation of 'placement-section'.
#' For example, 'in-introduction' signifies that the new content would
#' be created as a child document and added into the 02_introduction.qmd.
#' To add >1 (sub)section, make the location a list corresponding to the
#' order of (sub)section names listed in the 'new_section' parameter.
#' @param type Type of report to build. Default is SAR.
#' @param prev_year Year in which the previous assessment report
#'  was conducted. Used to pull previous assessment template.
#' @param custom TRUE/FALSE; Build custom sectioning for the
#' template, rather than the default for stock assessments in
#' your region? Default is false.
#' @param custom_sections List of existing sections to include in
#' the custom template. Note: this only includes sections within
#'  list.files(system.file("templates", "skeleton",
#'  package = "asar")). The name of the section, rather than the
#'  name of the file, can be used (e.g., 'abstract' rather than
#'  '00_abstract.qmd'). If adding a new section, also use
#'   parameters 'new_section' and 'section_location'.
#' @param include_figures TRUE/FALSE; Should figures be
#' included in the report? Default is true.
#' @param include_tables TRUE/FALSE; Should tables be included
#'  in the report? Default is true.
#' @param add_image TRUE/FALSE; Add image of species to the
#' template that is not already included in the project's
#' inst/resources/spp_img folder? Default is false.
#' @param spp_image File path to the species' image if not
#' using the image included in the project's repository.
#' @param bib_file File path to a .bib file used for citing references in
#' the report
#' @param rda_dir If the user has already created .rda files containing
#' figures, tables, alt text, and captions with `stockplotr`, rda_dir represents
#' the location of the folder containing these .rda files ("rda_files").
#' Otherwise, if the user has not used `stockplotr` to make those .rda files already,
#' those files will be generated automatically and placed within an "rda_files"
#' folder within rda_dir. The "rda_files" folder would have been
#' made with `stockplotr::exp_all_figs_tables()`, or by exporting files by running individual
#' `stockplotr` figure- and table-generating functions. If you have used `stockplotr` to
#' generate these .rda files, you can leave the arguments below blank. NOTE:
#' If an "rda_files" folder is detected within rda_dir, .rda files will not be
#' regenerated.
#' @param end_year The last year of assessment. The default is year - 1.
#' @param n_projected_years Number of years spawning biomass is projected for.
#' By default this number is set to 10
#' @param relative A logical value specifying if the resulting figures should be
#' relative spawning biomass. The default is 'FALSE'. 'ref_line' indicates which
#' reference point to use.
#' @param recruitment_scale_amount A number describing how much to scale down
#' the recruitment quantities shown on the y axis. For example,
#' recruitment_scale_amount = 100 would scale down a value from 500,000 -->
#' 5,000. This scale will be reflected in the y axis label.
#' @param recruitment_unit_label Units for recruitment
#' @param ref_line An argument inherited from `stockplotr::plot_spawning_biomass.R`.
#' A string specifying the type of reference you want to
#' compare spawning biomass to. The default is `"target"`, which looks for
#' `"spawning_biomass_target"` in the `"label"` column of `dat`. The actual
#' searching in `dat` is case agnostic and will work with either upper- or
#' lower-case letters but you must use one of the options specified in the
#' default list to ensure that the label on the figure looks correct
#' regardless of how it is specified in `dat`.
#' @param ref_point An argument inherited from `stockplotr::plot_biomass.R`. A known
#' value of the reference point along with the label for the reference point as
#' specified in the output file. Please use this option if the ref_line cannot
#' find your desired point. Indicate the reference point in the form
#' c("label" = value).
#' @param biomass_scale_amount A number describing how much to scale down the
#' biomass quantities shown on the y axis. See `recruitment_scale_amount`.
#' @param landings_unit_label Units for landings
#' @param spawning_biomass_label Units for spawning biomass
#' @param spawning_biomass_scale_amount  A number describing how much to scale down the
#' spawning biomass quantities shown on the y axis. See `recruitment_scale_amount`.
#' @param ref_line_sb A string specifying the type of
#' reference you want to compare spawning biomass to. The default is `"target"`,
#' which looks for `"spawning_biomass_target"` in the `"label"` column of `dat`.
#' The actual searching in `dat` is case agnostic and will work with either upper- or
#' lower-case letters but you must use one of the options specified in the
#' default list to ensure that the label on the figure looks correct
#' regardless of how it is specified in `dat`.
#' @param ref_point_sb Identical definition as `ref_point`, but this argument is
#' applied to plot_spawning_biomass.
#' @param indices_unit_label Units for index of abundance/CPUE
#' @param biomass_unit_label Abbreviated units for biomass
#' @param catch_unit_label Abbreviated units for catch
#' @param rerender_skeleton Re-create the "skeleton.qmd" in your outline when
#'        changes to the main skeleton need to be made. This reproduces the
#'        yaml, output (if changed), preamble quantities, and restructures your
#'        sectioning in the skeleton if indicated. All files in your folder
#'        will remain as is.
#' @return Create template and pull skeleton for a stock assessment report.
#'         Function builds a YAML specific to the region and utilizes current
#'         resources and workflows from different U.S. Fishery Science Centers.
#'         General sections are called as child documents in this skeleton and
#'         each of the child documents should be edited separately.
#' @export
#'
#' @examples
#' \dontrun{
#' create_template(
#'   new_section = "a_new_section",
#'   section_location = "before-introduction",
#'   rda_dir = here::here()
#' )
#'
#'
#' create_template(
#'   new_template = TRUE,
#'   format = "pdf",
#'   office = "NWFSC",
#'   species = "Dover sole",
#'   spp_latin = "Microstomus pacificus",
#'   year = 2010,
#'   author = c("John Snow", "Danny Phantom", "Patrick Star"),
#'   include_affiliation = TRUE,
#'   resdir = "C:/Users/Documents/Example_Files",
#'   model_results = "Report.sso",
#'   model = "SS3",
#'   new_section = "an_additional_section",
#'   section_location = "after-introduction",
#'   rda_dir = here::here()
#' )
#'
#' asar::create_template(
#'   new_template = TRUE,
#'   format = "pdf",
#'   office = "PIFSC",
#'   species = "Striped marlin",
#'   spp_latin = "Kajikia audax",
#'   year = 2018,
#'   author = "Alba Tross",
#'   model = "BAM",
#'   new_section = c("a_new_section", "another_new_section"),
#'   section_location = c("before-introduction", "after-introduction"),
#'   custom = TRUE,
#'   custom_sections = c("executive_summary", "introduction"),
#'   rda_dir = here::here()
#' )
#'
#' create_template(
#'   new_template = TRUE,
#'   format = "pdf",
#'   office = "NWFSC",
#'   region = "my_region",
#'   complex = FALSE,
#'   species = "Bluefish",
#'   spp_latin = "Pomatomus saltatrix",
#'   year = 2010,
#'   author = c("John Snow", "Danny Phantom", "Patrick Star"),
#'   add_author = "Sun E Day",
#'   include_affiliation = TRUE,
#'   simple_affiliation = TRUE,
#'   alt_title = FALSE,
#'   title = "Management Track Assessments Spring 2024",
#'   parameters = TRUE,
#'   param_names = c("region", "year"),
#'   param_values = c("my_region", "2024"),
#'   convert_output = FALSE,
#'   fleet_names = c("fleet1", "fleet2", "fleet3"),
#'   resdir = "C:/Users/Documents/Example_Files",
#'   model_results = "Report.sso",
#'   model = "SS3",
#'   new_section = "an_additional_section",
#'   section_location = "before-discussion",
#'   type = "SAR",
#'   prev_year = 2021,
#'   custom = TRUE,
#'   custom_sections = c("executive_summary", "introduction", "discussion"),
#'   include_figures = TRUE,
#'   include_tables = TRUE,
#'   add_image = TRUE,
#'   spp_image = "dir/containing/spp_image",
#'   rda_dir = "C:/Users/Documents",
#'   end_year = 2022,
#'   n_projected_years = 10,
#'   relative = FALSE,
#'   recruitment_scale_amount = 10,
#'   recruitment_unit_label = "metric tons",
#'   ref_line = "target",
#'   biomass_scale_amount = 100,
#'   landings_unit_label = "metric tons",
#'   spawning_biomass_label = "metric tons",
#'   spawning_biomass_scale_amount = 1000,
#'   recruitment_unit_label = "metric tons",
#'   ref_line_sb = "target",
#'   indices_unit_label = "CPUE",
#'   biomass_unit_label = "mt",
#'   catch_unit_label = "mt"
#' )
#' }
#'
create_template <- function(
    new_template = TRUE,
    format = c("pdf", "docx", "html", NULL),
    office = c("AFSC", "PIFSC", "NEFSC", "NWFSC", "SEFSC", "SWFSC"),
    region = NULL,
    complex = FALSE,
    species = NULL,
    spp_latin = NULL,
    year = NULL,
    file_dir = getwd(),
    author = "",
    add_author = NULL,
    include_affiliation = TRUE,
    simple_affiliation = FALSE,
    alt_title = FALSE,
    title = NULL,
    parameters = TRUE,
    param_names = NULL,
    param_values = NULL,
    convert_output = FALSE,
    fleet_names = NULL,
    resdir = NULL,
    model_results = NULL,
    model = NULL,
    new_section = NULL,
    section_location = NULL,
    type = "SAR",
    prev_year = NULL,
    custom = FALSE,
    custom_sections = NULL,
    include_figures = TRUE,
    include_tables = TRUE,
    add_image = FALSE,
    spp_image = NULL,
    bib_file = "asar_references.bib",
    rda_dir = getwd(),
    end_year = NULL,
    n_projected_years = 10,
    relative = FALSE,
    recruitment_scale_amount = 1,
    recruitment_unit_label = "metric tons",
    ref_line = c("target", "MSY", "msy", "unfished"),
    ref_point = NULL,
    biomass_scale_amount = 1,
    landings_unit_label = "metric tons",
    ref_point_sb = NULL,
    spawning_biomass_label = "metric tons",
    spawning_biomass_scale_amount = 1,
    ref_line_sb = c("target", "MSY", "msy", "unfished"),
    indices_unit_label = "",
    biomass_unit_label = "mt",
    catch_unit_label = "mt",
    rerender_skeleton = FALSE) {
  # If analyst forgets to add year, default will be the current year report is being produced
  if (is.null(year)) {
    year <- format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y")
  }

  # If analyst forgets to add end year, default will be year - 1
  if (is.null(end_year)) {
    end_year <- as.numeric(year) - 1
  }

  if (rerender_skeleton) {
    # TODO: set up situation where species, region can be changed
    report_name <- list.files(file_dir, pattern = "skeleton.qmd") # gsub(".qmd", "", list.files(file_dir, pattern = "skeleton.qmd"))
    if (length(report_name) == 0) stop("No skeleton quarto file found in the working directory.")
    if (length(report_name) > 1) stop("Multiple skeleton quarto files found in the working directory.")

    prev_report_name <- gsub("_skeleton.qmd", "", report_name)
    # Extract type
    type <- stringr::str_extract(prev_report_name, "^[A-Z]+")
    # Extract region
    region <- stringr::str_extract(prev_report_name, "(?<=_)[A-Z]+(?=_)")
    # report name without type and region
    report_name_1 <- gsub(glue::glue("{type}_"), "", prev_report_name)
    # Extract species
    species <- gsub("_",
                    " ",
                    gsub(glue::glue("{region}_"), "", report_name_1)
                    )


    new_report_name <- paste0(
      type, "_",
      ifelse(is.null(region), "", paste(gsub("(\\b[A-Z])[^A-Z]+", "\\1", region), "_", sep = "")),
      ifelse(is.null(species), "species", stringr::str_replace_all(species, " ", "_")), "_",
      "skeleton.qmd"
    )
  } else {
    # Name report
    if (!is.null(type)) {
      report_name <- paste0(
        type,
        "_"
      )
    } else {
      report_name <- paste0(
        "type_"
      )
    }
    # Add region to name
    if (!is.null(region)) {
      report_name <- paste0(
        report_name,
        gsub("(\\b[A-Z])[^A-Z]+", "\\1", region),
        "_"
      )
    } else {
      report_name <- report_name
    }
    # Add species to name
    if (!is.null(species)) {
      report_name <- paste0(
        report_name,
        gsub(" ", "_", species),
        "_skeleton.qmd"
      )
    } else {
      report_name <- paste0(
        report_name, "species_skeleton.qmd"
      )
    }
  } # close if rerender skeleton for naming

  # Select parameter from list
  if (length(format) > 1) {
    format <- "pdf"
  } else {
    format <- match.arg(format, several.ok = FALSE)
  }

  if (!is.null(office) & length(office) == 1) {
    office <- match.arg(office, several.ok = FALSE)
  } else if (length(office) > 1) {
    office <- ""
  }

  # Create subdirectory for files
  subdir <- ifelse(
    grepl("/report", file_dir),
    fs::path(file_dir),
    fs::path(file_dir, "report")
    )

  # Supporting files folder
  supdir <- file.path(subdir, "support_files")

  # if (!is.null(region)) {
  if (dir.exists(subdir) == FALSE) {
    dir.create(subdir, recursive = TRUE)
  }
  if (dir.exists(supdir) == FALSE) {
    dir.create(supdir, recursive = FALSE)
  }
  # }

  if (new_template) {
    if (is.null(type) | type == "SAR") {
      # Pull skeleton for sections
      current_folder <- system.file("templates", "skeleton", package = "asar")
      new_folder <- subdir

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
      if (add_image) {
        spp_image <- spp_image
      } else {
        spp_image <- system.file("resources", "spp_img", paste(gsub(" ", "_", species), ".png", sep = ""), package = "asar")
      }

      # Add bib file
      if (bib_file == "asar_references.bib") {
        bib_loc <- system.file("resources", "asar_references.bib", package = "asar")
        bib_name <- bib_file
      } else {
        # check if enter file exists
        # if (!file.exists(bib_file)) stop(".bib file not found.")
        warning(glue::glue("Bibliography file {bib_file} is not in the report directory. The file will not be read in on render if it is not in the same path as the skeleton file."))

        bib_loc <- bib_file # dirname(bib_file)
        bib_name <- stringr::str_extract(bib_file, "[^/]+$") # utils::tail(stringr::str_split(bib_file, "/")[[1]], n = 1)
      }

      # Check if this is a rerender of the skeleton file
      if (rerender_skeleton) {
        # read format in skeleton & check if format is identified in the rerender call
        if (!file.exists(file.path(file_dir, list.files(file_dir, pattern = "skeleton.qmd")))) stop("No skeleton quarto file found in the working directory.")
        prev_skeleton <- readLines(file.path(file_dir, list.files(file_dir, pattern = "skeleton.qmd")))
        # extract previous format
        prev_format <- stringr::str_extract(
          prev_skeleton[grep("format:", prev_skeleton) + 1],
          "[a-z]+"
          )
        year <- as.numeric(stringr::str_extract(
          prev_skeleton[grep("title:", prev_skeleton)],
          "[0-9]+"
          ))
        # if it is previously html and the rerender species html then need to copy over html formatting
        if (tolower(prev_format) != "html" & tolower(format) == "html") {
         if (!file.exists(file.path(file_dir, "support_files", "theme.scss"))) file.copy(system.file("resources", "formatting_files", "theme.scss", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
        }
        if (tolower(prev_format != "pdf" & tolower(format) == "pdf")) {
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
          # year - default to current year
          message("Undefined year:\nPlease identify year in your arguments or manually change it in the skeleton if value is incorrect.")
          # copy before-body tex
          if (!file.exists(file_dir, "support_files", "before-body.tex")) file.copy(before_body_file, supdir, overwrite = FALSE) |> suppressWarnings()
          # customize titlepage tex
          if (!file.exists(file_dir, "support_files", "_titlepage.tex") | !is.null(species)) create_titlepage_tex(office = office, subdir = supdir, species = species)
          # customize in-header tex
          if (!file.exists(file_dir, "support_files", "in-header.tex") | !is.null(species)) create_inheader_tex(species = species, year = year, subdir = supdir)
          # copy new spp image if updated
          if (!is.null(species)) file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
        }
      } else {
        # Check if there are already files in the folder
        if (length(list.files(subdir)) < 2) {
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
          # Copy bib file
          file.copy(bib_loc, subdir, overwrite = TRUE) |> suppressWarnings()
          # Copy us doc logo
          file.copy(system.file("resources", "us_doc_logo.png", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
          # Copy html format file if applicable
          if (tolower(format) == "html") file.copy(system.file("resources", "formatting_files", "theme.scss", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
        } else {
          warning("There are files in this location.")
          question1 <- readline("The function wants to overwrite the files currently in your directory. Would you like to proceed? (Y/N)")

          # answer question1 as y if session isn't interactive
          if (!interactive()){
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
            # Copy bib file
            file.copy(bib_loc, subdir, overwrite = TRUE) |> suppressWarnings()
            # Copy us doc logo
            file.copy(system.file("resources", "us_doc_logo.png", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
            # Copy html format file if applicable
            if (tolower(format) == "html") file.copy(system.file("resources", "formatting_files", "theme.scss", package = "asar"), supdir, overwrite = FALSE) |> suppressWarnings()
          } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
            warning("Report template files were not copied into your directory. If you wish to update the template with new parameters or output files, please edit the ", report_name, " in your local folder.")
          }
        } # close check for previous files & respective copying
        prev_skeleton <- NULL
      } # close if rerender

      # Convert output file if TRUE
      # Make sure not asking to rerender
      # if (!rerender_skeleton) {
        # Check if converted output already exists
        if (convert_output) {
          if (!file.exists(file.path(subdir, paste(stringr::str_replace_all(species, " ", "_"), "_std_res_", year, ".csv", sep = "")))) {
            print("__________Converting output file__________")
            if (tolower(model) == "bam" & is.null(fleet_names)) {
              # warning("Fleet names not defined.")
              convert_output(
                output_file = model_results,
                outdir = resdir,
                file_save = TRUE,
                model = model,
                savedir = subdir,
                save_name = paste(stringr::str_replace_all(species, " ", "_"), "_std_res_", year, sep = "")
              )
              # } else if (tolower(model) == "bam") {
              #   convert_output(
              #     output_file = model_results,
              #     outdir = resdir,
              #     file_save = TRUE,
              #     model = model,
              #     fleet_names = fleet_names,
              #     savedir = subdir,
              #     save_name = paste(sub(" ", "_", species), "_std_res_", year, sep = "")
              #   )
            } else {
              convert_output(
                output_file = model_results,
                outdir = resdir,
                file_save = TRUE,
                model = model,
                savedir = subdir,
                save_name = paste(stringr::str_replace_all(species, " ", "_"), "_std_res_", year, sep = "")
              )
            }
            # Rename model results file and results file directory if the results are converted in this fxn
            model_results <- paste0(stringr::str_replace_all(species, " ", "_"), "_std_res_", year, ".csv")
            resdir <- subdir
          } else {
            message("Output not converted: standard output already in path.")
            model_results <- paste0(stringr::str_replace_all(species, " ", "_"), "_std_res_", year, ".csv")
            resdir <- subdir
          }
        } # close check for converted output already
      # } # close rerender if

      # print("_______Standardized output data________")

      # run stockplotr::exp_all_figs_tables() if rda files not premade
      # output folder: rda_dir
      # Don't run on rerender
      if (!rerender_skeleton) {
        if (!dir.exists(fs::path(rda_dir, "rda_files"))) {
          if (!is.null(resdir) | !is.null(model_results)) {
            # load converted output
            if (convert_output) {
              output <- utils::read.csv(paste0(subdir, "/", paste(stringr::str_replace_all(species, " ", "_"), "_std_res_", year, ".csv", sep = "")))
            } else {
              output <- utils::read.csv(paste0(resdir, "/", model_results))
            }
            # run stockplotr::exp_all_figs_tables() to make rda files

            # test_exp_all <-
            tryCatch(
              {
                stockplotr::exp_all_figs_tables(
                  dat = output,
                  recruitment_scale_amount = recruitment_scale_amount,
                  end_year = end_year,
                  n_projected_years = n_projected_years,
                  relative = relative,
                  # make_rda = TRUE,
                  rda_dir = rda_dir,
                  ref_line = ref_line,
                  ref_point = ref_point,
                  biomass_scale_amount = biomass_scale_amount,
                  landings_unit_label = landings_unit_label,
                  spawning_biomass_scale_amount = spawning_biomass_scale_amount,
                  spawning_biomass_label = spawning_biomass_label,
                  recruitment_unit_label = recruitment_unit_label,
                  ref_line_sb = ref_line_sb,
                  ref_point_sb = ref_point_sb,
                  indices_unit_label = indices_unit_label,
                  biomass_unit_label = biomass_unit_label,
                  catch_unit_label = catch_unit_label
                )
                # TRUE
              },
              error = function(e) {
                warning("Failed to create all rda files from stockplotr package.")
                # FALSE
              }
            )
          } # else {
          # test_exp_all <- FALSE
          # }
        }
      }

      # Create tables qmd
      if ((include_tables & !rerender_skeleton) | (rerender_skeleton & !is.null(model_results))) {
        # if (!test_exp_all) {
        #   tables_doc <- paste0(
        #     "### Tables \n \n",
        #     "Please refer to the `stockplotr` package downloaded from remotes::install_github('nmfs-ost/stockplotr') to add premade tables."
        #   )
        #   utils::capture.output(cat(tables_doc), file = fs::path(subdir, "08_tables.qmd"), append = FALSE)
        #   warning("Results file or model name not defined.")
        # } else
        if (!is.null(resdir) | !is.null(model_results) | !is.null(model)) {
          # if there is an existing folder with "rda_files" in the rda_dir:
          if (dir.exists(fs::path(rda_dir, "rda_files"))) {
            create_tables_doc(
              subdir = subdir,
              rda_dir = rda_dir
            )
          }
        } else {
          tables_doc <- paste0(
            "## Tables \n \n",
            "Please refer to the `stockplotr` package downloaded from remotes::install_github('nmfs-ost/stockplotr') to add premade tables."
          )
          utils::capture.output(cat(tables_doc), file = fs::path(subdir, "08_tables.qmd"), append = FALSE)
          warning("Results file or model name not defined.")
        }
      }

      # Rename model results for figures and tables files
      # TODO: check if this is needed once the tables and figures docs are reformatted
      # if (convert_output) {
      #   model_results <- paste(stringr::str_replace_all(species, " ", "_"), "_std_res_", year, sep = "")
      # }

      # Create figures qmd
      if ((include_figures & !rerender_skeleton) | (rerender_skeleton & !is.null(model_results))) {
        # if (!test_exp_all) {
        #   figures_doc <- paste0(
        #     "### Figures \n \n",
        #     "Please refer to the `stockplotr` package downloaded from remotes::install_github('nmfs-ost/stockplotr') to add premade figures."
        #   )
        #   utils::capture.output(cat(figures_doc), file = fs::path(subdir, "09_figures.qmd"), append = FALSE)
        #   warning("Results file or model name not defined.")
        # } else
        if (!is.null(resdir) | !is.null(model_results) | !is.null(model)) {
          # if there is an existing folder with "rda_files" in the rda_dir:
          if (dir.exists(fs::path(rda_dir, "rda_files"))) {
            create_figures_doc(
              subdir = subdir,
              rda_dir = rda_dir
            )
          }
        } else {
          figures_doc <- paste0(
            "## Figures \n \n",
            "Please refer to the `stockplotr` package downloaded from remotes::install_github('nmfs-ost/stockplotr') to add premade figures."
          )
          utils::capture.output(cat(figures_doc), file = fs::path(subdir, "09_figures.qmd"), append = FALSE)
          warning("Results file or model name not defined.")
        }
      }

      # Part I
      # Create a report template file to render for the region and species
      # Create YAML header for document
      # Write title based on report type and region
      if (alt_title) {
        if (is.null(title)) {
          stop("Alternate title not defined. Please define an alternative title in the parameter 'title'.")
        }
        title <- title
      } else {
        title <- create_title(office = office, species = species, spp_latin = spp_latin, region = region, type = type, year = year)
      }

      # Pull authors and affiliations from national db
      # Check if rerender and if author is already added
      # TODO: add feature to allow removal of authors if there are ones that
        # are repeated from the previous skeleton and those named (not just
        # additions of new names)
      if (rerender_skeleton) {
        # Pull all author names from prev_skeleton
        author_prev <- grep(
          "\\- name:\\s*'",
          prev_skeleton,
          value = TRUE
        )
        # Remove every second occurance of "-name"
        author_prev <- author_prev[seq(1, length(author_prev), 2)]
        # Remove everything but the name
        author_prev <- sub(
          ".*\\- name:\\s*'([^']+)'.*",
          "\\1",
          author_prev
        )
        setdiff(author, author_prev) -> author
      }

      # Parameters to add authorship to YAML
      # Read authorship file
      authors <- utils::read.csv(system.file("resources", "authorship.csv", package = "asar", mustWork = TRUE)) |>
        dplyr::mutate(
          mi = dplyr::case_when(
            mi == "" ~ NA,
            TRUE ~ mi
          ),
          name = dplyr::case_when(
            is.na(mi) ~ paste0(first, " ", last),
            TRUE ~ paste(first, mi, last, sep = " ")
          )
        ) |>
        dplyr::select(name, office) |>
        dplyr::filter(name %in% author)

      if (length(author) != dim(authors)[1]){
        message("Some authors were not found in the author database. Please comment on this issue (https://github.com/nmfs-ost/asar/issues/19) to request name and affiliation additions to the archive of U.S. stock assessment authors.")
      }

      authors <- authors[match(author, authors$name), ]

      if (include_affiliation) {
        affil <- utils::read.csv(system.file("resources", "affiliation_info.csv", package = "asar", mustWork = TRUE))
      }
      if (!is.null(add_author)) {
        authors <- rbind(authors, data.frame(name = add_author, office = rep(NA, length(add_author))))
      }

      author_list <- list()
      if (include_affiliation & !simple_affiliation) {
        if (nrow(authors) > 0) {
          if (rerender_skeleton) {
            author_lines <- grep(
              "\\- name:\\s*'",
              prev_skeleton,
              value = TRUE
            )
            authors_prev <- sub(
              ".*\\- name:\\s*'([^']+)'.*",
              "\\1",
              author_lines
            )
            # remove authors previously in skeleton and keep new additions either from author or add_author
            author_to_add <- setdiff(authors$name, authors_prev)
            authors <- authors |>
              dplyr::filter(name %in% author_to_add)
          }
          for (i in 1:nrow(authors)) {
            auth <- authors[i, ]
            aff <- affil |>
              dplyr::filter(affiliation == auth$office)
            if (is.na(auth$office)) {
              paste(
                "  ", "- name: ", "'", auth$name, "'", "\n",
                "  ", "  ", "affiliations:", "\n",
                "  ", "  ", "  ", "- name: '[organization]'", "\n", # "NOAA Fisheries ",
                "  ", "  ", "  ", "  ", "address: '[address]'", "\n",
                "  ", "  ", "  ", "  ", "city: '[city]'", "\n",
                "  ", "  ", "  ", "  ", "state: '[state]'", "\n",
                "  ", "  ", "  ", "  ", "postal-code: '[postal code]'", "\n",
                sep = ""
              ) -> author_list[[i]]
            } else {
              paste(
                "  ", "- name: ", "'", auth$name, "'", "\n",
                "  ", "  ", "affiliations:", "\n",
                "  ", "  ", "  ", "- name: ", "'", aff$name, "'", "\n", # "NOAA Fisheries ",
                "  ", "  ", "  ", "  ", "address: ", "'", aff$address, "'", "\n",
                "  ", "  ", "  ", "  ", "city: ", "'", aff$city, "'", "\n",
                "  ", "  ", "  ", "  ", "state: ", "'", aff$state, "'", "\n",
                "  ", "  ", "  ", "  ", "postal-code: ", "'", aff$postal.code, "'", "\n",
                sep = ""
              ) -> author_list[[i]]
            }
          }
        } else {
          paste0(
            "  ", "- name: ", "'FIRST LAST'", "\n",
            "  ", "  ", "affiliations: \n",
            "  ", "  ", "  ", "- name: 'NOAA Fisheries' \n",
            "  ", "  ", "  ", "  ", "address: 'ADDRESS' \n",
            "  ", "  ", "  ", "  ", "city: 'CITY' \n",
            "  ", "  ", "  ", "  ", "state: 'STATE' \n",
            "  ", "  ", "  ", "  ", "postal-code: 'POSTAL CODE' \n"
            # sep = " "
          ) -> author_list[[1]]
        }
      } else if (include_affiliation & simple_affiliation) {
        if (nrow(authors) > 0) {
          for (i in 1:nrow(authors)) {
            auth <- authors[i, ]
            aff <- affil |>
              dplyr::filter(affiliation == auth$office)
            if (!is.na(auth$office)) {
              paste0(
                "  ", "- name: ", "'", auth$name, "'", "\n",
                "  ", "  ", "affiliations: ", "'", aff$name, "'", "\n"
              ) -> author_list[[i]]
            } else {
              paste0(
                "  ", "- name: ", "'", auth$name, "'", "\n",
                "  ", "  ", "affiliations: ", "'NO AFFILIATION'", "\n"
              ) -> author_list[[i]]
            }
          }
        } else {
          paste0(
            "  ", "- name: ", "'FIRST LAST'", "\n",
            "  ", "  ", "affiliations: ", "NO AFFILIATION", "\n"
          ) -> author_list[[1]]
        }
      } else {
        if (nrow(authors) > 0) {
          for (i in 1:nrow(authors)) {
            auth <- authors[i, ]
            paste0("  ", "- ", "'", auth$name, "'", "\n") -> author_list[[i]]
          }
        } else {
          paste0("  ", "- 'FIRST LAST' \n") -> author_list[[1]]
        }
      }

      # Create yaml
      yaml <- create_yaml(
        rerender_skeleton = rerender_skeleton,
        prev_skeleton = prev_skeleton,
        prev_format = prev_format,
        title = title,
        # alt_title = alt_title,
        author_list = author_list,
        author = author,
        office = office,
        add_author = add_author,
        add_image = add_image,
        spp_image = spp_image,
        species = species,
        spp_latin = spp_latin,
        region = region,
        format = format,
        parameters = parameters,
        param_names = param_names,
        param_values = param_values,
        bib_file = bib_file,
        bib_name = bib_name,
        year = year
      )

      if (!rerender_skeleton) print("__________Built YAML Header______________")

      # yaml_save <- capture.output(cat(yaml))
      # cat(yaml, file = here('template','yaml_header.qmd'))

      # add in html for draft watermark if in that format - otherwise pdf draft is in format_quarto fxn
      if (format == "html") {
        html_draft <- paste(
          "\n ```{=html} \n",
          "<div style='position: fixed; margin-top: 10%; margin-left:5%; font-size: xx-large; font-weight: 900; color: #CCCCCC; rotate: -45deg; z-index:-999;'>DRAFT</div>", "\n",
          "``` \n",
          sep = ""
        )
      }

      # Add preamble
      # add in quantities and output data R chunk
      # Indicate model output path

      # in case where rerndering skeleton and they want to update the model results
      if (rerender_skeleton) {
        if (!is.null(model_results) & !is.null(resdir)) {
          prev_conout <- convert_output
          convert_output <- FALSE
        }
      }

      # standard preamble
      preamble <- add_chunk(
        paste0(
          "# load converted output from asar::convert_output() \n",
          "output <- utils::read.csv('",
          ifelse(convert_output,
            paste0(subdir, "/", stringr::str_replace_all(species, " ", "_"), "_std_res_", year, ".csv"),
            paste0(resdir, "/", model_results)
          ), "') \n",
          "# Call reference points and quantities below \n",
          "output <- output |> \n",
          "  ", "dplyr::mutate(estimate = as.numeric(estimate), \n",
          "  ", "  ", "uncertainty = as.numeric(uncertainty)) \n",
          "start_year <- as.numeric(min(output$year, na.rm = TRUE)) \n",
          # change end year in the fxn to ifelse where is.null(year)
          "end_year <- (output |> \n",
          "  ", "dplyr::filter(!(year %in% c('Virg', 'Init', 'S/Rcurve', 'INIT')), \n",
          "  ", "  ", "!is.na(year)) |> \n",
          "  ", "dplyr::mutate(year = as.numeric(year)) |> \n",
          "  ", "dplyr::summarize(max_val = max(year)) |> \n",
          "  ", "dplyr::pull(max_val))-10", "\n",
          # for quantities - don't want any values that are split by factor
          "# subset output to remove quantities that are split by factor \n",
          "output2 <- output |> \n",
          "  ", "dplyr::filter(is.na(season), \n",
          "  ", "  ", "is.na(fleet), \n",
          "  ", "  ", "is.na(sex), \n",
          "  ", "  ", "is.na(area), \n",
          "  ", "  ", "is.na(growth_pattern), \n",
          "  ", "  ", "is.na(subseason), \n",
          "  ", "  ", "is.na(age))", "\n",
          "# terminal fishing mortality \n",
          "Fend <- output2 |> ", "\n",
          "  ", "dplyr::filter(c(label == 'fishing_mortality' & year == end_year) | c(label == 'terminal_fishing_mortality' & is.na(year))) |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# fishing mortality at msy \n",
          "# please change target if desired \n",
          "Ftarg <- output2 |>", "\n",
          "  ", "dplyr::filter(grepl('f_target', label) | grepl('f_msy', label) | c(grepl('fishing_mortality_msy', label) & is.na(year))) |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# Terminal year F respective to F target \n",
          "F_Ftarg <- Fend / Ftarg", "\n",
          "# terminal year biomass \n",
          "Bend <- output2 |>", "\n",
          "  ", "dplyr::filter(grepl('mature_biomass', label) | grepl('^biomass$', label),", "\n",
          "  ", "  ", "year == end_year) |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# target biomass (msy) \n",
          "# please change target if desired \n",
          "Btarg <- output2 |>", "\n",
          "  ", "dplyr::filter(c(grepl('biomass', label) & grepl('target', label) & estimate >1) | label == 'biomass_msy') |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# total catch in the last year \n",
          "total_catch <- output |>", "\n",
          "  ", "dplyr::filter(grepl('^catch$', label), \n",
          "  ", "year == end_year,", "\n",
          "  ", "  ", "is.na(fleet),", "\n",
          "  ", "  ", "is.na(age),", "\n",
          "  ", "  ", "is.na(area),", "\n",
          "  ", "  ", "is.na(growth_pattern)) |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          # chk_c <- dplyr::filter(output2, grepl("^catch$", label), year == end_year) |>
          #   dplyr::group_by(year) |>
          #   dplyr::summarise(total_catch = sum(estimate))
          "# total landings in the last year \n",
          "total_landings <- output |>", "\n",
          "  ", "dplyr::filter(grepl('landings_weight', label), year == end_year,", "\n",
          "  ", "  ", "is.na(fleet),", "\n",
          "  ", "  ", "is.na(age)) |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# spawning biomass in the last year\n",
          "SBend <- output2 |>", "\n",
          "  ", "dplyr::filter(grepl('spawning_biomass', label), year == end_year) |>", "\n",
          "  ", "dplyr::pull(estimate) |>", "\n",
          "  ", "  ", "unique()", "\n",
          "# overall natural mortality or at age \n",
          "M <- output |>", "\n",
          "  ", "dplyr::filter(grepl('natural_mortality', label)) |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# Biomass at msy \n",
          "# to change to another reference point, replace msy in the following lines with other label \n",
          "Bmsy <- output2 |>", "\n",
          "  ", "dplyr::filter(c(grepl('biomass', label) & grepl('msy', label) & estimate >1) | label == 'biomass_msy') |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# target spawning biomass(msy) \n",
          "# please change target if desired \n",
          "SBtarg <- output2 |>", "\n",
          "  ", "dplyr::filter(c(grepl('spawning_biomass', label) & grepl('msy$', label) & estimate >1) | label == 'spawning_biomass_msy$') |>", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# steepness \n",
          "h <- output |> ", "\n",
          "  ", "dplyr::filter(grepl('steep', label)) |> ", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# recruitment \n",
          "R0 <- output |> ", "\n",
          "  ", "dplyr::filter(grepl('R0', label) | grepl('recruitment_virgin', label)) |> ", "\n",
          "  ", "dplyr::pull(estimate)", "\n",
          "# female SB (placeholder)", "\n"
        ),
        label = "output_and_quantities",
        eval = ifelse(is.null(model_results), "false", "true")
      )
      # bring back the initial call of convert_output
      if (rerender_skeleton) {
        if (!is.null(model_results)) {
          convert_output <- prev_conout
        }
      }
      # extract old preamble if don't want to change
      if (rerender_skeleton) {
        question1 <- readline("Do you want to keep the current preamble? (Y/N)")

        # answer question1 as y if session isn't interactive
        if (!interactive()){
          question1 <- "y"
        }
        if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
          start_line <- grep("output_and_quantities", prev_skeleton) - 1
          # find next trailing "```"` in case it was edited at the end
          end_line <- grep("```", prev_skeleton)[grep("```", prev_skeleton) > start_line][1]
          preamble <- paste(prev_skeleton[start_line:end_line], collapse = "\n")

          if (!is.null(model_results) & !is.null(resdir)) {
            prev_results_line <- grep("output <- utils::read.csv", preamble)
            prev_results <- stringr::str_replace(
              preamble[prev_results_line],
              "(?<=read\\.csv\\().*?(?=\\))",
              glue::glue("'{resdir}/{model_results}'")
              )
            preamble <- append(
              preamble,
              prev_results,
              after = prev_results_line)[-prev_results_line]
            if (!grepl(".csv", model_results)) warning("Model results are not in csv format - Will not work on render")
          } else {
            message("Preamble maintained - model results not updated.")
          }
        } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
          preamble <- preamble
        } # close if updating preamble
      } # close if rerendering skeleton

      # Add page for citation of assessment report
      if (rerender_skeleton) {
        if (!is.null(title) | !is.null(species) | !is.null(year) | !is.null(author)) {
          citation_line <- grep("Please cite this publication as:", prev_skeleton) + 2
          citation <- glue::glue("{{{{< pagebreak >}}}} \n\n Please cite this publication as: \n\n {prev_skeleton[citation_line]} \n\n")
        } else {
          author <- grep("  - name: ", prev_skeleton)
          citation <- create_citation(
            author = author,
            title = title,
            year = year,
            office = office
          )
        }
      } else {
        citation <- create_citation(
          author = author,
          title = title,
          year = year,
          office = office
        )
        print("_______Add Report Citation________")
      }

      # Create report template
      # Include tables and figures if desired into template
      # at this point, files_to_copy is the most updated outline
      # if (include_tables) files_to_copy <- c(files_to_copy, "08_tables.qmd")
      # if (include_figures) files_to_copy <- c(files_to_copy, "09_figures.qmd")

      if (rerender_skeleton & custom == FALSE) {
        # identify all previous sections
        sections <- stringr::str_extract_all(
          prev_skeleton,
          "(?<=['`])[^']+\\.qmd(?=['`])"
        ) |>
        unlist() |>
        purrr::discard(~ .x == "")
        # add sections as list
        sections <- add_child(
          sections,
          label = gsub(".qmd", "", unlist(sections))
        )
      } else if (custom == FALSE) {
        sections <- add_child(
          c(
            "01_executive_summary.qmd",
            "02_introduction.qmd",
            "03_data.qmd",
            "04a_assessment-configuration.qmd",
            "04b_assessment-results.qmd",
            "04c_assessment-sensitivity.qmd",
            "04d_assessment-benchmarks.qmd",
            "04e_assessment-projections.qmd",
            "05_discussion.qmd",
            "06_acknowledgments.qmd",
            "07_references.qmd",
            "08_tables.qmd",
            "09_figures.qmd",
            "10_notes.qmd",
            "11_appendix.qmd"
          ),
          label = c(
            "executive_summary",
            "introduction",
            "data",
            "assessment-configuration",
            "assessment-results",
            "assessment-sensitivity",
            "assessment-benchmarks",
            "assessment-projections",
            "discussion",
            "acknowledgments",
            "references",
            "tables",
            "figures",
            "notes",
            "appendix"
          )
        )
      } else {
        # Option for building custom template
        # Create custom template from existing skeleton sections
        if (is.null(new_section)) {
          section_list <- add_base_section(files_to_copy)
          if (include_tables) {
            section_list <- c(section_list, "08_tables.qmd")
            custom_sections <- c(custom_sections, "tables")
          }
          if (include_figures) {
            section_list <- c(section_list, "09_figures.qmd")
            custom_sections <- c(custom_sections, "figures")
          }
          # Create sections object to add into template
          sections <- add_child(
            section_list,
            label = custom_sections
          )
        } else { # custom = TRUE
          # Create custom template using existing sections and new sections from analyst
          # Add sections from package options

          if (is.null(custom_sections)) {
            sec_list1 <- list(
              "01_executive_summary.qmd",
              "02_introduction.qmd",
              "03_data.qmd",
              "04a_assessment-configuration.qmd",
              "04b_assessment-results.qmd",
              "04c_assessment-sensitivity.qmd",
              "04d_assessment-benchmarks.qmd",
              "04e_assessment-projections.qmd",
              "05_discussion.qmd",
              "06_acknowledgments.qmd",
              "07_references.qmd",
              "08_tables.qmd",
              "09_figures.qmd",
              "10_notes.qmd",
              "11_appendix.qmd"
            )
            sec_list2 <- add_section(
              new_section = new_section,
              section_location = section_location,
              custom_sections = sec_list1,
              subdir = subdir
            )
            # Create sections object to add into template
            custom_sections <- gsub(".qmd", "", unlist(sec_list2))
            custom_sections <- sub("^[0-9]+_","", custom_sections)
            custom_sections <- sub("^[0-9]+[a-z]_", "", custom_sections)
            sections <- add_child(
              sec_list2,
              label = custom_sections
            )
          } else { # custom_sections explicit

            # Add selected sections from base
            sec_list1 <- add_base_section(files_to_copy)
            # Create new sections as .qmd in folder
            # check if sections are in custom_sections list
            if (any(stringr::str_replace(section_location, "^[a-z]+-", "") %notin% custom_sections)) {
              stop("Defined customizations do not match one or all of the relative placement of a new section. Please review inputs.")
            }
            if (include_tables) {
              sec_list1 <- c(sec_list1, "08_tables.qmd")
            }
            if (include_figures) {
              sec_list1 <- c(sec_list1, "09_figures.qmd")
            }
            # reorder sec_list1 alphabetically so that 11_appendix goes to end of list
            sec_list1 <- sec_list1[order(names(setNames(sec_list1, sec_list1)))]

            sec_list2 <- add_section(
              new_section = new_section,
              section_location = section_location,
              custom_sections = sec_list1,
              subdir = subdir
            )
            # Create sections object to add into template
            # name of chunks
            custom_sections <- gsub(".qmd", "", unlist(sec_list2))
            custom_sections <- sub("^[0-9]+_","", custom_sections)
            custom_sections <- sub("^[0-9]+[a-z]_", "", custom_sections)
            sections <- add_child(
              sec_list2,
              label = custom_sections
            )
          } # close if statement for very specific sectioning
        } # close if statement for extra custom
      } # close if statement for custom

      # Combine template sections
      report_template <- paste(
        yaml,
        if (format == "html") html_draft,
        preamble, "\n",
        citation,
        sections,
        sep = "\n"
      )

      print("___Created report template______")

      ######## |###############################################################
      ##### NEFSC MT Template####
      ######## |###############################################################
    } else if (type == "NEMT") {
      stop("Template not available.")
    }

    # Save template as .qmd to render
    utils::capture.output(cat(report_template), file = file.path(subdir, ifelse(rerender_skeleton, new_report_name, report_name)), append = FALSE)
    # Delete old skeleton
    if (length(grep("skeleton.qmd", list.files(file_dir, pattern = "skeleton.qmd"))) > 1) {
      question1 <- readline("Deleting previous skeleton file...Do you want to proceed? (Y/N)")

      # answer question1 as y if session isn't interactive
      if (!interactive()){
        question1 <- "y"
      }

      if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
        file.remove(file.path(file_dir, report_name))
      } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
        message("Skeleton file retained.")
      }
    }

    # Print message
    if (rerender_skeleton) {
      message(
        "Updated report skeleton in directory: ", subdir, "."
      )
    } else {
      message(
        "Saved report template in directory: ", subdir, "\n",
        "To proceeed, please edit sections within the report template in order to produce a completed stock assessment report."
      )
    }
    # Open file for analyst
    # file.show(file.path(subdir, report_name)) # this opens the new file, but also restarts the session
    # Open the file so path to other docs is clear
    # utils::browseURL(subdir)
  } else {
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

    svDialogs::dlg_message("Reminder: there are changes to be made when calling an old report. Please change the year in the citation and the location and name of the results file in the first chunk of the report.",
      type = "ok"
    )
  }
}
