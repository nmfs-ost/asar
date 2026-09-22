#' Rerender skeleton quarto document
#'
#' @inheritParams create_template
#' @param file_dir Required. Directory where the skeleton file is located. Can include or leave out the report folder in the path.
#'
#' @returns Update the "skeleton" file produce after running `create_template`. 
#' Prevents the loss of data in child documents and make easy updates without 
#' prior knowledge of quarto.
#' @export
#'
#' @examples
#' \dontrun{
#' rerender_skeleton(
#' file_dir = getwd(),
#' species = "Red Snapper",
#' office = "SEFSC",
#' region = "Gulf of America",
#' year = 2027,
#' authors = c("Jane Doe" = "SEFSC")
#' )
#' }
rerender_skeleton <- function(
    file_dir,
    species = "species",
    spp_latin = NULL,
    office = NULL,
    region = NULL,
    year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y"),
    custom_sections = NULL,
    new_section = NULL,
    section_location = NULL,
    custom_params = NULL,
    title = "[TITLE]",
    model_results = NULL,
    bib_file = NULL,
    type = "sar",
    spp_image = NULL,
    format = "pdf",
    authors = NULL
) {
  # Add in report to file_dir
  if (!grepl("report", file_dir)) file_dir <- file.path(file_dir, "report")
  # ID other directories
  supdir <- file.path(file_dir, "support_files")
  bibdir <- file.path(file_dir, "bibliography_files")
  
  #### Read in previous skeleton ----
  # TODO: set up situation where species, region can be changed
  report_name <- list.files(file_dir, pattern = "skeleton.qmd") # gsub(".qmd", "", list.files(file_dir, pattern = "skeleton.qmd"))
  if (length(report_name) == 0) cli::cli_abort("No skeleton quarto file found in the `file_dir` ({file_dir}).")
  if (length(report_name) > 1) cli::cli_abort("Multiple skeleton quarto files found in the `file_dir` ({file_dir}).")
  
  prev_report_name <- gsub("_skeleton.qmd", "", report_name)
  # Extract type
  type <- stringr::str_extract(prev_report_name, "^[a-z]+")
  # Extract region unless region is changed or updated
  # identify region from the skeleton
  prev_skeleton <- readLines(file.path(file_dir, list.files(file_dir, pattern = "skeleton.qmd")))
  if (is.null(region)) {
    region <- stringr::str_extract(
      prev_skeleton[grep("region: ", prev_skeleton)],
      "(?<=')[^']+(?=')"
    )
  }
  region_name <- ifelse(
    region != "NA", # !is.null(region) | !is.na(region)
    toupper(stringr::str_c(stringr::str_extract_all(region, "\\b[A-Za-z]")[[1]], collapse = "")),
    stringr::str_extract(prev_report_name, "(?<=_)[A-Z]+(?=_)")
  )
  # report name without type
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
  # Set new report name
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
  # make sure type is changed to skeleton
  if (type == "sar") type <- "skeleton"
  
  # extract previous format
  prev_format <- stringr::str_extract(
    prev_skeleton[grep("format:", prev_skeleton) + 1],
    "[a-z]+"
  )
  prev_year <- as.numeric(stringr::str_extract(
    prev_skeleton[grep("title:", prev_skeleton)],
    "[0-9]+"
  ))
  
  # Add in species image if updated in rerender
  if (!is.null(spp_image)) {
    file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
    # Change path to spp image since finished copying for yaml
    if (file.exists(spp_image)) {
      spp_image <- file.path("support_files", stringr::str_extract(spp_image, "(?<=/)[^/]+$"))
    }
  } else if (is.null(spp_image) && species != "species") {
    spp_image <- system.file("resources", "spp_img", paste(gsub(" ", "_", species), ".png", sep = ""), package = "asar") 
    # file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
    # spp image name for yaml
    spp_image <- glue::glue("support_files/{basename(spp_image)}")
  }
  # if it is previously html and the rerender species html then need to copy over html formatting
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
    # year - default to current year
    cli::cli_alert_warning("Undefined year.")
    cli::cli_alert_info("Please identify year in your arguments or manually change it in the skeleton if value is incorrect.",
                        wrap = TRUE
    )
    
    # copy before-body tex
    if (!file.exists(file_dir, "support_files", "before-body.tex")) file.copy(before_body_file, supdir, overwrite = FALSE) |> suppressWarnings()
    # customize titlepage tex
    if (!file.exists(file_dir, "support_files", "_titlepage.tex") | !is.null(species)) create_titlepage_tex(office = office, subdir = supdir, species = species)
    # customize in-header tex
    if (!file.exists(file_dir, "support_files", "in-header.tex") | !is.null(species)) create_inheader_tex(species = species, year = year, subdir = supdir)
    # copy new spp image if updated
    if (!is.null(species)) file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
  }
  
  #### Figs and tabs docs ----
  # extract name for tables.qmd from report folder
  fig_info <- migrate_legacy_docs(file_dir, doc_type = "figures", rerender_skeleton = FALSE)
  tbl_info <- migrate_legacy_docs(file_dir, doc_type = "tables", rerender_skeleton = FALSE)
  
  tables_doc_name <- if (can_rename_legacy_doc(tbl_info)) {
    tbl_info$current_name
  } else {
    list.files(file_dir, pattern = "tables.qmd")
  }
  # extract name for figures.qmd from report folder
  figures_doc_name <- if (can_rename_legacy_doc(fig_info)) {
    fig_info$current_name
  } else {
    list.files(file_dir, pattern = "figures.qmd")
  }
  
  #### Adjust the title ---- 
  if (title == "[TITLE]") {
    title <- sub("title: ", "", prev_skeleton[grep("title:", prev_skeleton)])
    if (title == "'Stock Assessment Report Template'" & (!is.null(office) | !is.null(species) | !is.null(region))) {
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

  #### Initialize bib name ----
  # Don't need to extract previous bib names bc create_yaml with rerender modifies the lines rather than build the whole thing
  # lines_after_bib <- prev_skeleton[(grep("bibliography:", prev_skeleton)[1] + 1):(grep("csl:", prev_skeleton)[1] - 1)]
  # bib_name <- basename(stringr::str_replace_all(lines_after_bib, "  - ", ""))
  # Add bib file if bib_file is not NULL
  # Note: this is copied from create_template
  if (!is.null(bib_file)) {
    file.copy(bib_file, bibdir, overwrite = TRUE) |> suppressWarnings()
    bib_name <- basename(bib_file)
  } else {
    bib_name <- NULL
  }
  
  #### Authors ----
  author_list <- add_authors(
    prev_skeleton = prev_skeleton,
    authors = authors, # need to put this in case there is a rerender otherwise it would not use the correct argument
    rerender_skeleton = TRUE
  )
  
  #### Parameters for yaml ----
  # Unpack
  parameters <- TRUE
  param_names <- custom_params |> names()
  param_values <- custom_params |> unname()

  #### yaml ----
  yaml <- create_yaml(
    prev_format = prev_format,
    format = format,
    prev_skeleton = prev_skeleton,
    author_list = author_list,
    title = title,
    rerender_skeleton = TRUE,
    office = office,
    spp_image = spp_image,
    species = species,
    spp_latin = spp_latin,
    region = region,
    parameters = TRUE,
    custom_params = custom_params,
    bib_name = bib_name,
    year = year,
    type = type
  )
  
  #### Params chunk ----
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
    # Add in region if it's not null
    if (!is.null(region) & !any(grepl("region <- params$region", params_chunk))) {
      params_chunk <- append(
        params_chunk,
        "region <- params$region",
        after =  length(params_chunk) - 1
      )
    }
    if (!is.null(param_values) & !is.null(param_names)) {
      for (i in length(param_values)) {
        add_param <- glue::glue("{param_names[i]} <- params${param_names[i]}")
        params_chunk <- append(
          params_chunk,
          add_param,
          after =  length(params_chunk) - 1
        )
      }
    }
  }
  
  #### preamble ----
  question1 <- readline("Update the preamble to match entered arguments? (Y/N)")
  
  # answer question1 as n if session isn't interactive
  if (!interactive()) {
    question1 <- "n"
  }
  if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
    start_line <- grep("label: 'preamble'", prev_skeleton) - 1
    # find next trailing "```"` in case it was edited at the end
    end_line <- grep("```", prev_skeleton)[grep("```", prev_skeleton) > start_line][1]
    # preamble <- paste(prev_skeleton[start_line:end_line], collapse = "\n")
    preamble <- prev_skeleton[start_line:end_line]
    
    if (!is.null(model_results)) {
      # show message and make README stating model_results info
      mod_time <- as.character(file.info(fs::path(model_results), extra_cols = FALSE)$ctime)
      mod_msg <- paste(
        "Report is based upon model output from", model_results,
        "that was last modified on:", mod_time
      )
      cli::cli_alert_info(mod_msg)
      writeLines(
        mod_msg,
        fs::path(
          file_dir,
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
        model_results # deparse(substitute(model_results))
      )
      # add back in pipe
      prev_results <- paste0(prev_results, " |>")
      preamble <- append(preamble, prev_results, after = prev_results_line)[-prev_results_line]
      
      # change chunk eval to true
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
      
      # if (!grepl(".csv", model_results)) warning("Model results are not in csv format - Will not work on render")
    } else {
      cli::cli_alert_info("Preamble maintained.")
      cli::cli_alert_info("Model results not updated.")
      preamble <- paste(preamble, collapse = "\n")
    }
  } else if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
    if (!is.null(model_results)) {# Assuming user saved converted output
      load_method <- glue::glue("load({model_results}) \n")
    } else {
      load_method <- ""
    }
    
    # standard preamble
    # copy preamble code into report folder
    file.copy(
      system.file("resources", "preamble.R", package = "asar"),
      file_dir,
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
  }
  
  #### disclaimer ----
  disclaimer <- "{{< pagebreak >}}\n\n## Disclaimer {.unnumbered .unlisted}\n\nThese materials do not constitute a formal publication and are for information only. They are in a pre-review, pre-decisional state and should not be formally cited or reproduced. They are to be considered provisional and do not represent any determination or policy of NOAA or the Department of Commerce.\n"
  
  #### citation ----
  if (title != "[TITLE]" | !is.null(species) | !is.null(year) | !is.null(authors)) {
    citation_line <- grep("Please cite this publication as:", prev_skeleton) + 2
    # citation <- glue::glue("{{< pagebreak >}} \n\n Please cite this publication as: \n\n {prev_skeleton[citation_line]}\n\n")
    # create the updated citation
    citation <- create_citation(
      authors = authors,
      title = title,
      year = year
    )
  } else {
    author <- grep("  - name: ", prev_skeleton)
    citation <- create_citation(
      authors = authors
    )
    cli::cli_alert_success("Added report citation.")
  }
  
  #### Create report outline (sections) ----
  # id the order of the files in the skeleton and copy over in that order
  files_to_copy <- stringr::str_extract(prev_skeleton[grep("knitr::knit_child", prev_skeleton)], "(?<=knit_child\\(').*?(?=\\')")
  
  if (!is.null(new_section) || !is.null(custom_sections)) custom <- TRUE else custom <- FALSE
  
  if (is.null(custom_sections)) {
    # identify all previous sections
    sections <- stringr::str_extract_all(
      prev_skeleton,
      "(?<=['`])[^']+\\.qmd(?=['`])"
    ) |>
      unlist() |>
      purrr::discard(~ .x == "")
    
    has_legacy_tables <- can_rename_legacy_doc(tbl_info)
    has_legacy_figures <- can_rename_legacy_doc(fig_info)
    
    if (has_legacy_tables) {
      sections <- stringr::str_replace_all(
        sections,
        tbl_info$legacy_name,
        tbl_info$current_name
      )
    }
    if (has_legacy_figures) {
      sections <- stringr::str_replace_all(
        sections,
        fig_info$legacy_name,
        fig_info$current_name
      )
    }
    
    figure_name <- if (has_legacy_figures) fig_info$current_name else figures_doc_name
    table_name <- if (has_legacy_tables) tbl_info$current_name else tables_doc_name
    
    figure_position <- which(sections == figure_name)
    table_position <- which(sections == table_name)
    if (length(figure_position) == 1 && length(table_position) == 1 && figure_position > table_position) {
      sections <- sections[sections != figure_name]
      table_position <- which(sections == table_name)
      sections <- append(
        sections,
        figure_name,
        after = table_position - 1
      )
    }
    
    # add sections as list
    sections <- add_child(
      sections,
      label = gsub(".qmd", "", unlist(sections))
    )
  } else {
    sections <- custom_true(
      new_section = new_section,
      section_location = section_location,
      custom_sections = custom_sections,
      files_to_copy = files_to_copy,
      tables_doc_name = tables_doc_name,
      figures_doc_name = figures_doc_name,
      subdir = file_dir
    )
  }

  #### Pull together template ----
  report_template <- paste(
    yaml,
    "\\printnoidxglossaries \n",
    paste(params_chunk, collapse = "\n"),
    preamble,
    disclaimer,
    citation,
    sections,
    sep = "\n"
  )
  #### save skeleton file ----
  utils::capture.output(cat(report_template), file = file.path(file_dir, new_report_name), append = FALSE)
  
  # Delete old skeleton
  if (length(grep("skeleton.qmd", list.files(file_dir, pattern = "skeleton.qmd"))) > 1) {
    question1 <- readline("Deleting previous skeleton file... Do you want to proceed? (Y/N)")
    
    # answer question1 as y if session isn't interactive
    if (!interactive()) {
      question1 <- "y"
    }
    
    if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
      file.remove(file.path(file_dir, report_name))
    } else if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
      cli::cli_alert_info("Skeleton file retained.")
    }
  }
  # Print message
  cli::cli_alert_success("Updated report skeleton in directory {file_dir}.")
}