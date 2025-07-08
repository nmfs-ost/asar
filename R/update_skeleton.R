update_skeleton <- function(
    file_dir
) {
  # Add in report to file_dir
  file_dir <- file.path(file_dir, "report")
  #### Call in old skeleton ----
  # TODO: set up situation where species, region can be changed
  report_name <- list.files(file_dir, pattern = "skeleton.qmd") # gsub(".qmd", "", list.files(file_dir, pattern = "skeleton.qmd"))
  if (length(report_name) == 0) cli::cli_abort("No skeleton quarto file found in the `file_dir` ({file_dir}).")
  if (length(report_name) > 1) cli::cli_abort("Multiple skeleton quarto files found in the `file_dir` ({file_dir}).")
  
  prev_report_name <- gsub("_skeleton.qmd", "", report_name)
  # Extract type
  type <- stringr::str_extract(prev_report_name, "^[A-Z]+")
  # Extract region
  region <- stringr::str_extract(prev_report_name, "(?<=_)[A-Z]+(?=_)")
  # report name without type and region
  report_name_1 <- gsub(glue::glue("{type}_"), "", prev_report_name)
  # Extract species
  species <- gsub(
    "_",
    " ",
    gsub(glue::glue("{region}_"), "", report_name_1)
  )
  new_report_name <- paste0(
    type, "_",
    ifelse(is.null(region), "", paste(gsub("(\\b[A-Z])[^A-Z]+", "\\1", region), "_", sep = "")),
    ifelse(is.null(species), "species", stringr::str_replace_all(species, " ", "_")), "_",
    "skeleton.qmd"
  )
  
  #### Read in previous skeleton ----
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
  # Add in species image if updated in rerender
  if (species != "species") {
    file.copy(spp_image, supdir, overwrite = FALSE) |> suppressWarnings()
  }
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
  
  #### Adjust the title ---- 
  title <- sub("title: ", "", prev_skeleton[grep("title:", prev_skeleton)])
  if (title == "'Stock Assessment Report Template' " & (!is.null(office) | !is.null(species) | !is.null(region))) {
    title <- create_title(
      office = office,
      species = species,
      spp_latin = spp_latin,
      region = region,
      type = type,
      year = year
    )
  }
  
  #### authors ---- 
  # this may not be the case for calling in report
  author_list <- add_authors(
    prev_skeleton = prev_skeleton, NULL,
    author = author, # need to put this in case there is a rerender otherwise it would not use the correct argument
    rerender_skeleton = TRUE
  )
  
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
    parameters = parameters,
    param_names = param_names,
    param_values = param_values,
    bib_name = bib_name,
    bib_file = bib_file,
    year = year,
    type = type
  )
  
  #### preamble ----
  start_line <- grep("output_and_quantities", prev_skeleton) - 1
  # find next trailing "```"` in case it was edited at the end
  end_line <- grep("```", prev_skeleton)[grep("```", prev_skeleton) > start_line][1]
  # preamble <- paste(prev_skeleton[start_line:end_line], collapse = "\n")
  preamble <- prev_skeleton[start_line:end_line]
  
  if (!is.null(model_results)) {
    # show message and make README stating model_results info
    mod_time <- as.character(file.info(fs::path(model_results), extra_cols = F)$ctime)
    mod_msg <- paste(
      "Report is based upon model output from", model_results,
      "that was last modified on:", mod_time
    )
    cli::cli_alert_info(mod_msg)
    writeLines(
      mod_msg,
      fs::path(
        subdir,
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
      deparse(substitute(model_results))
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
  }
    
  #### disclaimer ----
  disclaimer <- "{{< pagebreak >}}\n\n## Disclaimer {.unnumbered .unlisted}\n\nThese materials do not constitute a formal publication and are for information only. They are in a pre-review, pre-decisional state and should not be formally cited or reproduced. They are to be considered provisional and do not represent any determination or policy of NOAA or the Department of Commerce.\n"
    
  #### citation ----
  if (!is.null(title) | !is.null(species) | !is.null(year) | !is.null(author)) {
    citation_line <- grep("Please cite this publication as:", prev_skeleton) + 2
    citation <- glue::glue("{{< pagebreak >}} \n\n Please cite this publication as: \n\n {prev_skeleton[citation_line]}\n\n")
  } else {
    author <- grep("  - name: ", prev_skeleton)
    citation <- create_citation(
      author = author,
      ...
    )
    cli::cli_alert_success("Added report citation.")
  }
  if (custom) { 
    stop("Not currently working")
  } else {
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
  }
  
  #### Pull together template ----
  report_template <- paste(
    yaml,
    "\\printnoidxglossaries \n",
    params_chunk,
    preamble,
    disclaimer,
    citation,
    sections,
    sep = "\n"
  )
  #### save skeleton file ----
  utils::capture.output(cat(report_template), file = file.path(file_dir, new_report_name), append = FALSE)
}