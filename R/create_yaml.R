#' Create string for yml header in quarto file
#'
#' @inheritParams create_template
#' @param author_list A vector of strings containing pre-formatted author names
#' and affiliations that would be found in the format in a yaml of a quarto
#' file when using cat(author_list[[i]]).
#' @param bib_name Name of a bib file being added into the yaml. For example,
#' "asar.bib".
#' @param prev_skeleton Vector of strings containing all the lines of the
#' previous skeleton file. File is read in using the function readLines from
#' base R.
#' @param prev_format The format that the previous skeleton was directed to
#' render to. Parameter is inherited from create_template.
#' @param author_list A list of strings containing pre-formatted author names
#' and affiliations that would be found in the format in a yaml of a quarto
#' file when using `cat(author_list[[i]])`.
#' @param bib_name Name of a bib file being added into the yaml. For example,
#' "asar.bib".
#'
#' @return Create a string indicating the important formatting pieces for a
#' quarto file for a stock assessment report.
#' @export
#'
#' @examples
#' \dontrun{
#' create_yaml(
#'   rerender_skeleton = FALSE,
#'   prev_skeleton = NULL,
#'   title = "My title",
#'   author_list = "  - name: 'Patrick Star'\n    affiliations:\n      - name: 'NOAA Fisheries Southeast Fisheries Science Center'\n        address: '75 Virginia Beach Drive'\n        city: 'Miami'\n        state: 'FL'\n        postal-code: '33149'\n",
#'   author = c("Patrick Star" = "SEFSC"),
#'   office = "SEFSC",
#'   add_author = NULL,
#'   spp_image = NULL,
#'   species = "",
#'   spp_latin = NULL,
#'   region = NULL,
#'   format = "pdf",
#'   parameters = TRUE,
#'   param_names = NULL,
#'   param_values = NULL,
#'   bib_file = "path/asar_references.bib",
#'   bib_name = "asar_references.bib",
#'   year = 2025
#' )
#' }
create_yaml <- function(
    format = "pdf",
    office = NULL,
    region = NULL,
    species = "species",
    spp_latin = NULL,
    spp_image = "",
    year = NULL,
    bib_name = NULL,
    bib_file = "asar_references.bib",
    author_list = NULL,
    title = "[TITLE]",
    rerender_skeleton = FALSE,
    prev_skeleton = NULL,
    prev_format = NULL,
    parameters = TRUE,
    param_names = NULL,
    param_values = NULL,
    type = "SAR") {
  # check first if want to rerender current skeleton
  if (rerender_skeleton) {
    # Extract yaml from current template
    yaml_lines <- grep("---", prev_skeleton)
    yaml <- unlist(prev_skeleton[yaml_lines[1]:yaml_lines[2]])
    # Change format if different
    # find format line
    if (prev_format == "pdf" & format == "html") {
      new_format <- unlist(stringr::str_split(format_quarto("html"), "\n"))
      # cat(new_format)
      yaml <- yaml[-(grep("pdf-engine:", yaml):(grep("output-file:", yaml) - 1))]
      yaml <- append(yaml, new_format[-length(new_format)], after = grep("cover:", yaml))
    }

    # add authors
    # if (any(!is.null(author))) {
    # add_authors <- NULL
    # for (i in 1:length(author_list)) {
    #   toad <- paste(author_list[[i]], sep = ",")
    #   add_authors <- paste0(add_authors, toad) # -> add_authors
    # }
    add_authors <- unlist(stringr::str_split(author_list, "\n"))
    # remove trailing \n from each author entry
    add_authors <- gsub("\n", "", add_authors)
    # check if the template was blank before
    author_line <- grep("author:", yaml)
    # Replacing the empty author if template was made before adding any authors
    if (grepl("- name: 'FIRST LAST'", yaml[author_line + 1])) {
      yaml <- yaml[-((author_line + 1):(author_line + 8))]
      yaml <- append(yaml, add_authors, after = utils::tail(grep("author:", yaml), n = 1))
    } else {
      yaml <- append(yaml, add_authors, after = utils::tail(grep("postal-code:", yaml), n = 1))
    }
    # }

    # replace title
    # DOES NOT WORK when latin latex notation is in the title
    # TODO: replace {} in the latex notation
    yaml <- stringr::str_replace(yaml, yaml[grep("title:", yaml)], paste("title: ", title, sep = ""))

    # add add'l param names
    # this occurs below
    # if (!is.null(param_names) & !is.null(param_values)) {
    #   add_params <- paste("  ", " ", param_names, ": ", "'", param_values, "'", sep = "")
    #   yaml <- append(yaml, add_params, after = grep("bibliography:", yaml) - 1)
    # }

    # add in spp image
    if (species != "species") {
      yaml <- stringr::str_replace(yaml, yaml[grep("cover: ", yaml)], paste("cover: ", spp_image, sep = ""))
    }

    # Replace output-file name
    out_name <- glue::glue("{tolower(species)}_{type}_{year}")
    yaml <- stringr::str_replace(yaml, yaml[grep("output-file: ", yaml)], paste("output-file: ", out_name, sep = ""))

    # Parameters
    # office, region, and species are default parameters
    if (parameters) {
      # check if params is already in yaml, if not then add in params: following with the other lines
      if (!grep("params:", yaml)) {
        yaml <- append(yaml, "params:", after = grep("output-file:", yaml))
      }
      # if species, office, and latin are updated - replace in space
      if (!is.null(species) & any(grepl("species: ''", yaml))) {
        yaml <- stringr::str_replace(yaml, yaml[grep("species: ''", yaml)], paste("  ", " ", "species: ", "'", species, "'", sep = ""))
      }
      if (length(office) == 1 & !is.null(office) & any(grepl("office: ''", yaml))) {
        yaml <- stringr::str_replace(yaml, yaml[grep("office: ''", yaml)], paste("  ", " ", "office: ", "'", office, "'", sep = ""))
      }
      if (!is.null(spp_latin) & any(grepl("spp_latin: ''", yaml))) {
        yaml <- stringr::str_replace(yaml, yaml[grep("spp_latin: ''", yaml)], paste("  ", " ", "spp_latin: ", "'", spp_latin, "'", sep = ""))
      }
      # if params are not entered - use previous ones else change
      # TODO: add check for params not being replicated of default
      if (!is.null(param_names) | !is.null(param_values)) {
        if (length(param_names) != length(param_values)) {
          print("Please define ALL parameter names (param_names) and values (param_values).")
        } else {
          add_params <- NULL
          for (i in 1:length(param_names)) {
            toad <- paste("  ", " ", param_names[i], ": ", "'", param_values[i], "'", sep = "")
            add_params <- c(add_params, toad)
          } # close loop
          # add params into yaml
          yaml <- append(yaml, add_params, after = grep("params: ", yaml))
        } # close check
      } # close if adding add'l params
    } # close if params to be included in template

    # add bib file name
    if (bib_name != "asar_references.bib") {
      # check if input bib file contains a path
      if (file.exists(bib_file)) {
        cli::cli_alert("Copying bibliography file to report folder...")
        file.copy(bib_file, file_dir)
        # bib_file_only <- stringr::str_extract(bib_file, "[^/]+$")
        bib_format <- paste("-  ", bib_name, sep = "")
      } else if (!file.exists(file.path(file_dir, bib_file))) {
        cli::cli_alert_warning("Bibliography file {bib_file} is not in the report directory. The file will not be read in on render if it is not in the same path as the skeleton file.",
          wrap = TRUE
        )
        bib_format <- paste("-  ", bib_name, sep = "")
      }
    }
    yaml <- paste(yaml, collapse = " \n")
  } else { # not rerendering skeleton
    # Creating YAML
    yaml <- paste0(
      # start YAML notation
      "---", "\n",

      # Tile
      "title: ", "'", title, "'", "\n",

      # Author
      "author:", "\n"
    )
    # Add authors
    add_authors <- NULL
    for (i in 1:length(author_list)) {
      toad <- paste(author_list[[i]], sep = ",")
      add_authors <- paste0(add_authors, toad) # -> add_authors
    }
    yaml <- paste0(yaml, add_authors)

    # Add other parts
    yaml <- paste0(
      yaml,
      # Date
      "date: today", "\n",
      # "lang: en \n",
      "keep-tex: true \n",
      "mainfont: 'Latin Modern Sans' \n"
    )

    # Add species image on title page
    if (spp_image == "") {
      yaml <- paste0(
        yaml,
        # image as pulled in from above
        "cover: ", spp_image, "\n"
      )
    } else {
      yaml <- paste0(
        yaml,
        # image as pulled in from above
        "cover: support_files/", gsub(" ", "_", species), ".png", "\n"
      )
    }

    # Add lualatex engine
    if (format == "pdf") {
      yaml <- paste0(
        yaml,
        "pdf-engine: lualatex", "\n"
      )
    }
    # Add quarto format
    # quarto_formatting <- format_quarto(format = format)

    # Formatting
    yaml <- paste0(
      yaml,
      format_quarto(
        format = format,
        type = type
      ),
      # Add in output file name (Rendered name of pdf)
      "output-file: '", stringr::str_replace_all(species, " ", "_"), ifelse(is.null(species), "SAR_", "_SAR_"), year, "'", " \n"
    )

    # Add lua filters for compliance
    # PLACEHOLDER: Uncomment once .lua text is built

    # yaml <- paste0(yaml,
    #                # "contributes:", "\n",
    #                "filters:", "\n",
    #                "  ", "  ", "- acronyms.lua", "\n",
    #                "  ", "  ", "- accessibility.lua", "\n")

    # Parameters
    # office, region, and species are default parameters
    if (parameters) {
      yaml <- paste0(
        yaml, "params:", "\n",
        "  ", " ", "office: ", "'", office, "'", "\n",
        "  ", " ", "species: ", "'", species, "'", "\n",
        "  ", " ", "spp_latin: ", "'", spp_latin, "'", "\n"
      )
      if (!is.null(region)) {
        yaml <- paste0(yaml, "  ", " ", "region: ", "'", region, "'", "\n")
      }
      # Add more parameters if indicated
      if (!is.null(param_names) & !is.null(param_values)) {
        # check there are the same number of names and values
        if (length(param_names) != length(param_values)) {
          print("Please define ALL parameter names (param_names) and values (param_values).")
        } else {
          add_params <- NULL
          for (i in 1:length(param_names)) {
            toad <- paste("  ", " ", param_names[i], ": ", "'", param_values[i], "'", "\n", sep = "")
            add_params <- paste0(add_params, toad)
          } # close loop
        } # close check
        yaml <- paste0(yaml, add_params)
      } # close if adding add'l params
    } # close if params to be included in template

    # Add style guide
    # create_style_css(species = species, savedir = subdir)

    # yaml <- paste0(
    #   yaml,
    #   "css: styles.css", "\n"
    # )

    # Add option for bib file
    # if (!is.null(bib_file)) {
    bib <- glue::glue(
      "bibliography: ", "\n"
    )
    bib_all <- paste0("  ", "- ", bib_file, "\n", collapse = "")
    bib <- glue::glue("{bib} \n {bib_all}")
    yaml <- paste(yaml, bib, sep = "")
    # }
    # add in else statement once a national .bib file is made

    # Close yaml
    yaml <- glue::glue("{yaml}---\n")
  }
  # return finished yaml string
  yaml
}
