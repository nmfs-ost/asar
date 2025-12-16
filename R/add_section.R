#' Add New Section or Subsection to Template
#'
#' @inheritParams create_template
#' @param subdir Directory where the new sections will be saved. In the
#' create_template function, this defaults to the location where the
#' template is saved.
#'
#' @return Add an additional section or subsection to the report template
#' if it is not already present in the default template. This provides
#' the option to add it as a section before or after an existing section,
#' or within a section as a child document. For developers: this function
#' creates a list of sections that will be added to the skeleton file
#' made from create_template.
#' @export
#'
#' @examples add_section(
#'   new_section = "Ecosystem Considerations", section_location = "after-discussion",
#'   custom_sections = c("introduction.qmd", "model.qmd", "results.qmd", "discussion.qmd"),
#'   subdir = tempdir()
#' )
add_section <- function(
  subdir = NULL,
  custom_sections = NULL,
  new_section = NULL,
  section_location = NULL
) {
  if (is.null(new_section)) {
    cli::cli_abort("New section name (`new_section`) is NULL.")
  }
  if (is.null(section_location)) {
    cli::cli_abort("Location of new section (`section_location`) is NULL.")
  }
  # Location options
  # before-section
  # after-section
  # in-section (will always append to the end of the section)
  for (i in seq_along(new_section)) {
    section_i_name <- paste0(gsub(" ", "_", tolower(new_section[i])), ".qmd")
    local_section <- forstringr::str_extract_part(
      section_location[i], "-",
      before = FALSE
    )
    # looking at the section identified for the previous entry
    local_section_prev <- forstringr::str_extract_part(
      section_location[i - 1], "-",
      before = FALSE
    )

    # Extracting where the new section should be place in relation to a base section
    if (any("TRUE" %in% grepl("-", section_location))) {
      locality <- forstringr::str_extract_part(
        section_location[i], "-",
        before = TRUE
      )
      # run again in case there are double -
      if (grepl("-", locality)) {
        locality <- forstringr::str_extract_part(
          locality, "-",
          before = TRUE
        )
      }

      locality_prev <- forstringr::str_extract_part(
        section_location[i - 1], "-",
        before = TRUE
      )
    } else if (any("TRUE" %in% grepl(" ", section_location))) {
      locality <- forstringr::str_extract_part(
        section_location[i], " ",
        before = TRUE
      )
      # run again in case there are double -
      if (grepl("-", locality)) {
        locality <- forstringr::str_extract_part(
          locality, "-",
          before = TRUE
        )
      }

      locality_prev <- forstringr::str_extract_part(
        section_location[i - 1], "-",
        before = TRUE
      )
    }

    section_i <- paste0(
      ifelse(locality == "in", "### ", "## "), stringr::str_to_title(sub("_", " ", tolower(new_section[i]))), "\n",
      "\n",
      "[Insert text here]", "\n",
      "\n",
      add_chunk("# Insert code", label = paste0("example_chunk_", i)), "\n"
    )

    # Export new section .qmd - catch when this fails so that the user can adjust
    tryCatch(
      {
        utils::capture.output(
          cat(section_i),
          file = paste0(subdir, "/", section_i_name),
          append = FALSE
        )
        # TRUE
      },
      error = function(e) {
        cli::cli_abort("Unable to create new section. Please check your file path (file_dir).")
        # FALSE
      }
    )

    if (locality == "before") {
      custom_sections <- append(
        unlist(custom_sections),
        section_i_name,
        after = (which(grepl(local_section, custom_sections))[1] - 1)
      )
    } else if (locality == "after") {
      if (locality %in% locality_prev & local_section %in% local_section_prev) {
        custom_sections <- append(
          unlist(custom_sections),
          section_i_name,
          after = which(grepl(
            gsub(" ", "_", tolower(new_section[i - 1])),
            custom_sections
          ))
        )
      } else {
        custom_sections <- append(
          unlist(custom_sections),
          section_i_name,
          after = max(which(grepl(local_section, custom_sections)))
        )
      }
    } else if (locality == "in") {
      # cli::cli_abort("No available option for adding a new section 'in' another quarto document.", call. = FALSE)
      # recognize locality_prev file
      file_for_subsection <- list.files(file.path(subdir))[grep(local_section, list.files(file.path(subdir)))]
      if (length(file_for_subsection) == 0) {
        cli::cli_abort(c("Unable to find the template file containing the target location of new section file.",
          "i" = "Did you correctly enter the `section_location`?",
          "x" = "You entered `section_location` = {section_location}"
        ))
      }
      # create code for reading in child doc
      child_sec <- add_child(
        section_i_name,
        label = gsub(" ", "_", tolower(new_section[i]))
      ) |>
        stringr::str_remove("\\n \\{\\{< pagebreak >\\}\\} \\n")
      # append that text to file
      # if (!file.exists(fs::path(subdir, file_for_subsection)))
      utils::capture.output(cat(child_sec),
        file = fs::path(subdir, file_for_subsection),
        append = TRUE
      )
      # section does not need to be added to appended custom sections as stated above
      # creating qmd is already done in line 48
    } else {
      cli::cli_abort(c("Invalid selection for section placement (`section_location`).",
        "i" = "Did you correctly enter the `section_location`?",
        "i" = "You entered `section_location` = {section_location}.",
        "i" = "Use the following format to add a new section, where placement = in, before, or after: 'placement-section_name'."
      ))
    }
  } # close for loop
  if (!is.null(custom_sections)) custom_sections
}
