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
    new_section = NULL,
    section_location = NULL,
    custom_sections = NULL,
    custom = TRUE,
    subdir = NULL) {
  # Location options
  # before-section
  # after-section
  # in-section (will always append to the end of the section)
  for (i in 1:length(new_section)) {
    section_i_name <- paste0(gsub(" ", "_", tolower(new_section[i])), ".qmd")
    local_section <- forstringr::str_extract_part(
      section_location[i], "-",
      before = FALSE
    )
    local_section_prev <- forstringr::str_extract_part(
      section_location[i - 1], "-",
      before = FALSE
    )

    if (any("TRUE" %in% grepl("-", section_location))) {
      locality <- forstringr::str_extract_part(
        section_location[i], "-",
        before = TRUE
      )
      locality_prev <- forstringr::str_extract_part(
        section_location[i - 1], "-",
        before = TRUE
      )
    } else if (any("TRUE" %in% grepl(" ", section_location))) {
      locality <- forstringr::str_extract_part(
        section_location[i], " ",
        before = TRUE
      )
      locality_prev <- forstringr::str_extract_part(
        section_location[i - 1], "-",
        before = TRUE
      )
    }

    section_i <- paste0(
      "## ", stringr::str_to_title(sub("_", " ", tolower(new_section[i]))), "\n",
      "\n",
      "[Insert text here]", "\n",
      "\n",
      add_chunk("# Insert code", label = "example_chunk"), "\n"
    )
    utils::capture.output(
      cat(section_i),
      file = paste0(subdir, "/", section_i_name),
      append = FALSE
    )

    if (locality == "before") {
      custom_sections <- append(
        custom_sections,
        section_i_name,
        after = (which(grepl(local_section, custom_sections)) - 1)
      )
    } else if (locality == "after") {
      if (locality %in% locality_prev & local_section %in% local_section_prev) {
        custom_sections <- append(
          custom_sections,
          section_i_name,
          after = which(grepl(
            gsub(" ", "_", tolower(new_section[i - 1])),
            custom_sections
          ))
        )
      } else {
        custom_sections <- append(
          custom_sections,
          section_i_name,
          after = max(which(grepl(local_section, custom_sections)))
        )
      }
    } else if (locality == "in") {
      # stop("No available option for adding a new section 'in' another quarto document.", call. = FALSE)
      # recognize locality_prev file
      file_for_subsection <- list.files(file.path(subdir))[grep(local_section, list.files(file.path(subdir)))]
      # create code for reading in child doc
      child_sec <- add_child(
        section_i_name,
        label = gsub(" ", "_", tolower(new_section[i]))
      )
      # append that text to file
      if (!file.exists(fs::path(subdir, file_for_subsection))) utils::capture.output(cat(child_sec), file = fs::path(subdir, file_for_subsection), append = TRUE)
      # section does not need to be added to appended custom sections as stated above
      # creating qmd is already done in line 48
    } else {
      stop("Invalid selection for placement of section. Please name the follow the format 'placement-section_name' for adding a new section.")
    }
  } # close for loop
  custom_sections
}
