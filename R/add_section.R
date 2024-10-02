#' Add New Section or Subsection to Template
#'
#' @param sec_names Names of section(s) (e.g., introduction, results) or
#' subsection(s) (e.g., a section within the introduction) that will be
#' added to the document. Please make a short list if >1 section/subsection
#' will be added. The template will be created as a quarto document, added
#' into the skeleton, and saved for reference.
#' @param location Where the section(s)/subsection(s) will be added to
#' the skeleton template. Please use the notation of 'placement-section'.
#' For example, 'in-introduction' signifies that the new content would
#' be created as a child document and added into the 02_introduction.qmd.
#' To add >1 (sub)section, make the location a list corresponding to the
#' order of (sub)section names listed in the 'sec_names' parameter.
#' @param other_sections List of other sections in which the added (sub)sections
#' will be added (if not NULL).
#' @param subdir Directory where the new sections will be saved. In the
#' create_template function, this defaults to the location where the
#' template is saved.
#'
#' @return Add an additional section or subsection to the report template
#' if it is not already present in the default template. This provides
#' the option to add it as a section before or after an existing section,
#' or within a section as a child document.
#' @export
#'
#' @examples add_section(
#'   sec_names = "Ecosystem Considerations", location = "after-discussion",
#'   other_sections = c("introduction.qmd", "model.qmd", "results.qmd", "discussion.qmd"),
#'   subdir = tempdir()
#' )
add_section <- function(
    sec_names = NULL,
    location = NULL,
    other_sections = NULL,
    subdir = NULL) {
  # Location options
  # before-section
  # after-section
  # in-section (will always append to the end of the section)
  for (i in 1:length(sec_names)) {
    section_i_name <- paste0(gsub(" ", "_", sec_names[i]), ".qmd")
    local_section <- forstringr::str_extract_part(location[i], "-", before = FALSE)
    local_section_prev <- forstringr::str_extract_part(location[i - 1], "-", before = FALSE)

    if (any("TRUE" %in% grepl("-", location))) {
      locality <- forstringr::str_extract_part(location[i], "-", before = TRUE)
      locality_prev <- forstringr::str_extract_part(location[i - 1], "-", before = TRUE)
    } else if (any("TRUE" %in% grepl(" ", location))) {
      locality <- forstringr::str_extract_part(location[i], " ", before = TRUE)
      locality_prev <- forstringr::str_extract_part(location[i - 1], "-", before = TRUE)
    }

    section_i <- paste0(
      "## ", stringr::str_to_title(sub("_", " ", sec_names[i])), "\n",
      "\n",
      "[Insert text here]", "\n",
      "\n",
      add_chunk("# Insert code", label = "example_chunk"), "\n"
    )
    utils::capture.output(cat(section_i), file = paste0(subdir, "/", section_i_name), append = FALSE)

    if (locality == "before") {
      other_sections <- append(other_sections, section_i_name, after = (which(grepl(local_section, other_sections)) - 1))
    } else if (locality == "after") {
      if (locality %in% locality_prev & local_section %in% local_section_prev) {
        other_sections <- append(other_sections, section_i_name, after = which(grepl(gsub(" ", "_", sec_names[i - 1]), other_sections)))
      } else {
        other_sections <- append(other_sections, section_i_name, after = which(grepl(local_section, other_sections)))
      }
    } else if (locality == "in") {
      stop("No available option for adding a new section 'in' another quarto document.", call. = FALSE)
    } else {
      stop("Invalid selection for placement of section. Please name the follow the format 'placement-section_name' for adding a new section.")
    }
  }
  other_sections
}
