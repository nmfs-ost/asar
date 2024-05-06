#' Add New Section to Template
#'
#' @param sec_names section_names of the subsection. Please keep this short as the template
#'                  will be created as a quarto document into your skeleton and saved
#'                  later for reference. More than one section can be added. Please
#'                  make a list if this is desired.
#' @param location Where the section will be added in the generic template using
#'                 the notation of 'placement-section'. Example would be 'in-introduction'
#'                 where the new content would be created as a child document
#'                 and added into the 02_introduction.qmd. There is an option to
#'                 add more than one section. Add the location as a list
#'                 corresponding to the order of section names listed in the
#'                 'title' parameter
#' @param other_sections List of other sections that the added sections will be combined with (if not NULL)
#' @param subdir directory where the new sections will be saved - in the create_template
#'               fxn this defaults to the location where the template is saved
#'
#' @return Add an additional section to the report template if it is not already
#'         in the default template. This provides the option to adding it as a
#'         subsection either before or after it or within the subsection itself
#'         as a child document.
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
      chunkr("# Insert code", label = "example_chunk"), "\n"
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
  return(other_sections)
}
