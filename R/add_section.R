#' Add New Section to Template
#'
#' @param title title of the subsection. Please keep this short as the template
#'              will be created as a quarto document into your skeleton and saved
#'              later for reference. More than one section can be added. Please
#'              make a list if this is desired.
#' @param location Where the section will be added in the generic template using
#'                 the notation of 'placement-section'. Example would be 'in-introduction'
#'                 where the new content would be created as a child document
#'                 and added into the 02_introduction.qmd. There is an option to
#'                 add more than one section. Add the location as a list
#'                 corresponding to the order of section names listed in the
#'                 'title' parameter
#' @param multi TRUE/FALSE, more than one section is being added
#'
#' @return Add an additional section to the report template if it is not already
#'         in the default template. This provides the option to adding it as a
#'         subsection either before or after it or within the subsection itself
#'         as a child document.
#' @export
#'
#' @examples add_section(title = "Ecosystem Considerations", location = "after-discussion")
add_section <- function(
    title = NULL,
    location = NULL,
    multi = FALSE) {
  # Location options
  # before-section
  # after-section
  # in-section (will always apend to the end of the section)
  section_name <- forstringr::str_extract_part(location, "-", before = FALSE)

  if (grepl("before-", location)) {

  }

  if (grepl("in-", location)) {

  }

  if (grepl("after-", location)) {

  }
}
