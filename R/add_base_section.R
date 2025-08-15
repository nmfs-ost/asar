#' Add selected sections to outline
#'
#' @inheritParams create_template
#'
#' @return Call and copy the sections in the package templates to create an
#' outline for a stock assessment
#' @export
#'
#' @examples add_base_section(c("executive summary", "assessment", "results"))
add_base_section <- function(custom_sections = NULL) {
  if (is.null(custom_sections)) cli::cli_abort("Custom sections list (`custom_sections`) is NULL.")

  sec_sel <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2", custom_sections)))
  section_list <- list()
  for (i in seq_along(sec_sel)) {
    sec_file <- grep(
      list.files(system.file("templates", "skeleton", package = "asar")),
      pattern = sec_sel[i],
      value = TRUE
    )
    if (identical(sec_file, character(0))) cli::cli_abort("One or more section name(s) does not exist. Please check the spelling or if you are tring to add a section that is not in the default template, please use parameter 'custom_sections' and refer to documentation. To check which sections are in the base template please run list.files(system.file('templates', 'skeleton', package = 'asar')) in your console")
    section_list[[i]] <- sec_file
  }
  as.list(unlist(section_list))
}
