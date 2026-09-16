# rerender skeleton duplicated utils

custom_true <- function(
    new_section,
    section_location,
    custom_sections,
    files_to_copy,
    tables_doc_name,
    figures_doc_name,
    subdir
){
  ###### Rerender & custom ----
  # Option for building custom template
  # Create custom template from existing skeleton sections
  if (is.null(new_section)) {
    section_list <- add_base_section(files_to_copy)
    # Create sections object to add into template
    sections <- add_child(section_list,
                          label = stringr::str_extract(unlist(section_list), "(?<=_).+(?=\\.qmd$)")
    )
  } else { # custom = TRUE
    # Create custom template using existing sections and new sections from analyst
    # Add sections from package options
    
    if (is.null(custom_sections)) {
      # TODO: type - this needs to just pull all files from folder that
      # it was copying from when custom sections is null -- DONE
      
      sec_list1 <- unique(c(files_to_copy, tables_doc_name, figures_doc_name))
      sec_list2 <- add_section(
        new_section = new_section,
        section_location = section_location,
        custom_sections = sec_list1,
        subdir = subdir
      )
      
      # Create sections object to add into template
      sections <- add_child(
        sec_list2,
        label = stringr::str_remove_all(unlist(sec_list2), "^\\d{2}[a-zA-Z]?_|\\.qmd$")
      )
    } else { # custom_sections explicit
      
      # Add selected sections from base
      sec_list1 <- unique(c(unlist(add_base_section(files_to_copy)), tables_doc_name, figures_doc_name))
      # Create new sections as .qmd in folder
      # check if sections are in custom_sections list
      if (any(stringr::str_replace(section_location, "^[a-z]+-", "") %notin% custom_sections)) {
        cli::cli_abort("Defined customizations do not match one or all of the relative placement of a new section. Please review inputs.")
      }
      # reorder sec_list1 alphabetically so that 11_appendix goes to end of list
      sec_list1 <- sec_list1[order(names(stats::setNames(sec_list1, sec_list1)))]
      
      sec_list2 <- add_section(
        new_section = new_section,
        section_location = section_location,
        custom_sections = sec_list1,
        subdir = subdir
      )
      # Create sections object to add into template
      add_child(
        sec_list2,
        label = stringr::str_remove_all(unlist(sec_list2), "^\\d{2}[a-zA-Z]?_|\\.qmd$")
      )
    } # close if statement for very specific sectioning
  } # close if statement for extra custom
}