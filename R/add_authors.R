#' Format authors for skeleton
#'
#' @inheritParams create_template
#'
#' @returns A list of authors formatted for a yaml in quarto. Viewable by running the 
#' return object inside of cat() for each part of the list.
#' @export
#'
#' @examples
#' \dontrun{
#' add_authors(
#'   author = c("Danny Phantom"="SWFSC-LJCA","John Snow"="AFSC-ABL","Jane Doe"="NWFSC-SWA"),
#'   rerender_skeleton = FALSE
#' )
#' }
add_authors <- function(
    author,
    rerender_skeleton = FALSE,
    prev_skeleton = NULL) {
  # Set author into proper format - will get overwritten later if rerender = T
  author_names <- names(author)
  # Get authors into readable format for ordering
  authors <- data.frame(name = author_names, office = author, row.names = NULL)
  
  # Check if rerender and if author is already added
  # TODO: add feature to allow removal of authors if there are ones that
  # are repeated from the previous skeleton and those named (not just
  # additions of new names)
  if (rerender_skeleton) {
    
    if (is.null(prev_skeleton)) {
      # attempt to find the skeleton file
      if (file.exists(file.path(getwd(), list.files(file_dir, pattern = "skeleton.qmd")))) {
        prev_skeleton <- readLines(file.path(getwd(), list.files(file_dir, pattern = "skeleton.qmd")))
      } else {
        cli::cli_abort("No skeleton quarto file found in the working directory.")
      }
    }
    
    # Pull all author names from prev_skeleton
    author_prev <- grep(
      "\\- name:\\s*'",
      prev_skeleton,
      value = TRUE
    )
    
    # Remove every second occurance of "-name"
    if (length(author_prev) <= 2) {
      author_prev <- author_prev[1]
    } else {
      author_prev <- author_prev[seq(1, length(author_prev), 2)]
    }
    
    # Remove everything but the name
    author_prev <- sub(
      ".*\\- name:\\s*'([^']+)'.*",
      "\\1",
      author_prev
    )
    
    setdiff(author_names, author_prev) -> author2
    
    # return if null
    if (is.null(author2)) {
      return(NULL) # this line should stop the function here 
    } else {
      # Continue if there are authors
      # subset authors with only ones new ones
      authors <- dplyr::filter(authors, name %in% author2)
    }
  }
  
  # Load in affiliation
  affil <- utils::read.csv(system.file("resources", "affiliation_info.csv", package = "asar", mustWork = TRUE))
  
  author_list <- list()
  if (any(authors$name != "1")) { # nrow(authors) > 0 |
    # print("inside author==1")
    if (rerender_skeleton) {
      author_lines <- grep(
        "\\- name:\\s*'",
        prev_skeleton,
        value = TRUE
      )
      authors_prev <- sub(
        ".*\\- name:\\s*'([^']+)'.*",
        "\\1",
        author_lines
      )
      # remove authors previously in skeleton and keep new additions either from author
      author_to_add <- setdiff(authors$name, authors_prev)
      authors <- authors |>
        dplyr::filter(name %in% author_to_add)
    }
    for (i in 1:nrow(authors)) {
      auth <- authors[i, ]
      aff <- affil |>
        dplyr::filter(affiliation == auth$office)
      if (is.na(auth$office) | auth$office == "") {
        paste(
          "  ", "- name: ", "'", auth$name, "'", "\n",
          "  ", "  ", "affiliations:", "\n",
          "  ", "  ", "  ", "- name: '[organization]'", "\n", # "NOAA Fisheries ",
          "  ", "  ", "  ", "  ", "address: '[address]'", "\n",
          "  ", "  ", "  ", "  ", "city: '[city]'", "\n",
          "  ", "  ", "  ", "  ", "state: '[state]'", "\n",
          "  ", "  ", "  ", "  ", "postal-code: '[postal code]'", "\n",
          sep = ""
        ) -> author_list[[i]]
      } else {
        paste(
          "  ", "- name: ", "'", auth$name, "'", "\n",
          "  ", "  ", "affiliations:", "\n",
          "  ", "  ", "  ", "- name: ", "'", aff$name, "'", "\n", # "NOAA Fisheries ",
          "  ", "  ", "  ", "  ", "address: ", "'", aff$address, "'", "\n",
          # TODO: remove state in the following line when notation is changed in _titlepage.tex
          "  ", "  ", "  ", "  ", "city: ", "'", aff$city, ", ", aff$state, "'", "\n",
          "  ", "  ", "  ", "  ", "state: ", "'", aff$state, "'", "\n",
          "  ", "  ", "  ", "  ", "postal-code: ", "'", aff$postal.code, "'", "\n",
          sep = ""
        ) -> author_list[[i]]
      }
    }
  } else {
    if (rerender_skeleton) {
      author_list <- NULL
    } else {
      paste0(
        "  ", "- name: ", "'FIRST LAST'", "\n",
        "  ", "  ", "affiliations: \n",
        "  ", "  ", "  ", "- name: 'NOAA Fisheries' \n",
        "  ", "  ", "  ", "  ", "address: 'ADDRESS' \n",
        "  ", "  ", "  ", "  ", "city: 'CITY' \n",
        "  ", "  ", "  ", "  ", "state: 'STATE' \n",
        "  ", "  ", "  ", "  ", "postal-code: 'POSTAL CODE' \n"
        # sep = " "
      ) -> author_list[[1]]
    } # close rerender if else statement
  } # close if else statement
  
  return(author_list)
} # close function