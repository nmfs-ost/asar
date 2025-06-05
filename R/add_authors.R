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
add_authors <- function(author, rerender_skeleton = FALSE) {
  # Check if rerender and if author is already added
  # TODO: add feature to allow removal of authors if there are ones that
  # are repeated from the previous skeleton and those named (not just
  # additions of new names)
  if (rerender_skeleton) {
    # Pull all author names from prev_skeleton
    author_prev <- grep(
      "\\- name:\\s*'",
      prev_skeleton,
      value = TRUE
    )
    # Remove every second occurance of "-name"
    author_prev <- author_prev[seq(1, length(author_prev), 2)]
    # Remove everything but the name
    author_prev <- sub(
      ".*\\- name:\\s*'([^']+)'.*",
      "\\1",
      author_prev
    )
    setdiff(author, author_prev) -> author
  }
  # Read authorship file
  # authors <- utils::read.csv(system.file("resources", "authorship.csv", package = "asar", mustWork = TRUE)) |>
  #   dplyr::mutate(
  #     mi = dplyr::case_when(
  #       mi == "" ~ NA,
  #       TRUE ~ mi
  #     ),
  #     name = dplyr::case_when(
  #       is.na(mi) ~ paste0(first, " ", last),
  #       TRUE ~ paste(first, mi, last, sep = " ")
  #     )
  #   ) |>
  #   dplyr::select(name, office) |>
  #   dplyr::filter(name %in% author)
  # 
  # if (length(author) != dim(authors)[1]){
  #   message("Some authors were not found in the author database. Please comment on this issue (https://github.com/nmfs-ost/asar/issues/19) to request name and affiliation additions to the archive of U.S. stock assessment authors.")
  # }
  
  # authors <- authors[match(author, authors$name), ]
  
  # Place author naming into same format as before
  authors <- data.frame(office = author) |>
    tibble::rownames_to_column("name")
  
  affil <- utils::read.csv(system.file("resources", "affiliation_info.csv", package = "asar", mustWork = TRUE))
  
  author_list <- list()
  if (nrow(authors) > 0) {
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
  } # close if else statement
  
  return(author_list)
} # close function