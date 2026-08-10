#' Format author names for a citation
#'
#' Converts author names to the family-name-first format used in report
#' citations. Names containing given and family names are abbreviated using
#' their initials. A name with only one component, such as an initialism or a
#' mononym, is retained as the family name rather than causing name parsing to
#' fail.
#'
#' @param author_names A character vector containing one author name per
#'   element.
#'
#' @return A single character string containing the formatted author names,
#'   separated according to standard citation conventions.
#' @keywords internal
#' @noRd
format_citation_authors <- function(author_names) {
  author_names <- trimws(author_names)
  single_component <- !grepl("\\s", author_names)
  formatted_names <- character(length(author_names))

  # A single component cannot be reliably divided into given and family
  # names. Treating it as the family name preserves initials and mononyms.
  formatted_names[single_component] <- vapply(
    author_names[single_component],
    \(name) utils::toBibtex(utils::person(family = name)),
    character(1)
  )

  if (any(!single_component)) {
    formatted_names[!single_component] <- data.frame(
      input = author_names[!single_component]
    ) |>
      tidyr::separate_wider_regex(
        cols = input,
        # Caitlin Allen Akselrud is the only non-hyphenated dual last name
        # and needs to be included as its own pattern. The second pattern
        # allows for first initials rather than first names.
        patterns = c(first = "Caitlin |^[A-Z]. |.*[a-z] ", last = ".*$")
      ) |>
      tidyr::separate_wider_delim(
        cols = last,
        delim = ". ",
        names = c("mi", "last"),
        too_few = "align_end"
      ) |>
      dplyr::mutate(
        first = gsub(" ", "", first),
        mi = ifelse(is.na(mi), "", paste0(mi, ".")),
        first_initial = gsub("([A-Z])[a-z]+", "\\1.", first),
        bib = purrr::pmap_chr(
          list(x = first_initial, y = mi, z = last),
          \(x, y, z) {
            utils::toBibtex(
              utils::person(given = c(x, y), family = z)
            )
          }
        )
      ) |>
      dplyr::pull(bib)
  }

  formatted_names |>
    # `toBibtex()` adds a comma after a person with only a family name.
    # Remove it along with the trailing whitespace before joining authors.
    sub(pattern = ",?\\s*$", replacement = "") |>
    glue::glue_collapse(sep = ", ", last = ", and ") |>
    as.character()
}
