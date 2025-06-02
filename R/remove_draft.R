#' Remove draft watermark
#'
#' @param x Quarto file containing the draft watermark notation.
#' @param dir Path to folder where the quarto containing the draft watermark
#' notation is located. Defaults to the working directory.
#'
#' @return Remove the notation adding a watermark "DRAFT" from the skeleton.qmd
#' file produced from `create_template()`. The function requires the user to
#' either be in the working directory where the skeleton is located or use the
#' `dir` argument to direct the function to where to file is located. The user
#'  must use this before rendering the final report for the watermark to be
#'  removed.
#' @export
#'
#' @examples
#' \dontrun{
#' create_template()
#' remove_draft(x = "SAR_species_skeleton.qmd", dir = file.path(getwd(), "report"))
#' }
remove_draft <- function(
    x = list.files(getwd())[grep("skeleton.qmd", list.files(getwd()))],
    dir = getwd()) {
  # Read in skeleton
  if (any(grepl("skeleton.qmd", list.files(dir)))) {
    # case where user only identifies the dir and not x
    x <- list.files(dir)[grep("skeleton.qmd", list.files(dir))]
    skeleton <- readLines(file.path(dir, x))
  } else if (file.exists(file.path(dir, x))) {
    # case if user is in wd and does not identify any args
    skeleton <- readLines(file.path(dir, x))
  } else {
    stop("Skeleton file not found.")
  }
  skeleton <- readLines(file.path(dir, x))
  # Remove draft watermark lines
  # Find format
  format_line <- skeleton[grep("format:", skeleton) + 1]
  # extract the string before : in format_line
  format <- gsub(":", "", gsub(" ", "", format_line))
  # Find line where draft code is located based on format
  if (format == "pdf") {
    find_header <- grep("header-includes:", skeleton)
    # Remove lines
    skeleton <- skeleton[-(find_header:(find_header + 2))]
  } else if (format == "html") {
    find_html <- grep("=html", skeleton)
    # Remove lines
    skeleton <- skeleton[-(find_html:(find_html + 3))]
  } else if (format == "docx") {
    stop("Docx is not a functional format for this tool.")
  } else {
    stop("Invalid render format.")
  }
  # Save skeleton over previous input file
  writeLines(skeleton, file.path(dir, x))
}
