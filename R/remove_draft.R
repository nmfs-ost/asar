remove_draft <- function(
    x = list.files(getwd())[grep("skeleton.tex", list.files(getwd()))],
    dir = getwd()) {
  # Read in skeleton
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
