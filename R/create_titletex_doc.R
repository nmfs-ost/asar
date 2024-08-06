#' Create a title page latex document
#'
#' @param office primary science center writing the document
#' @param subdir directory where files are going to be held
#'
#' @return Create a _titlepage.tex document that contains formatting options for
#'  a cover page. The only thing that changes currently is the primary author's
#'  fishery science center.
#' @export
create_titletex_doc <- function(office = "",
                                subdir){
  if(office == "NEFSC"){
    center = "Northeast Fisheries Science Center"
  } else if (office == "NWFSC"){
    center = "Northwest Fisheries Science Center"
  } else if (office == "SEFSC"){
    center = "Southeast Fisheries Science Center"
  } else if (office == "SWFSC"){
    center = "Southwest Fisheries Science Center"
  } else if (office == "AFSC"){
    center = "Alaska Fisheries Science Center"
  } else if (office == "PIFSC"){
    center = "Pacific Islands Fisheries Science Center"
  }
  # Read basic latex file
  lines <- readLines(
    system.file("resources", "formatting_files", "_titlepage.tex", package = "ASAR")
  )
  if(office != "") {
    to_add <- paste(center, "\\newline", sep = "") # unlist(rlang::dots_list(...))
    lines <- append(lines, to_add, after = 110)
  }
  # write latex file to directory of local template
  write(lines, file = paste(subdir, "/_titlepage.tex", sep = ""))
}
