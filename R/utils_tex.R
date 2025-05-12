#############################################
####    Dynamically Change Formatting    ####
#############################################

#' Create a title page latex document
#'
#' @param office primary science center writing the document
#' @param subdir directory where files are going to be held
#' @param species target species for assessment
#'
#' @return Create a _titlepage.tex document that contains formatting options for
#'  a cover page. The only thing that changes currently is the primary author's
#'  fishery science center.
#' @export
create_titlepage_tex <- function(office = "",
                                 subdir,
                                 species) {
  # Read basic latex file
  lines <- readLines(
    system.file("resources", "formatting_files", "_titlepage.tex", package = "asar")
  )

  # Add alt text to cover page image
  line_before <- grep("\\$if\\(cover)\\$", lines)
  cp_alt <- paste(
    "\\pdftooltip{\\includegraphics[width=6in]{$cover$}}{",
    "An illustration of ", species,
    "}",
    sep = ""
  )
  lines <- append(lines, cp_alt, after = line_before)

  # Add office to bottom ref of title page
  if (office == "NEFSC") {
    center <- "Northeast Fisheries Science Center"
  } else if (office == "NWFSC") {
    center <- "Northwest Fisheries Science Center"
  } else if (office == "SEFSC") {
    center <- "Southeast Fisheries Science Center"
  } else if (office == "SWFSC") {
    center <- "Southwest Fisheries Science Center"
  } else if (office == "AFSC") {
    center <- "Alaska Fisheries Science Center"
  } else if (office == "PIFSC") {
    center <- "Pacific Islands Fisheries Science Center"
  }
  if (office != "") {
    to_add <- paste(center, "\\newline", sep = "") # unlist(rlang::dots_list(...))
    line_before <- grep("National Marine Fisheries Service", lines)
    lines <- append(lines, to_add, after = line_before)
  }
  # write latex file to directory of local template
  write(lines, file = paste(subdir, "/_titlepage.tex", sep = ""))
}

#------------------------------------------------------------------------------

#' Create in-header latex document
#'
#' @param species common species name - used for footer
#' @param year year assessment is conducted
#' @param subdir directory where other files will be copied into
#'
#' @return Create an in-header latex document that dynamically changes based on
#' the species and year along with other factors.
#' @export
create_inheader_tex <- function(species = NULL, year = NULL, subdir) {
  if (is.null(year)) {
    year <- format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y")
  }
  lines <- readLines(
    system.file("resources", "formatting_files", "in-header.tex", package = "asar")
  )
  if (is.null(species)) {
    to_add <- paste(
      "\\usepackage[headsepline=0.005pt:,footsepline=0.005pt:,plainfootsepline,automark]{scrlayer-scrpage}", "\n",
      "\\clearpairofpagestyles", "\n",
      "\\ohead[]{\\headmark} \\cofoot[\\pagemark]{\\pagemark}", "\n",
      # "\\lohead{", species," assessment ", year,"}","\n",
      "\\ModifyLayer[addvoffset=-.6ex]{scrheadings.foot.above.line}", "\n",
      "\\ModifyLayer[addvoffset=-.6ex]{plain.scrheadings.foot.above.line}", "\n",
      "\\setkomafont{pageheadfoot}{\\small}", "\n",
      sep = ""
    )
    lines <- append(lines, to_add)
  } else {
    to_add <- paste(
      "\\usepackage[headsepline=0.005pt:,footsepline=0.005pt:,plainfootsepline,automark]{scrlayer-scrpage}", "\n",
      "\\clearpairofpagestyles", "\n",
      "\\ohead[]{\\headmark} \\cofoot[\\pagemark]{\\pagemark}", "\n",
      "\\lohead{", species, " assessment ", year, "}", "\n",
      "\\ModifyLayer[addvoffset=-.6ex]{scrheadings.foot.above.line}", "\n",
      "\\ModifyLayer[addvoffset=-.6ex]{plain.scrheadings.foot.above.line}", "\n",
      "\\setkomafont{pageheadfoot}{\\small}", "\n",
      sep = ""
    )
    lines <- append(lines, to_add)
  }
  write(lines, file = paste(subdir, "/in-header.tex", sep = ""))
}

#------------------------------------------------------------------------------
