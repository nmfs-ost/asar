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
                                 species = "") {
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

  # add soul package directly so error goes away
  lines <- append(
    lines,
    "% add soul package to remove latex error\n\\usepackage{soul}\n",
  )
  # Add in notation to add glossary
  # Notation permenantly added in the initial in-header.tex doc
  # gloss <- paste(
  #   "\\usepackage[acronym, nonumberlist]{glossaries}","\n",
  #   "\\makenoidxglossaries", "\n",
  #   "\\loadglsentries{report_glossary.tex}", "\n",
  #   sep = ""
  # )
  # lines <- append(lines, gloss)

  write(lines, file = paste(subdir, "/in-header.tex", sep = ""))
}

#------------------------------------------------------------------------------

# Check for tables and id number of headers for header tagging
id_num_headers <- function(tex_file) {
  # input = tex file (lines from add_tagging)
  # also add option provide it as a tex file? -- no bc it needs tagpdf
  # output = edited tex file (or lines)
  # recognize locations of table(s)
  # id how many header rows there are
  # add tagpdf commands accordingly
  table_lines <- grep("\\\\begin\\{table\\}|\\\\begin\\{longtable\\}", tex_file)
  
  if (length(table_lines) == 0) {
    return(tex_file)
  }
  
  for (i in rev(table_lines)) {
    # Find table chunk
    table_end <- grep("\\\\end\\{table\\}|\\\\end\\{longtable\\}", tex_file)[grep("\\\\end\\{table\\}|\\\\end\\{longtable\\}", tex_file) > i][1]
    table_chunk <- tex_file[i:table_end]
    # Find rows that indicate formatting before and after header(s)
    # this will not work for flextable
    # header_start <- grep("\\\\toprule", table_chunk) # this might not be tried and true
    # header_end <- grep("\\\\midrule", table_chunk)
    # if (length(header_start) == 0 | length(header_end) == 0) {
    #   # find first and second hline
    #   header_start <- grep("\\\\hline", table_chunk)[1]
    #   header_end <- grep("\\\\hline", table_chunk)[2]
    # }
    
    # number of header rows
    if (length(header_start) == 0 || length(header_end) == 0) {
      warning("Could not find \\toprule or \\midrule in table chunk; skipping header row tagging for this table.")
      next
    }
    # For now setting all number of headers to 1
    n_header_rows <- 1
    # n_header_rows <- header_end - header_start - 1
    # notation for header
    tag_header <- paste0("\\tagpdfsetup{table/header-rows={", n_header_rows, "}}")
    
    # update tex_file
    tex_file <- append(
      tex_file,
      tag_header,
      after = i - 1
    )
  }
  tex_file
}
