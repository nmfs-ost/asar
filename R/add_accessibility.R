#' Add Accessibility to .tex documents
#'
#' Altering latex file of report to increase accessibility of the document.
#'
#' @param x .tex file to add accessibility into
#' @param dir directory where the tex file is located that will be edited
#' @param rda_dir folder where rda files containing alternative text is located
#' @param compile TRUE/FALSE - indicate whether the document (X) should be rendered after these files are changed
#' @param rename change the name of the latex file for final compilation or saving
#'
#' @return DRAFT: This function was made to help add in latex packages and content
#' associated with PDF tagging. Quarto does not allow the user to edit anything
#' before documentclass, so this function alters the rendered .tex file. Users
#' should either compile directly through the function or run
#' tinytex::lualatex(...) afterwards in the console.
#' @export
#'
add_accessibility <- function(
  x = list.files(getwd())[grep("skeleton.tex", list.files(getwd()))],
  dir = getwd(),
  rda_dir = getwd(),
  compile = TRUE,
  rename = NULL
  ) {
  # Read latex file
  tex_file <- readLines(file.path(dir, x))
  
  # Identify line where the new accessibility content should be added after
  # line_after <- grep("\\PassOptionsToPackage\\{dvipsnames\\,svgnames\\,x11names\\}\\{xcolor\\}", tex_file)
  # # Acessibility additions before /documentclass
  # line_to_add <- "\\input{accessibility.tex}"
  # # Add line into file
  # tex_file <- append(line_to_add, tex_file, after = line_after)
  # # DO NOT UNCOMMENT FOLLOWING LINES WHEN OPERATING FXN AS A WHOLE
  # # We want to keep tex_file open so we can make changes to figures later down the line
  # # Export file
  # # write(tex_file, file = paste(dir, x, sep = "/"))
  # 
  # # Add accessibility.tex to directory
  # accessibility <- paste0(
  #   "\\DocumentMetadata{%", "\n",
  #   "  ", "%  uncompress, %only for debugging!!", "\n",
  #   "  ", "pdfversion=2.0,", "\n",
  #   "  ", "testphase={phase-II, tabular, graphic}%", "\n",
  #   "  ", "% testphase={phase-II,math, tabular, graphic}% TOC Does not work", "\n",
  #   "  ", "% testphase={phase-III,math}% TOC works", "\n",
  #   "}", "\n",
  #   "\\tagpdfsetup{activate, tabsorder=structure}", "\n",
  #   "% Use the following to fix bug in November 2023 download of LaTeX", "\n",
  #   "\\ExplSyntaxOn", "\n",
  #   "\\cs_generate_variant:Nn__tag_prop_gput:Nnn{cnx}", "\n",
  #   "\\ExplSyntaxOff", "\n",
  #   "%", "\n"
  # )
  # Save accessibility partial
  # utils::capture.output(cat(acceesibility), file = file.path(dir, "accessibility.tex"), append = FALSE)

  # Check: count instances of pattern
  # sub_count <- length(
  #   grep("1", 
  #        stringr::str_count(
  #          tex_file, 
  #          pattern = stringr::coll("\\pandocbounded{\\includegraphics["))
  #        )
  #   )
  
  # Identify lines with figures
  # this approach allows us to not mistake the replacement for other figures
  fig_lines <- grep("fig-([a-z]+|[a-z]+_[a-z]+)-plot-1.pdf", tex_file)
  # TODO: 
  # create check to see if there are any instances where the suffix is not plot-1
  # Replace instances of macro in the tex file
  # replace_macro <- gsub(
  #   "\\pandocbounded",
  #   "\\pdftooltip",
  #   tex_file
  # )
  
  # Replace pandocbounded with pdftooltip so alt text can be added
  tex_file[fig_lines] <- lapply(
    tex_file[fig_lines],
    function(line) {
      gsub("\\pandocbounded", "\\pdftooltip", line)
    }
  )
  
  # Insert alt text for figures
  # Call alt text in list with names
  obj_files <- list.files(file.path(rda_dir, "rda_files"))

  # read all files in obj_files and put into list
  alt_text_list <- list()
  for (i in seq_along(obj_files)) {
    load(file.path(rda_dir, "rda_files", obj_files[i]))
    # extract name to add into the list for placement
    rda_name <- stringr::str_replace(obj_files[i], "_figure.rda", "")
    # if name is >1 word then replace the _ with - to follow naming convention for
    # figures in tex file
    if (grepl("_", rda_name)) rda_name <- stringr::str_replace(rda_name, "_", "-")
    # convert to name in tex file to find where the line is located
    tex_name <- glue::glue("fig-{rda_name}-plot-1.pdf")
    # extract alt. text with figure
    alt_text <- rda$alt_text
    # names(alt_text) <- tex_name
    # place obj into list
    alt_text_list[[tex_name]] <- alt_text
      # call tex obj name using names()
      # call alt text using list[[i]]
    # remove rda file to declutter
    rm(rda)
  }

  # Find where figure is located and append the alt. text
  for (i in seq_along(alt_text_list)) {
    fig_line <- grep(names(alt_text_list[i]), tex_file)
    # Check that line we are adding the alt text to is for correct fig
    if (!grepl(names(alt_text_list[i]), tex_file[fig_line])) {
      warning(glue::glue("Non-matching object name to tex file line."))
      next
    }
    # Check that selected tex_line contains a marked figure - aka correct placement
    file_name <- stringr::str_remove(x, ".tex")
    if (!grepl(glue::glue("{file_name}_files/figure-pdf/fig-"), tex_file[fig_line])) {
      warning(glue::glue("Improper line for appendment: \n Skipped adding alternative text for {names(alt_text_list[i])}"))
      next
    }
    tex_file[fig_line] <- paste(tex_file[fig_line], "{", alt_text_list[[i]], "}", sep = "")
    # tex_file[fig_line] <- strwrap(paste(tex_file[fig_line], "{", alt_text_list[[1]], "}", sep = ""))  # remove strwrap if does not render
  }
  # tex_file[430] <- paste(tex_file[fig_line], "{", alt_text_list[[1]], "}", sep = "")
  # strwrap(paste(tex_file[fig_line], "{", alt_text_list[[1]], "}", sep = ""), width = 80)

  # Checks
  # add check if there are more plots that did not have alt text added
  # if (length(obj_files) != length(fig_lines)) {
  #   # Find which ones were not changed
  #   # figs_miss_alt <- 
  #   warning("Missing alternative text for the followiing figures:")
  # }
  # TODO: test case where additional figure is added into the .tex file that is not included in the rda files

  # Save overwrite tex file
  write(unlist(tex_file), file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))
  # utils::capture.output(cat(tex_file), file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)), append = FALSE)
  # Render the .tex file after edits
  if (compile) {
    # test if this can be done when skeleton is in different folder than the wd
    tinytex::lualatex(file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))
  }
}
