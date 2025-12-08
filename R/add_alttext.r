#' Add alternative text into latex
#'
#' @inheritParams add_accessibility
#' @param tagged Indicate if the input tex file from dir has the latex package 
#' \tagpdf used so that tagging is present.
#'
#' @return This function was made to help add in
#' alternative text to latex documents generated from
#' quarto. Quarto does not currently contain a way to
#' add alternative text to PDF documents, so this function
#' was developed as a work around. The addition of alternative
#' text needs to be found in either the rda files produced from
#' stockplotr::exp_all_figs_tables or in the captions_alt_text.csv also produced from
#' the same function. Users not using this format should create a csv file with
#' columns containing "label" and "alt_text" where the label column contains the
#'  exact label name when referencing the image/figure in text. The label is
#'  very important as it provides a way for the function to match where the
#'  alternative text gets placed. When compile is set to TRUE, the alternative
#'  text using this format will not be available and must be used in conjunction
#'  with `asar::add_tagging()`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_template(
#'   new_template = TRUE,
#'   format = "pdf",
#'   office = "NWFSC",
#'   region = "U.S. West Coast",
#'   species = "Dover sole",
#'   spp_latin = "Microstomus pacificus",
#'   year = 2010,
#'   author = c("John Snow", "Danny Phantom", "Patrick Star"),
#'   model_results = "Report.sso",
#'   model = "SS3",
#'   new_section = "an_additional_section",
#'   section_location = "after-introduction",
#'   figures_dir = getwd()
#' )
#'
#' path <- getwd()
#'
#' quarto::quarto_render(file.path(path, "report", "SAR_USWC_Dover_sole_skeleton.qmd"))
#'
#' withr::with_dir(
#'   file.path(path, "report"),
#'   add_alttext(
#'     x = "SAR_USWC_Dover_sole_skeleton.tex",
#'     dir = getwd(),
#'     alttext_csv_dir = getwd(),
#'     figures_dir = path,
#'     compile = FALSE,
#'     rename = "SAR_Dover_sole_tagged"
#'   )
#' )
#' }
#'
add_alttext <- function(
    dir = file.path(getwd(), list.files(getwd())[grep("skeleton.tex", list.files(getwd()))]),
    figures_dir = getwd(),
    alttext_csv = getwd(),
    compile = TRUE,
    rename = NULL,
    tagged = TRUE) {
  # Read latex file
  if (!file.exists(dir)) cli::cli_abort("File {dir}/{x} does not exist!")
  tex_file <- readLines(dir)

  # Check if alt text csv is where indicated
  if (!file.exists(file.path(alttext_csv_dir, "captions_alt_text.csv"))) cli::cli_abort("'captions_alt_text.csv' not found in {alttext_csv_dir}.")

  # Identify lines with figures
  # # check if any lines have figures added
  # if (!any(grepl("fig-([a-z]+|[a-z]+_[a-z]+)-1.pdf", tex_file))) cli::cli_abort("No images/figures present in file.")
  # # this approach allows us to not mistake the replacement for other figures
  # # For render to pdf
  # fig_lines <- grep("fig-([a-z]+|[a-z]+_[a-z]+)-1.pdf", tex_file) # -plot
  # # Find images from previous naming conventions after quarto render
  # # TODO: this might need to be take out in the future - aka not needed
  # fig_lines <- c(fig_lines, grep("fig-([a-z]+|[a-z]+_[a-z]+)-plot-1.pdf", tex_file))
  # # for html render or external images
  # fig_lines <- c(
  #   fig_lines,
  #   grep("fig-([a-z]+|[a-z]+_[a-z]+)-1.png", tex_file)
  # )
  
  fig_lines <- grep("\\\\includegraphics", tex_file) # [-(1:3)]
  lines_to_remove <- c()
  # Remove lines from fig_lines that don't contain pandocbounded
  for (i in seq_along(fig_lines)) {
    # find line
    line <- tex_file[fig_lines[i]]
    # identify  if there is pandocbouded in line -- indicating it's a figure added not by the template
    if (grepl("pandocbounded", line)) next
    # Indicate which position(s) to remove from fig_lines
    lines_to_remove <- c(lines_to_remove, i)
  }
  # Remove lines that do not contain pandocbounded
  fig_lines <- fig_lines[-lines_to_remove]

  # TODO:
  # Create alternative options for render to html or docx
  
  # New method: find line with label from csv then back track to lines with pandoc_bounded
  # Add alt text to custom images
  # read in alt text csv file to match with labels
  alttext <- utils::read.csv(alttext_csv)
  for (i in 1:nrow(alttext)) {
    # Find line label
    label <- glue::glue("fig-{alttext$label[i]}")
    label_line_idx <- grep(
      paste0("\\\\caption\\{\\\\label\\{", label, "\\}"),
      tex_file
    )
    # Commenting out the below warning bc when you use {stockplotr}, it produces 
    # all figures alt text even ones not exported by {stockplotr} yet
    if (length(label_line_idx) == 0) next # {
      # stop(paste(
      #   'The following figure label does not appear in the document:',
      #   label
      # ))
    # }
    
    # Find which figure is right before this from fig_lines
    # only select the number closest to label_line_idx
    fig_line_idx <- max(fig_lines[fig_lines < label_line_idx])
    # Identify the line where the figure is names and loaded in
    # This is the line that will contain the alt text
    line <- tex_file[fig_line_idx]
    
    # Add selected alttext onto end of the line
    # tex_file[fig_line_idx] <- paste(
    #   line,
    #   "{",
    #   alttext$alt_text[i],
    #   "}",
    #   sep = ""
    # )
    
    # Add selected alt text after keepaspectratio
    tex_file[i] <- gsub(
      "keepaspectratio",
      paste0("keepaspectratio,alt={'", alttext$alt_text[i], "'}"),
      tex_file[i]
    )
    # tex_file[i] <- paste(tex_file[i], "{", alttext$alt_text[i], "}", sep = "")
  }
  
  # Replace pandocbounded with pdftooltip so alt text can be added
  # This only works when not tagging documents
  # This is an older method of applying alt text
  # tex_file[fig_lines] <- lapply(
  #   tex_file[fig_lines],
  #   function(line) {
  #     gsub("\\pandocbounded", "\\pdftooltip", line)
  #   }
  # )
  
  # Save overwrite tex file
  write(
    unlist(tex_file),
    file = file.path(
      dir,
      ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)
    )
  )
  # utils::capture.output(cat(tex_file), file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)), append = FALSE)
  message("______Alternative text added to tex file.______")
  # Render the .tex file after edits
  if (compile) {
    message("______Compiling in progress - This can take a while...______")
    withr::with_dir(
      # changes the working directory only for rendering the tex file
      dir,
      tinytex::lualatex(file.path(ifelse(
        !is.null(rename),
        glue::glue("{rename}.tex"),
        x
      )))
    )
    message("______Compiling finished______")
  }

  # # Add alt text to custom images
  # # read in alt text csv file to match with labels
  # alttext <- utils::read.csv(file.path(alttext_csv_dir, "captions_alt_text.csv"))
  # if (length(addl_figs) > 0) {
  #   for (i in addl_figs) {
  #     # Find line label
  #     line <- tex_file[i]
  #     # Find line following target to extract label
  #     matches <- grep("\\label", tex_file)
  #     label_line <- matches[matches > i][1]
  #     line_label <- stringr::str_extract(tex_file[label_line], "\\\\label\\{([^}]*)\\}") |>
  #       stringr::str_remove_all("^\\\\label\\{|\\}$")
  #     # Match label name to label in csv and extract alttext
  #     alttext_i <- alttext |>
  #       dplyr::filter(label == line_label) |>
  #       dplyr::pull(alt_text)
  #     if (is.na(label_line)) {
  #       alttext_i <- ""
  #       cli::cli_alert_warning("No alternative text found for {line_label}.")
  #     }
  #     # Add selected alttext onto end of the line
  #     tex_file[i] <- gsub(
  #       "keepaspectratio",
  #       paste0("keepaspectratio,alt={'", alttext_i, "'}"),
  #       tex_file[i]
  #     )
  #     # tex_file[i] <- paste(tex_file[i], "{", alttext_i, "}", sep = "")
  #   }
  # }
  # 
  # # Insert alt text for figures
  # # Call alt text in list with names
  # obj_files <- list.files(file.path(figures_dir, "figures"))
  # 
  # # read all files in obj_files and put into list
  # figures_list <- grep("figure", obj_files)
  # alt_text_list <- list()
  # for (i in figures_list) {
  #   load(file.path(figures_dir, "figures", obj_files[i]))
  #   # extract name to add into the list for placement
  #   rda_name <- stringr::str_replace(obj_files[i], "_figure.rda", "")
  #   # if name is >1 word then replace the _ with - to follow naming convention for
  #   # figures in tex file
  #   if (grepl("_", rda_name)) rda_name <- stringr::str_replace(rda_name, "_", "-")
  #   # convert to name in tex file to find where the line is located
  #   tex_name <- glue::glue("fig-{rda_name}-1.png") # replacing pdf - img ext are changed in next step
  #   # extract alt. text with figure
  #   alt_text <- rda$alt_text
  #   # names(alt_text) <- tex_name
  #   # place obj into list
  #   alt_text_list[[tex_name]] <- alt_text
  #   # call tex obj name using names()
  #   # call alt text using list[[i]]
  #   # remove rda file to declutter
  #   rm(rda)
  # }
  # 
  # # Convert all pdf images to png if render was to pdf
  # # extract all files from render folder
  # # TODO: add check if this has already been done then move to next img in the folder
  # img_path <- file.path(dir, gsub(".tex", "_files/figure-pdf", x))
  # if (dir.exists(img_path)) {
  #   imgs <- list.files(img_path)
  #   for (i in seq_along(imgs)) {
  #     img_file <- imgs[i]
  #     if (grepl(".png", img_file)) next
  #     img_file_con <- gsub(".pdf", ".png", img_file)
  #     if (!file.exists(file.path(img_path, img_file_con))) {
  #       pdftools::pdf_convert(
  #         file.path(img_path, img_file),
  #         format = "png",
  #         dpi = 300,
  #         filenames = file.path(img_path, img_file_con)
  #       ) |>
  #         suppressWarnings() |>
  #         suppressMessages()
  #     }
  #     # Replace names in the tex file
  #     tex_file <- gsub(img_file, img_file_con, tex_file)
  #   }
  # }
  # 
  # # Find where figure is located and append the alt. text
  # # TODO: make checks so only adds to images that don't already have alt text included in them
  # for (i in seq_along(alt_text_list)) {
  #   fig_line <- grep(names(alt_text_list[i]), tex_file)
  #   # Check that line we are adding the alt text to is for correct fig
  #   if (!grepl(names(alt_text_list[i]), tex_file[fig_line])) {
  #     cli::cli_alert_warning("Non-matching object name to tex file line.")
  #     next
  #   }
  #   # Check that selected tex_line contains a marked figure - aka correct placement
  #   file_name <- stringr::str_remove(x, ".tex")
  #   if (!grepl(glue::glue("{file_name}_files/figure-pdf/fig-"), tex_file[fig_line])) {
  #     cli::cli_alert_warning("Improper line for appendment.")
  #     cli::cli_alert_warning("Skipped adding alternative text for {names(alt_text_list[i])}", wrap = TRUE)
  #     next
  #   }
  #   tex_file[fig_line] <- gsub(
  #     "keepaspectratio",
  #     paste0("keepaspectratio,alt={'", alt_text_list[[i]], "'}"),
  #     tex_file[fig_line]
  #   )
  #   # tex_file[fig_line] <- paste(tex_file[fig_line], "{", alt_text_list[[i]], "}", sep = "")
  #   # tex_file[fig_line] <- strwrap(paste(tex_file[fig_line], "{", alt_text_list[[1]], "}", sep = ""))  # remove strwrap if does not render
  # }
  # 
  # # Checks
  # # add check if there are more plots that did not have alt text added
  # # if (length(obj_files) != length(fig_lines)) {
  # #   # Find which ones were not changed
  # #   # figs_miss_alt <-
  # #   warning("Missing alternative text for the followiing figures:")
  # # }
  # # TODO: test case where additional figure is added into the .tex file that is not included in the rda files
  # 
  # # Save overwrite tex file
  # write(unlist(tex_file), file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))
  # # utils::capture.output(cat(tex_file), file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)), append = FALSE)
  # cli::cli_alert_success("______Alternative text added to tex file.______")
  # # Render the .tex file after edits
  # if (compile) {
  #   # Find name for the previously rendered report
  #   prev_report_name <- list.files(dir, pattern = ".pdf")
  #   if (length(prev_report_name) > 1) {
  #     cli::cli_alert_info("______Multiple pdfs found in directory, defaulting to the tex file name______")
  #     pdf_report_name <- NULL
  #   }
  #   pdf_report_name <- ifelse(is.null(rename), prev_report_name, glue::glue("{rename}.pdf"))
  #   if (length(prev_report_name) == 0) {
  #     cli::cli_alert_info("______No previous report found, defaulting to the tex file name______")
  #     pdf_report_name <- NULL
  #   }
  #   # Render the report
  #   cli::cli_alert_info("______Compiling in progress - This can take a while...______")
  #   # test if this can be done when skeleton is in different folder than the wd
  #   tinytex::lualatex(
  #     file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)),
  #     pdf_file = pdf_report_name
  #   )
  #   cli::cli_alert_success("______Compiling finished______")
  # }
}
