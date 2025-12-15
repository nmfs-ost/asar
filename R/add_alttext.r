#' Add alternative text into latex
#'
#' @inheritParams add_accessibility
#' @param tagged Indicate if the input tex file from dir has the latex package, 
#' tagpdf, used so that tagging is present.
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
#'  with `asar::add_tagging()` unless tagged is set to FALSE. Default is TRUE.
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
#'     alttext_csv = getwd(),
#'     compile = FALSE,
#'     rename = "SAR_Dover_sole_tagged"
#'   )
#' )
#' }
#'
add_alttext <- function(
    x = list.files(getwd())[grep("skeleton.tex", list.files(getwd()))],
    dir = getwd(),
    alttext_csv = file.path(getwd(), "captions_alt_text.csv"),
    compile = TRUE,
    rename = NULL,
    tagged = TRUE) {
  # Read latex file
  if (!file.exists(file.path(dir, x))) cli::cli_abort("File {dir}/{x} does not exist!")
  tex_file <- readLines(file.path(dir, x))

  # Check if alt text csv is where indicated
  if (!file.exists(alttext_csv)) cli::cli_abort("'captions_alt_text.csv' not found in {alttext_csv}.")

  fig_lines <- grep("\\\\includegraphics", tex_file) # [-(1:3)]
  lines_to_remove <- c()
  # Remove lines from fig_lines that don't contain pandocbounded
  for (i in seq_along(fig_lines)) {
    # find line
    line <- tex_file[fig_lines[i]]
    # identify  if there is pandocbounded in line -- indicating it's a figure added not by the template
    # added in pdftooltip as well if previous runs occurred
    if (grepl("pandocbounded|pdftooltip", line)) next
    # Indicate which position(s) to remove from fig_lines
    lines_to_remove <- c(lines_to_remove, i)
  }
  # Remove lines that do not contain pandocbounded
  if (length(lines_to_remove) > 0) {
    fig_lines <- fig_lines[-lines_to_remove]
  }
  
  # read in alt text csv file to match with labels
  alttext <- utils::read.csv(alttext_csv)

  # TODO:
  # Create alternative options for render to html or docx

  # New method: find line with label from csv then back track to lines with pandoc_bounded
  # Add alt text to custom images
  for (i in 1:nrow(alttext)) {
    # Find line label
    label <- glue::glue("fig-{alttext$label[i]}")
    label_line_idx <- grep(
      paste0("\\\\caption\\{\\\\label\\{", label, "\\}"),
      tex_file
    )
    # Skip to next row if label is not in the doc
    if (length(label_line_idx) == 0) next
    
    # Find which figure is right before this from fig_lines
    # only select the number closest to label_line_idx
    fig_line_idx <- max(fig_lines[fig_lines < label_line_idx])
    # Identify the line where the figure is named and loaded in
    # This is the line that will contain the alt text
    line <- tex_file[fig_line_idx]
    
    # determine which method alttext should be added
    if (tagged) {
      # check if alt text was previously added
      if (grepl("alt=", tex_file[fig_line_idx])) next
      # check if tooltip method for alt text was already added and replace
      if (grepl("pdftooltip", tex_file[fig_line_idx])) {
        tex_file[fig_line_idx] <- gsub("pdftooltip", "pandocbounded", tex_file[fig_line_idx])
        # remove the alt text at the end of the line
        tex_file[fig_line_idx] <- gsub(
          "\\{[^{}]*\\}$",
          "",
          tex_file[fig_line_idx]
        )
      }
      # Add selected alt text after keepaspectratio
      tex_file[fig_line_idx] <- gsub(
        "keepaspectratio",
        paste0("keepaspectratio,alt={'", alttext$alt_text[i], "'}"),
        tex_file[fig_line_idx]
      )
    } else {
      # Add selected alttext onto end of the line
      tex_file[fig_line_idx] <- paste(
        line,
        "{",
        alttext$alt_text[i],
        "}",
        sep = ""
      )
      # Replace pandocbounded with pdftooltip so alt text can be added
      # This only works when not tagging documents
      # This is an older method of applying alt text
      tex_file[fig_lines] <- lapply(
        tex_file[fig_lines],
        function(line) {
          gsub("\\pandocbounded", "\\pdftooltip", line)
        }
      )
    }
  }
  
  # Commenting this out for now -- without this part of the function the alt text
  # can not be visually seen when hovering the mouse over the image
  # HOWEVER; the alt text is embedded when document is tagged. You can check by
  # going to the figure tag, right click > properties and you should see the alt
  # text box filled in
  
  # if (tagged) {
  #   # Convert pdf images to png and replace file type in tex so alt text appears in the document
  #   # Use this method if you want to physically see the alt text (otherwise it's embedded)
  #   pdf_figures <- fig_lines[grep(".pdf", tex_file[fig_lines])]
  #   for (j in pdf_figures) {
  #     # ID image to replace
  #     img_path <- file.path(dir, stringr::str_match(tex_file[j], "\\\\includegraphics\\[.*?\\]\\{(.*?)\\}")[1, 2])
  #     # replace file extension from pdf to png
  #     tex_file[j] <- gsub("\\.pdf", ".png", tex_file[j])
  #     # convert and save image to png
  #     pdftools::pdf_convert(
  #       img_path,
  #       format = "png",
  #       dpi = 300,
  #       filenames = gsub("\\.pdf", ".png", normalizePath(img_path, mustWork = FALSE))
  #                        # fun tip: you can use the function basename() to pull out the file name from a path
  #     ) |>
  #       suppressWarnings() |>
  #       suppressMessages()
  #   }
  # }
  
  # Save overwrite tex file
  write(
    unlist(tex_file),
    file = file.path(
      dir,
      ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)
    )
  )
  # utils::capture.output(cat(tex_file), file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)), append = FALSE)
  cli::cli_alert_success("______Alternative text added to tex file.______")
  # Render the .tex file after edits
  if (compile) {
    cli::cli_alert_info("______Compiling in progress - This can take a while...______")
    withr::with_dir(
      # changes the working directory only for rendering the tex file
      dir,
      tinytex::lualatex(file.path(ifelse(
        !is.null(rename),
        glue::glue("{rename}.tex"),
        x
      )))
    )
    cli::cli_alert_success("______Compiling finished______")
  }
}
