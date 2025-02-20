#' Add alternative text into latex
#'
#' @param x .tex file containing report. Typically produced after initially
#' rendering the skeleton made from create_template.
#' @param dir directory where the tex file is located that will be edited
#' @param rda_dir folder where rda files containing alternative text is located
#' @param compile Indicate whether the document (X) should be
#' rendered after these files are changed. Default TRUE.
#' @param rename Indicate a name for the new tex file produced from this
#' function. There is no need to include ".tex" in the name. Defaults to current
#' name and overwrites the current tex file.
#' @param alttext_csv_dir Directory for the csv file containing alternative
#' text and captions generated when running satf::exp_all_figs_tables
#'
#' @return This function was made to help add in
#' alternative text to latex documents generated from
#' quarto. Quarto does not currently contain a way to
#' add alternative text to PDF documents, so this function
#' was developed as a work around. The addition of alternative
#' text needs to be found in either the rda files produced from
#' satf::exp_all_figs_tables or in the captions_alt_text.csv also produced from
#' the same function. Users not using this format should create a csv file with
#' columns containing "label" and "alt_text".
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   create_template(
#'   new_template = TRUE,
#'   format = "pdf",
#'   office = "NWFSC",
#'   region = "U.S. West Coast",
#'   species = "Dover sole",
#'   spp_latin = "Microstomus pacificus",
#'   year = 2010,
#'   author = c("John Snow", "Danny Phantom", "Patrick Star"),
#'   include_affiliation = TRUE,
#'   convert_output = TRUE,
#'   resdir = "C:/Users/Documents/Example_Files",
#'   model_results = "Report.sso",
#'   model = "SS3",
#'   new_section = "an_additional_section",
#'   section_location = "after-introduction",
#'   rda_dir = getwd()
#'   )
#'
#'   path <- getwd()
#'
#'   quarto::quarto_render(file.path(path, "report", "SAR_USWC_Dover_sole_skeleton.qmd"))
#'
#'   withr::with_dir(
#'   file.path(path, "report"),
#'    add_alttext(
#'      x = "SAR_USWC_Dover_sole_skeleton.tex",
#'      dir = getwd(),
#'      alttext_csv_dir = getwd(),
#'      rda_dir = path,
#'      compile = TRUE,
#'      rename = "SAR_Dover_sole_tagged")
#'    )
#' }
#'
add_alttext <- function(
    x = list.files(getwd())[grep("skeleton.tex", list.files(getwd()))],
    dir = getwd(),
    rda_dir = getwd(),
    alttext_csv_dir = getwd(),
    compile = TRUE,
    rename = NULL
) {
  # Read latex file
  tex_file <- readLines(file.path(dir, x))

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

  # Check instance of pandocbounded that haven't been replaced
  # these are the additional images
  addl_figs <- grep("\\pandocbounded", tex_file)[-1]
  # ignore line 82 - this is the department of commerce logo
  # replace as above
  tex_file[addl_figs] <- lapply(
    tex_file[addl_figs],
    function(line) {
      gsub("\\pandocbounded", "\\pdftooltip", line)
    }
  )
  # Add alt text to custom images
  # read in alt text csv file to match with labels
  alttext <- utils::read.csv(file.path(alttext_csv_dir, "captions_alt_text.csv"))
  for (i in addl_figs) {
    # Find line label
    line <- tex_file[i]
    # Find line following target to extract label
    matches <- grep("\\label", tex_file)
    label_line <- matches[matches > i][1]
    line_label <- stringr::str_extract(tex_file[label_line], "\\\\label\\{([^}]*)\\}") |>
      stringr::str_remove_all("^\\\\label\\{|\\}$")
    # Match label name to label in csv and extract alttext
    alttext_i <- alttext |>
      dplyr::filter(label == line_label) |>
      dplyr::pull(alt_text)
    if (is.na(label_line)) {
      alttext_i <- ""
      warning(glue::glue(
        "No alternative text found for {line_label}."
      ))
    }
    # Add selected alttext onto end of the line
    tex_file[i] <- paste(tex_file[i], "{", alttext_i, "}", sep = "")
  }

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
  message("______Alternative text added to tex file.______")
  # Render the .tex file after edits
  if (compile) {
    message("______Compiling in progress - This can take a while...______")
    # test if this can be done when skeleton is in different folder than the wd
    tinytex::lualatex(file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)))
    message("______Compiling finished______")
  }
}
