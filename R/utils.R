# Credit for following utils belongs to `claudiozandonella/trackdown`
# More information can be found at https://claudiozandonella.github.io/trackdown/
#
# These were added in association with adjustments to the function `upload_file()`
# in the package `trackdown`. Adjustments were made to ignore authorization since
# this tool's workflow uses googledrive::drive_auth which is part of tidyverse.
# This works in cases where 'trackdown' is blocked by google for your organization.
# It is also adjusted to apply over multiple files rather than just one.

#----    evaluate_file    ----
#' Evaluate File local and Google Drive Information
#'
#' @param file character indicating the path to the local file (or output)
#' @param gfile character indicating the name of a Google Drive file
#' @param gpath character indicating the path in Google Drive
#' @param shared_drive character. The name of a Google Drive shared drive
#'   (optional).
#' @param test character indicating whether to test no line in dribble ("none"),
#'   single line in dribble ("single") or both condition accepted ("both")
#'
#' @return a list with relevant information
#'  - file - character indicating the path to the local file (or output)
#'  - file_info - list with file info returned from  get_file_info()
#'    function
#'  - gfile - character indicating the corrected gfile naem for the file
#'  - dribble_info - list with dribble info of the file and parent
#'    returned by get_dribble_info() function
#'
#' @noRd
#'
#' @examples
#' # file
#' file <- "tests/testthat/test_files/examples/example-1.Rmd"
#'
#' # output
#' file <- "tests/testthat/test_files/examples/example-1.pdf"
#'
#' evaluate_file(file)
#'
evaluate_file <- function(file,
                          gfile = NULL,
                          gpath = "trackdown",
                          shared_drive = NULL,
                          test = c("none", "single", "both")){

  test <-  match.arg(test)

  # check local file exists and get file info
  # check_file(file)
  file_info <- get_file_info(file = file)

  # set correct gfile
  gfile <- ifelse(is.null(gfile), yes = file_info$file_basename, no = gfile)

  # get dribble info
  dribble_info <- get_dribble_info(gfile = gfile,
                                   path = gpath,
                                   shared_drive = shared_drive)

  # check there is no file (or a single file) with same name in drive
  # check_dribble(dribble_info$file, gfile, test = test)

  return(list(file = file,
              file_info = file_info,
              gfile = gfile,
              dribble_info = dribble_info))
}

#----    get_file_info    ----

#' Get file info
#'
#' Given the path to a file, get file information about path, file-name, file
#' extension, file-basename.
#'
#' @param file a string indicating the path to a file
#'
#' @return a list with
#' - path: the path to the file. If there is no path `"."` is
#'   returned
#' - file_name: file name with extension
#' - extension: the file extension without point and all lowercase
#'  - file_basename: the file name without extension
#'
#' @noRd
#'
#' @examples
#' get_file_info("my_folder/my_file.txt")
#' get_file_info("my.file.txt")
#'
get_file_info <- function(file){
  # check file is a single string
  if(!(is.character(file) && length(file) == 1L))
    stop("file has to be a single string")

  # get info
  path <- dirname(file)
  file_name <- basename(file)

  # ensure there is extension
  if(!grepl(pattern = "\\.", file_name))
    stop("file do not include extension")

  # get extension as last element split "."
  extension <- strsplit(file_name, split = "\\.")[[1]]
  extension <- tail(extension, n = 1)

  file_basename <- sub(pattern = paste0("\\.", extension), replacement = "",
                       file_name)

  return(list(path = path,
              file_name = file_name,
              extension = tolower(extension), # get lowercase
              file_basename = file_basename))
}

#----    upload_document    ----

#' Upload (or Update) a Document in Google Drive
#'
#' Internal function to upload (or update) a local file to Google Drive as a
#' plain text document. Local file information and Google Drive document
#' information and have to be provided. Option `hide_code` allows to
#' remove code chunks from the text document and option `update`
#' indicates whether to update file in Google Drive.
#'
#' @param file character. The path (without file extension) of a local `.Rmd`
#'   file.
#' @param file_info list with file info returned from get_file_info() function
#' @param gfile character. The name of a Google Drive file (defaults to local
#'   file name).
#' @param gpath character indicating the (sub)directory in Google Drive.
#' @param dribble_document A list with two dribble object regarding the gfile
#'   and the parent item.
#' @param hide_code logical value indicating whether to remove code from the
#'   text document (chunks and header). Placeholders of  type `"[[chunk-<name>]]"`
#'   are displayed instead.
#' @param update logical value indicating whether to update or upload the
#'   document.
#' @param rich_text (experimental) logical value (default is `TRUE`)
#'   indicating whether to upload to Google Docs a rich document (i.e.,
#'   important text that should not be changed is highlighted).
#' @param rich_text_par (experimental) argument used to pass a list with custom
#'   settings for rich_text.
#'
#' @return a dribble of the uploaded (or updated) document
#' @noRd
#'
#' @examples
#' file <- "tests/testthat/test_files/examples/example-1.Rmd"
#' file_info <- get_file_info(file)
#' gfile <- "example-1"
#' dribble_document <- get_dribble_info(gfile = gfile, path = "unit_tests/examples")
#' hide_code <- FALSE
#' upload_document(file, file_info, gfile, gpath = "trackdown/examples",
#'                 dribble_document, hide_code, update = TRUE)
#'
upload_document <- function(file, file_info,
                            gfile, gpath, dribble_document,
                            hide_code, rich_text = TRUE, rich_text_par = NULL,
                            update = FALSE){
  #---- temp file ----
  # create .temp-file to upload
  temp_file <- file.path(file_info$path,
                         paste0(".temp-", file_info$file_basename, ".txt"))
  file.copy(file, temp_file, overwrite = TRUE)

  # remove temp-file on exit
  on.exit(invisible(file.remove(temp_file)), add = TRUE)

  # read document lines
  document <-  readLines(temp_file, warn = FALSE)


  #---- hide code ----
  # if(isTRUE(hide_code)){
  #   start_process("Removing code...")
  #   document <- hide_code(document = document,
  #                         file_info = file_info)
  #   finish_process(paste("Code removed from", emph_file(file_info$file_name)))
  # }


  #---- upload document ----

  googledrive::local_drive_quiet() # suppress messages from googledrive

  # Format document to a single string
  document_oneline <- format_document(document,
                                      file_info = file_info,
                                      hide_code = hide_code)
  cat(document_oneline, file = temp_file)


  if(isTRUE(update)){
    start_process("Updating document with local changes to Google Drive...")

    # Update document
    res <- googledrive::drive_update(
      media = temp_file,
      file = dribble_document$file)

    finish_process(paste("Document updated at",
                         cli::col_blue(paste(gpath, gfile, sep = "/"))))
  } else {
    start_process("Uploading document to Google Drive...")

    # Upload document
    res <- googledrive::drive_upload(
      media = temp_file,
      path = dribble_document$parent,
      name = gfile,
      type = "document")

    finish_process(paste("Document uploaded at",
                         cli::col_blue(paste(gpath, gfile, sep = "/"))))
  }

  #----    rich_text    ----

  # if(isTRUE(rich_text)){
  #   run_rich_text(text = document_oneline,
  #                 document_ID = res$id,
  #                 extension = file_info$extension,
  #                 rich_text_par = rich_text_par)
  #
  #   finish_process("Rich text requests completed")
  # }

  return(res)
}

#----    format_document    ----

#' Format the document as a single string
#'
#' @param document a vector with the content of the document
#' @param file_info list with file info returned from get_file_info() function
#' @param hide_code logical value indicating whether the code was from the
#'   text document
#'
#' @return a string with the content of the document
#' @noRd
#'
#' @examples
#'   document <- readLines("tests/testthat/test_files/examples/example-1.Rmd")
#'   file_info <- get_file_info("tests/testthat/test_files/examples/example-1.Rmd")
#'   format_document(document, file_info = file_info, hide_code = FALSE)
#'
format_document <- function(document, file_info, hide_code){

  # Add instructions
  document <- c(get_instructions(file_info = file_info,
                                 hide_code = hide_code),
                document,
                "") #Adds a final EOL

  # Format in a single line
  res <- paste(document, collapse = "\n")

  return(res)
}

#----    get_instructions    ----

#' Add Instructions
#'
#' Add instruction on top of document to explain reviewdown
#'
#' @param file_info list with file info returned from get_file_info() function
#' @param hide_code logical value indicating whether the code was from the
#'   text document
#'
#' @return a string with the instructions
#' @noRd
#'
#' @examples
#'   file_info <- get_file_info("tests/testthat/test_files/examples/example-1.Rmd")
#'   get_instructions(file_info, TRUE)
#'
get_instructions <- function(file_info, hide_code){

  language <- switch(file_info$extension,
                     "rmd" = "Markdown",
                     'qmd' = 'Quarto',
                     "rnw" = "LaTeX")


  placeholder1 <- switch(hide_code,
                         "TRUE" = 'Please do not remove placeholders of type "[[chunk-<name>]]" or "[[document-header]]"',
                         "FALSE" = NULL)
  placeholder2 <- c(sprintf("FILE-NAME: %s",file_info$file_name),
                    sprintf("HIDE-CODE: %s", hide_code),
                    sprintf("DATE-UPLOAD: %s", Sys.Date()))

  instructions <- c(
    "#----Trackdown Instructions----#",
    sprintf("This is not a common Document. The Document includes properly formatted %s syntax and R code. Please be aware and responsible in making corrections as you could break the code. Limit changes to narrative text and avoid modifying R code.",
            language),
    placeholder1,
    "Once the review is over accept all changes: Tools -> Review suggested edits -> Accept all.",
    "You must not modify or remove these lines, we will do it for you ;)",
    placeholder2,
    "#----End Instructions----#",
    "\n")

  return(instructions)
}

#----    remove_google_comments    ----

#' Remove Google Comments
#'
#' Remove Google comments from the downloaded file. Comments are all listed at
#' the bottom of the file. They are of type "[a]my comment" and the tag is also
#' present in the text. Tags have character indexes.
#'
#' @param document document character vector with the lines of the document
#'
#' @return document document character vector with the lines of the document
#' @noRd
#'
#' @examples
#' document <- readLines("tests/testthat/test_files/examples/Comments-restore.Rmd")
#' remove_google_comments(document)
#'

remove_google_comments <- function(document){

  # Identify comments
  line_comments <- grep("^\\[[a-z]+\\]", x = document, perl = TRUE)

  # If the last line is not a comment return
  if(!(length(document)) %in% line_comments){
    start_process("No Google comments found")
    return(document)
  }


  # Real comments are all at the bottom of the file. Identify them according to
  # index difference, is the last sequence of 1
  seq_comments <- rle(diff(line_comments))
  # Identify number of comments
  if(tail(seq_comments$values,1) == 1L){
    n_comments <- tail(seq_comments$lengths,1) + 1 # correct number of elements
  } else {
    n_comments <- 1 # only one comment
  }

  # Get correct comments sequence
  line_comments <- tail(line_comments, n_comments)

  # Save which comment have issues
  issue_comment <- data.frame(index = vector("character"),
                              line = vector("numeric"))

  for(i in line_comments){
    # Extract comment index from comment line
    comment_index <- gsub("^\\[([a-z]+)\\].*", replacement = "\\1", document[i], perl = TRUE)
    pattern <- paste0("\\[",comment_index,"\\]")

    # Find comment in the text
    line_with_comment <- grep(pattern, document[-i], perl = TRUE)
    text_comment <- document[line_with_comment[1]]

    # Check there is only one pattern occurency in the text
    n_patterns <- length(regmatches(text_comment, gregexpr(pattern, text_comment))[[1]])

    if(length(line_with_comment) == 1L && n_patterns == 1L){
      document[line_with_comment[1]] <- gsub(pattern, replacement = "", text_comment)
    } else{
      issue_comment <- rbind(issue_comment, data.frame(index = comment_index,
                                                       line = i))
    }
  }

  if(nrow(issue_comment)>0){
    start_process(paste("Issue in identifying Google comment(s):",
                        paste0("[", issue_comment$index, "]", collapse = " ")))
  }

  # Remove comment from the bottom of the file
  lines_to_remove <- setdiff(line_comments, issue_comment$line)
  if(length(lines_to_remove)>0){
    document <- document[-lines_to_remove]
  }

  return(document)
}

#----    eval_instructions    ----

#' Evaluate Docuemnt Instructions
#'
#' Given the document (vector with the text lines) retrieve instructions indexes
#' and the FILE-NAME and HIDE-CODE options
#'
#' @param document character vector with the lines of the document
#'
#' @return a list with:
#' - instruction_start - integer inidicating the instructions initial line
#' - instruction_end - integer inidicating the instructions end line
#' - file_name - character indicating the file name
#' - hide_code - logical indicating whether code was removed
#'
#' @noRd
#'
#' @examples
#'
#' document <- readLines("tests/testthat/test_files/examples/example-1-restore.Rmd", warn = FALSE)
#' eval_instructions(document)
#'
#' # no instructions delimiters
#' eval_instructions(document[-1])
#'
#' # no file_name
#' eval_instructions(document[-6])
#'
#' # no hide_code
#' eval_instructions(document[-7])
#'

eval_instructions <- function(document, file_name = NULL){

  # get instruction lines
  instruction_start <- which(grepl("#----Trackdown Instructions----#", document))
  instruction_end <- which(grepl("#----End Instructions----#", document))

  # test retrieve instructions
  my_test <- length(c(instruction_start, instruction_end))
  if (my_test!= 2L){
    warning("Failed retrieving instructions delimiters. ",
            "Intructions delimiters at the beginning shuld not be removed.", call. = FALSE)
    instruction <- document # search options in the whole document
    instruction_start <- NULL
    instruction_end <- NULL
  } else {
    instruction <- document[instruction_start:instruction_end]
  }


  # get file-name, hide-code, and date-upload options lines
  line_file_name <- which(grepl("^FILE-NAME:", instruction))
  line_hide_code <- which(grepl("^HIDE-CODE: ", instruction))
  line_date_upload <- which(grepl("^DATE-UPLOAD: ", instructions))

  # test retrieve FILE-NAME
  if (length(line_file_name)!= 1L){
    warning("Failed retrieving FILE-NAME, current file name is used instead.", call. = FALSE)
    old_file_name <- file_name
  } else {
    old_file_name <- gsub("^FILE-NAME:\\s*(.*)\\s*","\\1", instruction[line_file_name])
  }

  # test retrieve HIDE-CODE
  if (length(line_hide_code)!= 1L){
    warning("Failed retrieving HIDE-CODE. Considering presence of code tags instead.", call. = FALSE)
    hide_code <- any(grepl("^\\[\\[(document-header|chunk-.*)\\]\\]", document))
  } else {
    hide_code <- as.logical(gsub("^HIDE-CODE:\\s*(.*)\\s*","\\1", instruction[line_hide_code]))
  }

  # test retrieve DATE-UPDATE
  if (length(line_date_upload)!= 1L){
    warning("Failed retrieving DATE-UPLOAD. Considering presence of code tags instead.", call. = FALSE)
    date_upload <- any(grepl("^\\[\\[(document-header|chunk-.*)\\]\\]", document))
  } else {
    date_upload <- as.logical(gsub("^DATE-UPLOAD:\\s*(.*)\\s*","\\1", instruction[line_date_upload]))
  }

  res <- list(start = instruction_start,
              end = instruction_end,
              file_name = old_file_name,
              hide_code = hide_code,
              date_upload = date_upload)

  return(res)
}

#----    check_supported_documents    ----

#' Check Supported Documents
#'
#' Only .Rmd, .Qmd, and .Rnw files are supported
#'
#' @param file_info list with file info returned from get_file_info() function
#'
#' @return NULL
#' @noRd
#'
#' @examples
#' file_info <- get_file_info("my-report.txt")
#' check_supported_documents(file_info)
#'

check_supported_documents <- function(file_info){
  if(!(file_info$extension %in% c("rmd", "rnw", "qmd"))) # check supported files
    stop(paste(file_info$file_name, "not supported file type (only .Rmd, .Qmd, or .Rnw)"),
         call. = FALSE)
}

#----    get_os    ----

#' Get the current operating system
#'
#' @return the current os as string
#' @noRd
#'
#' @examples
#' os <- get_os()
#'

get_os <- function(){
  return(.Platform$OS.type)
}

#----    Messages Utils    ----

# start_process
start_process <- function(message){
  cli::cat_bullet(bullet_col = "#8E8E8E", message)
}

# finish_process
finish_process <- function(message){
  cli::cat_bullet(bullet = "tick", bullet_col = "green", message)
}

# emph_file
emph_file <- function(file){
  cli::col_blue(basename(file))
}

# main_process
main_process <- function(message){
  cat(cli::cat_rule(message), "\n")
}
