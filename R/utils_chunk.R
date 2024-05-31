#----    restore_file    ----

#' Restore the Downloaded File with Code Info
#'
#' Restore placeholders of type `"[[chunk-<name>]]"`/`"[[Document-header]]"` with
#' the actual code and sanitize file.
#'
#' @param temp_file character indicating the path to the downloaded file
#' @param file_name character indicating the current file name
#' @param path character indicating the folder of the original file
#' @param gpath character indicating the google drive folder for the file that is being restored
#' @param rm_gcomments (experimental) logical value indicating whether or not to
#'   remove Google comments
#'
#' @return a single string with the content of the document
#' @noRd
#'

restore_file <- function(temp_file, file_name, path, gpath, rm_gcomments = FALSE){

  # read document lines
  document <- readLines(temp_file, warn = FALSE)

  # remove Google comments
  if(isTRUE(rm_gcomments)){
    document <- remove_google_comments(document)
  }

  # eval instructions
  instructions <- eval_instructions(document = document, file_name = file_name)

  # # extract upload date
  # upload_date <- instructions$date_upload
  #
  # # find date last edited
  # gpath_dribble <- get_path_dribble(gpath)$id
  # modified_dates_all <- googledrive::drive_ls(gpath_dribble) |>
  #   dplyr::mutate(modified = purrr::map_chr(drive_resource, "modifiedTime")) |>
  #   dplyr::filter(name == gsub(".qmd", "", file_name))
  # modified_date <- gsubfn::strapplyc(modified_dates_all$modified, "\\d+-\\d+-\\d+", simplify = TRUE)
  #
  # # if date uploaded and date edited are equal - return warning and do not proceed? - otherwise continue?
  # if (isTRUE(upload_date==modified_date)){
  #   res <- "no_updates"
  #   return(res)
  # } else {
  #   # remove instructions if indexes are available
  #   if(!is.null(instructions$start) & !is.null(instructions$end)){
  #     document <- document[-c(instructions$start:instructions$end)]
  #   }

  if(!is.null(instructions$start) & !is.null(instructions$end)){
    document <- document[-c(instructions$start:instructions$end)]
  }

  # restore code
  if(isTRUE(instructions$hide_code)){
    document <- restore_code(document = document,
                             file_name = instructions$file_name,
                             path = path)
  }

    # sanitize document
    document <- sanitize_document(document)

    cat(document, file = temp_file)

    return(invisible(document))
  # }
}

#----    restore_code    ----

#' Restore Header and Chunks Code
#'
#' Given the document, the co
#'
#' @param document character vector with the lines of the document
#' @param file_name character indicating the name of the file used to load code
#'   info
#' @param path character indicating the path to the file
#'
#' @return character vector with the lines of the document
#' @noRd
#'
#' @examples
#' # Rmd
#' file_name <- "example_1.Rmd"
#'
#' # Rnw
#' file_name <- "example_1.Rnw"
#'
#' path <- "tests/testthat/test_files/examples"
#' document <- readLines(file.path(path, paste0("restore_", file_name)), warn = FALSE)
#' restore_code(document, file_name, path)
#'

restore_code <- function(document, file_name, path){

  # Check .trackdown folder is available
  if(!dir.exists(file.path(path, ".trackdown")))
    stop(paste0("Failed restoring code. Folder .trackdown is not available in ", path), call. = FALSE)

  # load code info
  header_info <- load_code(file_name = file_name, path = path, type = "header")
  chunk_info <- load_code(file_name = file_name, path = path, type = "chunk")

  #---- restore header ----

  if(is.null(header_info)){
    # Skip and set index_header to allow document[seq_len(index_header)]
    # in restore_chunk to return nothing
    index_header <- 0L
  } else {
    # Find "[[document-header]]" tag
    index_header <- grep("^\\[\\[document-header\\]\\]", document)

    if(length(index_header) != 1L) {
      warning("Failed retrieving [[document-header]] placeholder, code added at first line", call. = FALSE)
      document <- c(header_info$header_text, document)
      index_header <- 1L
    } else {
      document[index_header] <- header_info$header_text
    }
  }

  #---- restore chunks ----

  if(!is.null(chunk_info)){
    document <- restore_chunk(document = document,
                              chunk_info = chunk_info,
                              index_header = index_header)
  }

  return(document)

}

#----    restore_chunk    ----

#' Restore Chunk Code
#'
#' Given the document, chunk info and header line index, restore chunks in the
#' document. Allow to fix possible missing chunk-tags in the document by adding
#' them right after the previous matching chunk. In case the first one is
#' missing, chunk is added after the header. Note that actual chunks restoring
#' process starts from the end going backwards.
#'
#' @param document character vector with the lines of the document
#' @param chunk_info dataframe with the chunk information to restore previously
#'   saved
#' @param index_header integer indicating the line index of th header
#'
#' @return character vector with the lines of the document
#' @noRd
#'
#' @examples
#'
#' # Rmd
#' file <- "tests/testthat/test_files/examples/example-1-restore.Rmd"
#' chunk_info <- load_code("example-1.Rmd", path = "tests/testthat/test_files/examples",
#'                           type = "chunk")
#' index_header <- 9
#'
#' # Rnw
#' file <- "tests/testthat/test_files/examples/example-1-restore.Rnw"
#' chunk_info <- load_code("example-1.Rnw", path = "tests/testthat/test_files/examples",
#'                           type = "chunk")
#' index_header <- 12
#'
#' document <- readLines(file, warn = FALSE)
#' restore_chunk(document, chunk_info, index_header)
#'

restore_chunk <- function(document, chunk_info, index_header){

  index_chunks <- grep("^\\[\\[chunk-.+\\]\\]", document)
  # extract names [[chunk-*]] removing possible spaces
  names_chunks <- gsub("^\\s*(\\[\\[chunk-.+\\]\\])\\s*","\\1", document[index_chunks])

  match <- chunk_info$name_tag %in% names_chunks


  my_seq <- rev(seq_len(nrow(chunk_info))) # revers order start form last chunk
  unmatched <- NULL
  for (i in my_seq){
    if(isFALSE(match[i])){
      unmatched <- c(chunk_info$chunk_text[i], unmatched)

      # test if is the last remaining chunk
      if(i == 1L){
        document <- c(document[seq_len(index_header)],  # if no header index_header is 0
                      paste0(unmatched, collapse = "\n\n"),
                      document[(index_header + 1):length(document)])
        unmatched <- NULL
      }

    } else {
      # get correct index_chunk matching names in document
      line_index <- index_chunks[names_chunks == chunk_info$name_tag[i]]

      # restore chunk together with previous unmatched chunks
      document[line_index] <- paste0(c(chunk_info$chunk_text[i], unmatched),  collapse = "\n\n")
      unmatched <- NULL # reset
    }
  }

  return(document)
}
