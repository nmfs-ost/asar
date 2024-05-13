# Updates local files from those in google drive
# Overwrite the local files
# Suggested for edits to be done in google drive once uploaded then analyst will
#   pull all accepted changes using this fxn

# Name may change to update files - and other update_files fxn will be removed
download_files <- function(file,
                          gfile = NULL,
                          gpath = "trackdown",
                          shared_drive = NULL,
                          rm_gcomments = FALSE,
                          force = FALSE) {

  #---- check arguments ----
  if(!is.logical(rm_gcomments)) stop("rm_gcomments argument has to be logical",
                                     call. = FALSE)
  if(!is.logical(force)) stop("force argument has to be logical",
                              call. = FALSE)
  #---- start process ----
  main_process(paste("Downloading", emph_file(file), "with online changes..."))

  gpath <- sanitize_path(gpath) # remove possible final "/"

  #----    get autho token    ----
  if(!googledrive::drive_has_token()){
    googledrive::drive_auth(token = trackdown_token())
  }

  #---- check document info ----
  document <- evaluate_file(file = file,
                            gfile = gfile,
                            gpath = gpath,
                            shared_drive = shared_drive,
                            test = "single")

  check_supported_documents(document$file_info)

  #---- check user ----

  # check whether user really wants to download file from Google Drive
  if(interactive() && isFALSE(force)){
    response <- utils::menu(
      c("Yes", "No"),
      title = paste("Downloading the file from Google Drive will overwrite local file.",
                    "Do you want to proceed?"))

    if (response == 2L) {
      cli::cli_alert_danger("Process aborted")
      return(NULL)
    }
  }

  #---- download document ----

  # sub_process("Downloading...")

  downloaded_file <- file.path(document$file_info$path,
                               paste0(".temp-", document$file_info$file_basename, ".txt"))

  googledrive::local_drive_quiet() # suppress messages from googledrive

  # download file from Google Drive
  googledrive::drive_download(
    file = document$dribble_info$file,
    type = "text/plain",
    path = downloaded_file,
    overwrite = TRUE)

  temp_file <- file.path(document$file_info$path,
                         paste0(".temp-", document$file_info$file_name))
  file.rename(downloaded_file, temp_file)

  #---- restore file ----

  restore_file(temp_file = temp_file,
               file_name = document$file_info$file_name,
               path = document$file_info$path,
               rm_gcomments = rm_gcomments)


  #---- compare and replace ----

  if (!check_identity(temp_file = temp_file, local_file = document$file)) {
    file.rename(temp_file, document$file)
    finish_process(paste(cli::col_blue(file), "updated with online changes!"))
    finish_process("Process completed!")
    changed <- TRUE
  } else {
    cli::cli_alert_danger(paste("The local", cli::col_blue(file), "is identical with the Google Drive version", cli::col_red("Aborting...")))
    # remove temp-file
    invisible(unlink(temp_file))
    changed <-  FALSE
  }

  #---- end ----
  return(invisible(changed)) # to return a invisible TRUE/FALSE for rendering
}
