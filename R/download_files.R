#' Download recent version of section content from Google Drive
#'
#' @param office NOAA line office analyst is associated with
#' @param region Region/state stock is associated with if applicable.
#' @param species Target species for assessment
#' @param year year assessment is being conducted
#' @param rm_gcomments Remove comments from Google Doc
#' @param force
#'
#' @return Update local files any changes made in Google drive. Warning: this
#' function overwrites local files. Ideally, once these files are uploaded into
#' Google drive, the analyst and collaborations make their edits in Google docs.
#' This typically only includes text editing. If the user is advanced, one could
#' edit R chunks in the Google doc, but no testing can be done until it is
#' downloaded locally using this function However. this is not recommended.
#' @export logical value indicating whether to skip confirm check by user
#'   (default is `FALSE`).
#'
#' @examples
download_files <- function(office = NULL,
                           region = NULL,
                           species = NULL,
                           year = NULL,
                          # shared_drive = NULL,
                          rm_gcomments = FALSE,
                          force = FALSE) {
  # Find local files that have the file names
  if (!is.null(region)) {
    subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", region, "/", year)
  } else {
    subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", year)
  }
  # Section skeletons/files - .qmd only
  all_sections <- list.files(subdir, pattern = ".qmd")
  sections <- all_sections[!grepl("skeleton.qmd", all_sections)]

  #---- check arguments ----
  if(!is.logical(rm_gcomments)) stop("rm_gcomments argument has to be logical",
                                     call. = FALSE)
  if(!is.logical(force)) stop("force argument has to be logical",
                              call. = FALSE)
  #---- start process ----
  main_process(paste("Downloading files with online changes..."))

  # gpath <- sanitize_path(gpath) # remove possible final "/"

  #----    get autho token    ----
  if(!googledrive::drive_has_token()){
    googledrive::drive_auth()
  }

  #---- check user ----

  # check whether user really wants to download file from Google Drive
  if(interactive() && isFALSE(force)){
    response <- utils::menu(
      c("Yes", "No"),
      title = paste("Downloading files from Google Drive will overwrite local file.",
                    "Do you want to proceed?"))

    if (response == 2L) {
      cli::cli_alert_danger("Process aborted")
      return(NULL)
    }
  }

  for (i in 1:length(sections)) {
    file = sections[4]
    gfile = gsub(".qmd","", sections[4])
    if(!is.null(region)){
      gpath = paste("National Stock Assessment Report Archive", office, region, species, year, sep = "/")
    } else {
      gpath = paste("National Stock Assessment Report Archive", office, species, year, sep = "/")
    }
    #---- check document info ----
    document <- evaluate_file(file = file,
                              gfile = gfile,
                              gpath = gpath,
                              # shared_drive = shared_drive,
                              test = "single"
                              )

    check_supported_documents(document$file_info)

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
    # close loop
  }
}
