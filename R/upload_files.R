#' Upload file to Google Drive for collaborative writing and editing
#'
#' Uploads a local file to Google Drive as a plain text document. Will only
#' upload the file if it doesn't already exist in the chosen location. By
#' default files are uploaded in the folder "trackdown", if is not available on
#' Google Drive, permission to create it is required to the user. To update an
#' already existing file see [update_file()]. It is also possible to
#' upload the output (pdf or html) of the file specifying the `path_output`
#' argument. In case of html files, if `pagedown` package and Chrome are
#' available, users can decide to upload a pdf version of the html file.\cr\cr
#' To know more about `trackdown` workflow and features see
#' [trackdown-package()] help page.
#'
#' This is an edited version of the upload_file function from the `trackdown`
#' package. The package automatically runs an authentication if it isn't run
#' and NOAA blocks read and write access from the `trackdown` package. More
#' information about `trackdown` can be found at
#' https://claudiozandonella.github.io/trackdown/
#'
#' @param office NOAA line office analyst is associated with
#' @param region Region/state stock is associated with if applicable.
#' @param species Target species for assessment
#' @param shared_drive character. The name of a Google Drive shared drive
#'   (optional).
#' @param hide_code logical value indicating whether to remove code from the
#'   text document (chunks and header). Placeholders of type `"[[chunk-<name>]]"`
#'   are displayed instead.
#' @param  path_output default `NULL`, specify the path to the output to
#'   upload together with the other file. PDF are directly uploaded, HTML can be
#'   first converted into PDF if package `pagedown` and Chrome are
#'   available.
#' @param rich_text `r lifecycle::badge("experimental")` logical value (default is `TRUE`)
#'   indicating whether to upload to Google Docs a rich document (i.e.,
#'   important text that should not be changed is highlighted). See “Rich Text”
#'   in details section.
#' @param rich_text_par `r lifecycle::badge("experimental")` argument used to pass a list with custom
#'   settings for rich_text. See “Rich Text” in details section.
#' @param force logical value indicating whether to skip confirm check by user
#'   (default is `FALSE`).
#' @param open logical value indicating whether to open the created document
#'   in a browser (default is `TRUE` in interactive sessions).
#'
#' @return a dribble of the uploaded file (and output if specified).
#'
#' @details
#'
#' **Rich Text `r lifecycle::badge("experimental")`**
#'
#' The `rich_text` option (default is `TRUE`) allows to upload a rich
#' document to Google Docs. Important text that should not be changed is
#' highlighted. This includes, added instructions at the top of the document,
#' placeholders hiding the code, header of the document (YAML header or LaTeX
#' preamble), code chunks, and in-line code.
#'
#' Default colour is opaque yellow. You can customize the colour specifying the
#' `rgb_color` option in the `rich_text_par` argument. The
#' `rgb_color` has to be a list with elements `red`, `green`, and `blue`.
#' Each element has to be a numeric value between 0 and 1. See example below.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Change default color to opaque light-blue
#' upload_file(file = "path-to/my-file", rich_text = TRUE,
#'             rich_text_par = list(rgb_color = list(red = 102/255,
#'                                                   green = 204/255,
#'                                                   blue = 255/255)))
#' }
#'

upload_files <- function(
  office = NULL,
  region = NULL,
  species = NULL,
  shared_drive = NULL,
  hide_code = FALSE,
  path_output = NULL,
  rich_text = TRUE,
  rich_text_par = NULL,
  force = FALSE,
  open = rlang::is_interactive()
  ) {

  # Adding customization to function for SAR Tool
  # Files in skeleton
  if (!is.null(region)) {
    subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", region, "/", year)
  } else {
    subdir <- paste0("~/stock_assessment_templates", "/", office, "/", species, "/", year)
  }

  # Section skeletons/files - .qmd only
  all_sections <- list.files(subdir, pattern = ".qmd")
  sections <- all_sections[!grepl("skeleton.qmd", all_sections)]

  cat(cli::cat_rule(paste("Uploading files to", cli::col_magenta("Google Drive"))), "\n")

  gpath <- gsub("/$", "", gpath) # remove possible final "/"

  #---- check arguments ----
  # if(!is.logical(force)) stop("force argument has to be logical",
  #                             call. = FALSE)

  #----    get autho token    ----
  if(!googledrive::drive_has_token()){
    googledrive::drive_auth(token = trackdown_token())
  }

  # Upload all files in folder
  for (i in 1:length(sections)) {
    file = sections[i]
    gfile = gsub(".qmd","", sections[i])
    if(!is.null(region)){
      gpath = paste("National Stock Assessment Report Archive", office, region, species, year, sep = "/")
    } else {
      gpath = paste("National Stock Assessment Report Archive", office, species, year, sep = "/")
    }
    #---- check document info ----
    document <- evaluate_file(file = file,
                              gfile = gfile,
                              gpath = gpath,
                              shared_drive = shared_drive,
                              test = "none")

    # check_supported_documents(document$file_info)

    #---- check output info----
    # if (!is.null(path_output)) {
    #   output <- evaluate_file(file = path_output,
    #                           gfile = paste0(document$gfile, "-output"),  # name based on the correct gfile of the document
    #                           gpath = gpath,
    #                           shared_drive = shared_drive,
    #                           test = "none")
    # }

    #---- upload document ----
    res <- upload_document(
      file = document$file,
      file_info = document$file_info,
      gfile = document$gfile,
      gpath = gpath,
      dribble_document = document$dribble_info,
      hide_code = hide_code,
      rich_text = rich_text,
      rich_text_par = rich_text_par,
      update = FALSE)

    #---- upload output ----
    if (!is.null(path_output)) {
      dribble_output <- upload_output(
        path_output = output$file,
        output_info = output$file_info,
        gfile_output = output$gfile,
        gpath = gpath,
        dribble_output = output$dribble_info,
        update = FALSE,
        force = force)

      res[2, ] <- dribble_output
      res <- googledrive::as_dribble(res)
    }

    #---- end ----
    # trackdown::finish_process("Process completed!")

    # if (open) {
    #   utils::browseURL(res[["drive_resource"]][[1]][["webViewLink"]])
    # }

    return(invisible(res))
  }
}
