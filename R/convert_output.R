#' Convert Output
#'
#' Format stock assessment output files. Function is unfinished
#'
#' @param output.file name of the file(s) containing assessment model output
#' @param outdir output directory folder
#' @param model assessment model used in evaluation;
#'              "ss", "bam", "asap", "fims", "amak", "ms-java", "wham", "mas", "aspm"
#'
#' @author Samantha Schiano
#'
#' @return A reformatted and standardized version of assessment model results
#'         for application in building a stock assessment reports and to easily
#'         adapt results among regional assessments.
#'
#'
#' @export
#'
convert_output <- function(
    output.file = NULL,
    outdir = NULL,
    model = NULL) {



  # Convert SS3 output Report.sso file
  if (model == "ss") {

  }

  if (model == "bam") {
    dat <- dget(output.file)
    # dat_list <- list()
    # Extract data from list to make useable
    # Loop over all items to output each object/transform
    for (p in 1:length(dat)) {
      # is the object class double or list?
      if (is.double(dat[p])) {
        # is the object a vector or matrix?
        if (is.vector(dat[p])) {
          df <- data.frame(t(sapply(dat[p], c))) |>
            tidyr::pivot_longer(cols = everything(), names_to = paste(names(dat[p])), values_to = "value")
        } else if (is.matrix(dat[[p]])) {
          df <- as.matrix(dat[[p]])
        } # close if statement for checking if double obj is vector or matrix
        print(assign(names(dat[p]), df)) # print df from double class obj

      } else if (is.list(dat[p])) {
        for (i in 1:length(dat[p])){
          # if the object is a vector treat as such
          if (is.vector(dat[p][[i]])){
            df <- data.frame(matrix(unlist(dat[p][[i]]), nrow=length(dat[p][[i]]), byrow=TRUE),stringsAsFactors=FALSE) |>
              dplyr::mutate(parm.cons = names(dat[p][[i]]))
            # must add more proper call names - value of x will vary based on df
            colnames(df) <- c(names(dat[p][i]), "x")

            # if the object is a matrix treat as such
          } else if (is.matrix(dat[p][[i]])){
            # Turn the object into a matrix  - will need to be handled later when transforming the data
            df <- as.matrix(dat[[p]][[i]])
          }
          print(assign(names(dat[[p]][i]), df))
        } # close loop for list objects after pulled
      } # close if statement for checking if objects from dat is double or list
      # dat_list[paste(names(dat[p]))] <- df
    } # close loop over objects listed in dat file

    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    # Specify what to do with the pulled data
    # Might need to rename some values
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  } # close if output is from BAM

  if (model == "asap") {
    # This is how Bai read the ASAP output
    # asap_output conversion written by Bai Li
    # asap_output <- dget(file.path(casedir, "output", subdir, paste("s", keep_sim_id[om_sim], sep = ""), "asap3.rdat"))
    # setwd(file.path(casedir, "output", subdir, paste("s", keep_sim_id[om_sim], sep = "")))
    # asap_std <- readRep("asap3", suffix = ".std")
  }
}
