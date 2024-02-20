#' Convert Output
#'
#' Format stock assessment output files. Function is unfinished
#'
#' @param output.file name of the file(s) containing assessment model output
#' @param outdir output directory folder
#' @param model assessment model used in evaluation;
#'              "ss", "bam", "asap", "fims", "amak", "ms-java", "wham", "mas", "aspm"
#' @param file.format csv, txt, ss, rdata, dat, multi
#' @param multi.file TRUE or FALSE; default FALSE
#'
#' @author Samantha Schiano
#'
#' @return A reformatted and standardized version of assessment model results
#'         for application in building a stock assessment reports and to easily
#'         adapt results among regional assessments.
#'
#' @examples
#' convert_output(output.file = "CumReport.sso", outdir = "~", model = 'ss', file.format = 'multi', multi.file = FALSE)
#'
#' @export
#'
convert_output <- function(
    output.file = NULL,
    outdir = NULL,
    model = NULL,
    file.format = NULL,
    multi.file = FALSE,
    ...){

  if(model=="ss" & multi.file==TRUE){

  } else if (model=='ss' & multi.file==FALSE){
    stop("Output for stock synthesis is composed of multiple files. Recheck and add files to 'output.file' parameter then change 'multi.file' to TRUE.")
  }

  if(model=='bam'){
    if(length(output.file)>1 & multi.file==FALSE){
      stop("Number of files > 2 and no multi-file indicated. Change 'multi.file' to TRUE or double check you have the the right output file.")
    } else {

    }
  }

  if(model=='asap'){

  }

}
