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
  # Blank dataframe and set up to mold output into
  out_new <- data.frame(label = NA,
                        time = NA,
                        fleet = NA,
                        area = NA,
                        season = NA,
                        age = NA,
                        sex = NA,
                        intial = NA,
                        estimate = NA,
                        uncertainty = NA,
                        likelihood = NA,
                        gradient = NA,
                        estimated = NA # TRUE/FALSE
                        )

  # Convert SS3 output Report.sso file
  if (model == "ss") {
    # read SS report file
    # Associated function to extract columns for table - from r4ss
    get_ncol <- function(file, skip = 0) {
      nummax <- max(utils::count.fields(file,
                                        skip = skip, quote = "",
                                        comment.char = ""
      )) + 1
      return(nummax)
    }
    # Read as table
    dat <- read.table(
      file = output.file,
      col.names = 1:get_ncol(output.file),
      fill = TRUE,
      quote = "",
      colClasses = "character", # reads all data as characters
      nrows = -1,
      comment.char = "",
      blank.lines.skip = FALSE
    )

    # Step 1 identify and extract breaks in output
    # Function to extract rows, identify the dfs, and clean them up
    SS3_extract_df <- function(dat, label){
      # Locate the row containing the specified value from the df
      value_row <- which(apply(dat, 1, function(row) any(row == label)))[2]

      # If the parameter value is not found, return NA
      if(length(value_row) == 0){
        message("Label not found in data frame.")
        return(NA)
      }
      # Search for the next blank row after the value
      next_blank <- which(apply(dat, 1, function(row) all(is.na(row) | row == "" | row == "-" | row == "#")) & (seq_len(nrow(dat)) > value_row))[1]

      # Combine the rows surrounding the selected metric from the output table
      rows <- c(value_row, next_blank)

      # Extract the metric using the rows from above as a guide and clean up empty columns
      clean_df <- dat[rows[1]:(rows[2]-1),] |>
        naniar::replace_with_na_all(condition = ~.x == "")
      clean_df <- Filter(function(x)!all(is.na(x)), clean_df)

      return(clean_df)
    }

    # Estimated and focal parameters to put into reformatted output df - naming conventions from SS3
    param.names <-c()
    # Loop for all identified parameters to extract for plotting and use
    for(i in 1:length(param.names)){

    } # close loop
  } # close SS if statement

  if (model == "bam") {
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    # Step 1: Extract values from BAM output
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    dat <- dget(output.file)
    # dat_list <- list()

    # Extract data from list to make useable
    # Loop over all items to output each object/transform
    for (p in 1:length(dat)) {
      out <- dat[p]
      # is the object class double or list?
      if (is.double(out)) {
        # is the object a vector or matrix?
        if (is.vector(out)) {
          df <- data.frame(t(sapply(out, c))) |>
            tidyr::pivot_longer(cols = everything(), names_to = paste(names(out)), values_to = "value")
        } else if (is.matrix(out)) {
          df <- as.matrix(out)
        } # close if statement for checking if double obj is vector or matrix
        print(assign(names(dat[p]), df)) # print df from double class obj
      } else if (is.list(out)) {
        for (i in 1:length(out)) {
          # if the object is a vector treat as such
          if (is.vector(out[[i]])) {
            df <- data.frame(matrix(unlist(out[[i]]), nrow = length(out[[i]]), byrow = TRUE), stringsAsFactors = FALSE) |>
              dplyr::mutate(parm.cons = names(out[[i]]))
            # must add more proper call names - value of x will vary based on df
            colnames(df) <- c(names(out[i]), "x")

            # if the object is a matrix treat as such
          } else if (is.matrix(out[[i]])) {
            # Turn the object into a matrix  - will need to be handled later when transforming the data
            df <- as.matrix(out[[i]])
          }
          print(assign(names(dat[p][i]), df))
        } # close loop for list objects after pulled
      } # close if statement for checking if objects from dat is double or list
    } # close loop over objects listed in dat file

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    # Specify what to do with the pulled data
    # Might need to rename some values
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  } # close BAM if statement

  if (model == "asap") {
    # This is how Bai read the ASAP output
    # asap_output conversion written by Bai Li
    # asap_output <- dget(file.path(casedir, "output", subdir, paste("s", keep_sim_id[om_sim], sep = ""), "asap3.rdat"))
    # setwd(file.path(casedir, "output", subdir, paste("s", keep_sim_id[om_sim], sep = "")))
    # asap_std <- readRep("asap3", suffix = ".std")
  } # close asap if statement
} # close function
