#' Convert Output
#'
#' Format stock assessment output files. Function is unfinished
#'
#' @param output.file name of the file containing assessment model output. This is the Report.sso file for SS3, the rdat file for BAM, the...
#' @param input.file name of the input file for running the assessment model
#' @param outdir output directory folder
#' @param model assessment model used in evaluation;
#'              "ss3", "bam", "asap", "fims", "amak", "ms-java", "wham", "mas", "asap"
#'
#' @author Samantha Schiano
#'
#' @return A reformatted and standardized version of assessment model results
#'         for application in building a stock assessment reports and to easily
#'         adapt results among regional assessments. The resulting object is
#'         simply a transformed and machine readable version of a model output file.
#'
#'
#' @export
#'
convert_output <- function(
    output.file = NULL,
    input.file = NULL,
    outdir = NULL,
    model = NULL) {
  # Blank dataframe and set up to mold output into
  out_new <- data.frame(
    label = NA,
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
  if (model == "ss3") {
    # read SS3 report file
    # Associated function to extract columns for table - from r4ss
    get_ncol <- function(file, skip = 0) {
      nummax <- max(utils::count.fields(file,
        skip = skip, quote = "",
        comment.char = ""
      )) + 1
      return(nummax)
    }
    # Read as table
    dat <- utils::read.table(
      file = output.file,
      col.names = 1:get_ncol(output.file),
      fill = TRUE,
      quote = "",
      # colClasses = "character", # reads all data as characters
      nrows = -1,
      comment.char = "",
      blank.lines.skip = FALSE
    )

    # Step 1 identify and extract breaks in output
    # Function to extract rows, identify the dfs, and clean them up
    SS3_extract_df <- function(dat, label) {
      # Locate the row containing the specified value from the df
      value_row <- which(apply(dat, 1, function(row) any(row == label)))[2]

      # If the parameter value is not found, return NA
      if (is.na(value_row)) {
        message("Label not found in data frame.")
        return(NA)
      }
      # Search for the next blank row after the value
      next_blank <- which(apply(dat, 1, function(row) all(is.na(row) | row == "" | row == "-" | row == "#")) & (seq_len(nrow(dat)) > value_row))[1]
      if(is.na(next_blank)){
        next_blank <- nrow(dat)
      }
      # Combine the rows surrounding the selected metric from the output table
      rows <- c(value_row, next_blank)

      # Extract the metric using the rows from above as a guide and clean up empty columns
      clean_df <- dat[rows[1]:(rows[2] - 1), ] |>
        naniar::replace_with_na_all(condition = ~ .x == "")
      clean_df <- Filter(function(x) !all(is.na(x)), clean_df)

      return(clean_df)
    }

    # Estimated and focal parameters to put into reformatted output df - naming conventions from SS3
    # Is this all of the possible headers?
    param_names <- c(
      "LIKELIHOOD",
      "Input_Variance_Adjustment",
      "PARAMETERS",
      "DERIVED_QUANTITIES",
      "MGparm_By_Year_after_adjustments",
      "selparm(Size)_By_Year_after_adjustments",
      "selparm(Age)_By_Year_after_adjustments",
      "RECRUITMENT_DIST",
      "MORPH_INDEXING",
      "SIZEFREQ_TRANSLATION",
      "MOVEMENT",
      "EXPLOITATION",
      "TIME_SERIES",
      "SPR_series",
      "Kobe_Plot",
      "SPAWN_RECRUIT",
      "Spawning_Biomass_Report_1",
      "NUMBERS_AT_AGE_Annual_1",
      "Z_AT_AGE_Annual_1",
      "INDEX_1",
      "INDEX_2",
      "INDEX_3",
      "DISCARD_SPECIFICATION",
      "DISCARD_OUTPUT",
      "DISCARD_MORT",
      "MEAN_BODY_WT",
      "FIT_LEN_COMPS",
      "FIT_SIZE_COMPS",
      "FIT_AGE_COMPS",
      "OVERALL_COMPS",
      "LEN_SELEX",
      "RETENTION",
      "KEEPERS",
      "DEADFISH",
      "AGE_SELEX",
      "ENVIRONMENTAL_DATA",
      "TAG_Recapture",
      "NUMBERS_AT_AGE",
      "NUMBERS_AT_LENGTH",
      "CATCH_AT_AGE",
      "BIOLOGY",
      "SPR/YPR_PROFILE",
      "Dynamic_Bzero"
    )
    # SS3 Groupings
    std_set <- c(2,6,7,13,21,23,24,27,29,31,32,33,38,39,40,45,46,55)
    std2_set <- c(4,8) # can I make it so it falls into above set?
    cha_set <- 53
    rand_set <- c(9,10,22,28,30)
    unkn_set <- c(3,25,34,48,51,52)
    info_set <- c(1,5,26,35,39)
    aa.al_set <- c(11,12,14,16,17,18,19,20,36,37,47,49)
    nn_set <- c(41,42,43,44,50,54,56)

    groups <- list(std_set=std_set, std2_set=std2_set, cha_set=cha_set, rand_set=rand_set, unkn_set=unkn_set, info_set=info_set, aa.al_set=aa.al_set, nn_set=nn_set)

    for(i in 1:length(groups)){
      sub1 <- groups[[i]]
      x <- gsub("_set", "", names(groups[i]))
      # assign(x, c())
      vec <- c()
      for (j in 1:length(sub1)) {
        sub2 <- param_names[sub1[[j]]]
        vec <- c(vec, sub2)
      }
      assign(x, vec)
    }
    # Loop for all identified parameters to extract for plotting and use
    # Create list of parameters that were not found in the output file
    miss_parms <- list()
    for (i in 1:length(param_names)) {
      parm_sel <- param_names[i]
      extract <- SS3_extract_df(dat, parm_sel)
      if (is.na(extract)) {
        miss_parms <- c(miss_parms, parm_sel)
        next
      } else {
        if(parm_sel %in% std){
          # remove first row - naming
          df1 <- extract[-1,]
          # Find first row without NAs = headers
          df2 <- df1[complete.cases(df1), ]
          # identify first row
          row <- df2[1,]
          # make row the header names for first df
          colnames(df1) <- row
          # find row number that matches 'row'
          rownum <- prodlim::row.match(row, df1)
          # Subset data frame
          df3 <- df1[-c(1:rownum),]
          # Reformat data frame
          # add conditional statement if there is time, fleet, area, season, fleet, or sex as a header
          # if()
          if(colnames(df3) %in% c("Yr", "yr", "year")){
            df3 <- df3 |>
              dplyr::rename(year = Yr)
          }
          df4 <- tidyr::pivot_longer(df3, !year, names_to = "label", values_to = "estimate")
        } else if (parm_sel %in% std2) {

        } else if (parm_sel %in% cha) {

        } else if (parm_sel %in% rand) {

        } else if (parm_sel %in% unkn) {

        } else if (parm_sel %in% info) {

        } else if (parm_sel %in% aa.al) {

        } else if (parm_sel %in% nn) {

        } else {

        }



      }
    } # close loop
  } # close SS3 if statement

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
            tidyr::pivot_longer(cols = tidyr::everything(), names_to = paste(names(out)), values_to = "value")
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
