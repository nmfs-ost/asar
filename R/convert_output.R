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
    year = NA,
    fleet = NA,
    area = NA,
    season = NA,
    age = NA,
    sex = NA,
    intial = NA,
    estimate = NA,
    uncertainty = NA,
    uncertainty_label = NA,
    likelihood = NA,
    gradient = NA,
    estimated = NA, # TRUE/FALSE
    module_name = NA
  )
  out_new <- out_new[-1,]

  # Convert SS3 output Report.sso file
  if (model %in% c("ss3", "SS3")) {
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
    # Future changes will include a direct pull of these parameters from the output file instead of a manual list
    # Below the parameters are grouped and narrowed down into priority to reach deadline.
    # Other parameters will be developed into the future
    param_names <- c(
      "DEFINITIONS",
      "DERIVED_QUANTITIES",
      "ENVIRONMENTAL_DATA",
      "Input_Variance_Adjustment",
      "LIKELIHOOD",
      "MGparm_By_Year_after_adjustments",
      "MORPH_INDEXING",
      "OVERALL_COMPS",
      "PARAMETERS",
      "Parm_devs_detail",
      "BIOMASS_AT_AGE",
      "BIOMASS_AT_LENGTH",
      "CATCH",
      "DISCARD_AT_AGE",
      "EXPLOITATION",
      "CATCH_AT_AGE",
      "F_AT_AGE",
      "MEAN_SIZE_TIMESERIES",
      "NUMBERS_AT_AGE",
      "NUMBERS_AT_LENGTH",
      "SPAWN_RECRUIT",
      "SPAWN_RECR_CURVE",
      "SPR_SERIES",
      "TIME_SERIES",
      "COMPOSITION_DATABASE",
      "DISCARD_SPECIFICATION",
      "DISCARD_OUTPUT",
      "INDEX_1",
      "INDEX_2",
      "INDEX_3",
      "FIT_LEN_COMPS",
      "FIT_AGE_COMPS",
      "FIT_SIZE_COMPS",
      "MEAN_BODY_WT_OUTPUT",
      "TAG_Recapture",
      "AGE_SELEX",
      "LEN_SELEX",
      "selparm(Size)_By_Year_after_adjustments",
      "selparm(Age)_By_Year_after_adjustments",
      "SELEX_database",
      "AGE_AGE_KEY",
      "AGE_LENGTH_KEY",
      "AGE_SPECIFIC_K",
      "BIOLOGY",
      "Biology_at_age_in_endyr",
      "Growth_Parameters",
      "MEAN_BODY_WT(Begin)",
      "MOVEMENT",
      "Natural_Mortality",
      "RECRUITMENT_DIST",
      "Seas_Effects",
      "SIZEFREQ_TRANSLATION",
      "Dynamic_Bzero",
      "GLOBAL_MSY",
      "Kobe_Plot",
      "SPR/YPR_Profile"
    )
    # SS3 Groupings - manually done
    # Notes on the side indicate those removed since the information is not needed
    # std_set <- c(2,6,13,21,23,24,27,29,31,32,33,38,40,45,46,55) # Removing - 7
    # std2_set <- c(4,8) # can I make it so it falls into above set?
    # cha_set <- 53
    # rand_set <- c(9,10,22,28,30,39)
    # unkn_set <- c(3,25,34,48,51,52)
    # info_set <- c(1,5,15,26,35)
    # aa.al_set <- c(11,12,14,16,17,18,19,20,36,37,47,49)
    # nn_set <- c(41,42,43,44,50,54,56)

    # groups <- list(std_set=std_set, std2_set=std2_set, cha_set=cha_set, rand_set=rand_set, unkn_set=unkn_set, info_set=info_set, aa.al_set=aa.al_set, nn_set=nn_set)

    # for(i in 1:length(groups)){
    #   sub1 <- groups[[i]]
    #   x <- gsub("_set", "", names(groups[i]))
    #   # assign(x, c())
    #   vec <- c()
    #   for (j in 1:length(sub1)) {
    #     sub2 <- param_names[sub1[[j]]]
    #     vec <- c(vec, sub2)
    #   }
    #   assign(x, vec)
    # }
    # First release will converted output for SS3 will only include the below parameters
    std <- c("DERIVED_QUANTITIES",
             "MGparm_By_Year_after_adjustments",
             "CATCH",
             "SPAWN_RECRUIT",
             "TIME_SERIES",
             "DISCARD_OUTPUT",
             "INDEX_2",
             "Kobe_Plot")
    std2 <- c("OVERALL_COMPS")
    cha <- c("Dynamic_Bzero")
    rand <- c("SPR_SERIES")
    unkn <- c("MEAN_BODY_WT_OUTPUT")
    info <- c("LIKELIHOOD")
    aa.al <- c("BIOMASS_AT_AGE",
               "BIOMASS_AT_LENGTH",
               "DISCARD_AT_AGE",
               "CATCH_AT_AGE",
               "F_AT_AGE",
               "MEAN_SIZE_TIMESERIES",
               "NUMBERS_AT_AGE",
               "NUMBERS_AT_LENGTH",
               "AGE_SELEX",
               "LEN_SELEX")
    nn <- NA

    # Loop for all identified parameters to extract for plotting and use
    # Create list of parameters that were not found in the output file
    # std: 2, 6, 13, 21, 24, 27, 29, 55
    factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "time", "era", "subseas", "platoon","growth_pattern", "gp")
    errors <- c("StdDev","sd","se","SE","cv","CV")
    miss_parms <- c()
    # add progress bar for each SS3 variable
    # pb = txtProgressBar(min = 0, max = length(param_names), initial = 0)
    # Start loop over variables
    for (i in 1:length(param_names)) {
      # Indication for progress bar
      svMisc::progress(i,)
      # setTxtProgressBar(pb,i)
      # Start processing data frame
      parm_sel <- param_names[i]
      extract <- suppressMessages(SS3_extract_df(dat, parm_sel))
      if (!is.data.frame(extract)) {
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
          colnames(df3) <- tolower(row)
          # Reformat data frame
          if (any(colnames(df3) %in% c("Yr", "yr", "year"))) {
            df3 <- df3 |>
              dplyr::rename(year = yr)
          }
          if("label" %in% colnames(df3)){
            if (any(grepl("_[0-9]+$",df3$label))){
              df4 <- df3 |>
                dplyr::mutate(
                  year = stringr::str_extract(label, "[0-9]+$"),
                  label = stringr::str_remove(label, "_[0-9]+$"),
                  # Add factors consistent with other else
                  area = NA,
                  sex = NA,
                  growth_pattern = NA,
                  fleet = NA
                ) # need to remove the multiple error one
            }
          } else if (any(colnames(df3) %in% c(factors, errors))) {
            # Keeping check here if case arises that there is a similar situation to the error
            # aka there are multiple columns containing the string and they are not selected properly
            # if (any(factors %in% colnames(df3))) {
            #   fac_names <- names(df3)[grepl(paste(factors, collapse = "|"), names(df3))]
            # }

            df4 <- df3 |>
              tidyr::pivot_longer(!tidyselect::any_of(c(factors, errors)), names_to = "label", values_to = "estimate") |> # , colnames(dplyr::select(df3, tidyselect::matches(errors)))
              dplyr::mutate(area = dplyr::case_when(grepl("_[0-9]_", label) ~ stringr::str_extract(label, "(?<=_)[0-9]+"),
                                                    TRUE ~ NA),
                            sex = dplyr::case_when(grepl("_Fem_", label) ~ "female",
                                                   grepl("_Mal_", label) ~ "male",
                                                   grepl("_SX:1$", label) ~ "female",
                                                   grepl("_SX:2$", label) ~ "male",
                                                   TRUE ~ NA),
                            growth_Pattern = dplyr::case_when(grepl("_GP_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
                                                              TRUE ~ NA),
                            fleet = dplyr::case_when(grepl("):_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
                                                     grepl("):_[0-9][0-9]+$", label) ~ stringr::str_extract(label, "(?<=_)[0-9][0-9]$"),
                                                     TRUE ~ NA),
                            label = stringr::str_extract(label, "^.*?(?=_\\d|_GP|_Fem|_Mal|_SX|:|$)")
                            )
          } else {
            warning("Data frame not compatible.")
          }
          if(any(colnames(df4) %in% c("value"))) df4 <- dplyr::rename(df4, estimate = value)
          # need to add conditional for setup of the data param_SX:X_GP:#
          if(any(grepl("_SX:[0-9]_GP:[0-9]", unique(df4$label)))){
            df4 <- df4 |>
              dplyr::mutate(label = dplyr::case_when(grepl("_SX:[0-9]_GP:[0-9]", label) ~ stringr::str_extract(label, ),
                                                     grepl("_GP:[0-9]", label) ~ stringr::str_extract(label, ),
                                                     grepl("_GP:[0-9]", label) ~ stringr::str_extract(label, ),
                                                     TRUE ~ abel),
                            fleet = NA,
                            growth_pattern = NA,
                            sex = NA
                            )
          }
          # Check if error values are in the labels column and extract out
          if (any(sapply(errors, function(x) grepl(x, unique(df4$label))))) {
            err_names <- unique(df4$label)[grepl(paste(errors, collapse = "|"), unique(df4$label)) & !unique(df4$label) %in% errors]
            if(any(grepl("sel", err_names))){
              df4 <- df4
            } else if (length(intersect(errors, colnames(df4))) == 1) {
              df4 <- df4[-grep(paste(errors, "_", collapse = "|", sep = ""), df4$label),]
            } else {
              df4 <- df4 |>
                tidyr::pivot_wider(
                  names_from = label,
                  values_from = estimate,
                  id_cols = c(intersect(colnames(df4), factors)),
                  values_fill = NA
                ) |>
                tidyr::pivot_longer(
                  cols = -c(intersect(colnames(df4), factors), err_names),
                  names_to = "label",
                  values_to = "estimate"
                ) |>
                dplyr::select(tidyselect::any_of(c("label", "estimate", factors, errors, err_names)))
              if(length(err_names) > 1) {
                df4 <- df4 |>
                  dplyr::select(-c(err_names[2:length(err_names)]))
                find_error_value <- function(column_names, to_match_vector){
                  vals <- sapply(column_names, function(col_name) {
                    match <- sapply(to_match_vector, function(err) if (grepl(err, col_name)) err else NA)
                    na.omit(match)[1]
                    })
                  # only unique values and those that intersect with values vector
                  intersect(unique(vals), to_match_vector)
                }
                err_name <- find_error_value(names(df4), errors)
                colnames(df4)[grepl(paste(errors, collapse = "|"), colnames(df4))] <- err_name
              }
            }
          }

          df5 <- df4 |>
            dplyr::select(tidyselect::any_of(c("label", "estimate", "year", factors, errors))) |>
            dplyr::mutate(module_name = parm_sel)

          if(any(colnames(df5) %in% errors)){
            df5 <- df5 |>
              dplyr::mutate(uncertainty_label = tolower(colnames(dplyr::select(df4, tidyselect::any_of(paste("^",errors,"$",sep="")))))) |>
              dplyr::rename(lncertainty = intersect(colnames(df5), errors))
          } else {
            df5 <- df5 |>
              dplyr::mutate(uncertainty_label = NA,
                            uncertainty = NA)
          }
          # param_df <- df5
          # if (ncol(out_new) < ncol(df5)){
          #   warning(paste0("Transformed data frame for ", parm_sel, " has more columns than default."))
          # } else if (ncol(out_new) > ncol(df5)){
          #   warning(paste0("Transformed data frame for ", parm_sel, " has less columns than default."))
          # }
          df5[setdiff(tolower(names(out_new)), tolower(names(df5)))] <- NA
          out_new <- rbind(out_new, df5)
        } else if (parm_sel %in% std2) {
          # 4, 8
          # remove first row - naming
          df1 <- extract[-1,]
          # Find first row without NAs = headers
          df2 <- df1[complete.cases(df1), ]
          # rotate data
          # identify first row
          row <- df2[1,]
          # make row the header names for first df
          colnames(df1) <- row
          # find row number that matches 'row'
          # rownum <- prodlim::row.match(row, df1)
          # Subset data frame
          df1 <- df1[-1,]
          # Defining columns for the grouping
          if(any(grepl("len_bins", colnames(df1)))){
            std2_id <- "len_bins"
          } else {
            std2_id <- "fleet"
          }
          # pivot data
          df3 <- df1[-max(nrow(df1)),] |>
            tidyr::pivot_longer(
              cols = -intersect(c(factors, errors, "len_bins"), colnames(df1)),
              names_to = "label",
              values_to = "estimate"
            ) |>
            tidyr::pivot_wider(
              names_from = tidyselect::all_of(std2_id),
              values_from = estimate
            )
          # if(!std2_id %in% factors){
            colnames(df3) <- stringr::str_replace(colnames(df3), "Label", std2_id )
          # }
          df4 <- df3 |>
            tidyr::pivot_longer(
              cols = -intersect(c(factors, errors, "len_bins"), colnames(df1)),
              names_to = "Label",
              values_to = "Estimate"
            ) |>
            dplyr::mutate(module_name = parm_sel)
          # Add to new dataframe
          df4[setdiff(tolower(names(out_new)), tolower(names(df4)))] <- NA
          out_new <- rbind(out_new, df4)
        } else if (parm_sel %in% cha) {
          miss_parms <- c(miss_parms, parm_sel)
          next
        } else if (parm_sel %in% rand) {
          miss_parms <- c(miss_parms, parm_sel)
          next
        } else if (parm_sel %in% unkn) {
          miss_parms <- c(miss_parms, parm_sel)
          next
        } else if (parm_sel %in% info) {
          miss_parms <- c(miss_parms, parm_sel)
          next
        } else if (parm_sel %in% aa.al) {
          miss_parms <- c(miss_parms, parm_sel)
          next
        } else if (parm_sel %in% nn) {
          miss_parms <- c(miss_parms, parm_sel)
          next
        } else {
          miss_parms <- c(miss_parms, parm_sel)
          next
        }
      } # close if param is in output file
      # Close progress bar for iteration
      # close(pb)
    Sys.sleep(0.01)
    if(i == max(length(param_names))) cat("Done! \n")
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
  out_new
} # close function
