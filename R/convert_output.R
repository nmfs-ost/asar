#' Convert Output
#'
#' Format stock assessment output files. Function is unfinished
#'
#' @param output.file name of the file containing assessment model output. This is the Report.sso file for SS3, the rdat file for BAM, the...
#' @param input.file name of the input file for running the assessment model
#' @param outdir output directory folder
#' @param model assessment model used in evaluation;
#'              "ss3", "bam", "asap", "fims", "amak", "ms-java", "wham", "mas", "asap"
#' @param fleet_names Names of fleets in the assessment model as shortened in the
#'                    output file, required for transforming BAM model output
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
    model = NULL,
    fleet_names = NULL) {

  # Blank dataframe and set up to mold output into
  out_new <- data.frame(
    label = NA,
    time = NA,
    year = NA,
    fleet = NA,
    area = NA,
    season = NA,
    subseason = NA,
    age = NA,
    sex = NA,
    growth_pattern = NA,
    len_bins = NA,
    intial = NA,
    estimate = NA,
    uncertainty = NA,
    uncertainty_label = NA,
    likelihood = NA,
    gradient = NA,
    estimated = NA, # TRUE/FALSE
    module_name = NA,
    # Additional factors from SS3
    bio_pattern = NA,
    birthseas = NA,
    settlement = NA,
    morph = NA,
    # beg/mid = NA, # need to identify df where this is applicable
    type = NA,
    factor = NA,
    platoon = NA,
    month = NA,
    sexes = NA,
    part = NA,
    bin = NA,
    kind = NA
  )
  out_new <- out_new[-1,]

  # pull together path
  if(!is.null(outdir)){
    output.file = paste(outdir, "/", output.file, sep = "")
  } else {
    output.file = output.file
  }

  # Check if can locate output file
  if(!file.exists(output.file)){
    stop("output file path is invalid.")
  }

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

      # warning("This functions only operates with Stock Synthesis version 3.30 and newer.")
      # question1 <- readline("Would you like to proceed? (Y/N)")
      # if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
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
      # } else {
      #   stop("This function in its current state can not process the data.")
      # }

      # Check SS3 model version
      vers <- as.numeric(stringr::str_extract(dat[1,1], "[0-9].[0-9][0-9]"))
      if(vers < 3.3){
        stop("This function in its current state can not process the data.")
      }

    # Estimated and focal parameters to put into reformatted output df - naming conventions from SS3
    # Future changes will include a direct pull of these parameters from the output file instead of a manual list
    # Below the parameters are grouped and narrowed down into priority to reach deadline.
    # Other parameters will be developed into the future
    param_names <- c(
      # "DEFINITIONS",
      "DERIVED_QUANTITIES",
      "ENVIRONMENTAL_DATA",
      # "Input_Variance_Adjustment",
      "LIKELIHOOD",
      "MGparm_By_Year_after_adjustments",
      # "MORPH_INDEXING",
      "OVERALL_COMPS",
      "PARAMETERS",
      "Parm_devs_detail",
      "BIOMASS_AT_AGE",
      "BIOMASS_AT_LENGTH",
      "CATCH",
      "DISCARD_AT_AGE",
      # "EXPLOITATION",
      "CATCH_AT_AGE",
      "F_AT_AGE",
      "MEAN_SIZE_TIMESERIES",
      "NUMBERS_AT_AGE",
      "NUMBERS_AT_LENGTH",
      "SPAWN_RECRUIT",
      "SPAWN_RECR_CURVE",
      # "SPR_SERIES",
      "TIME_SERIES",
      # "COMPOSITION_DATABASE",
      # "DISCARD_SPECIFICATION",
      "DISCARD_OUTPUT",
      "INDEX_1",
      "INDEX_2",
      "INDEX_3",
      "FIT_LEN_COMPS",
      "FIT_AGE_COMPS",
      "FIT_SIZE_COMPS",
      "MEAN_BODY_WT_OUTPUT",
      # "TAG_Recapture",
      "AGE_SELEX",
      "LEN_SELEX",
      "selparm(Size)_By_Year_after_adjustments",
      "selparm(Age)_By_Year_after_adjustments",
      "SELEX_database",
      # "AGE_AGE_KEY",
      # "AGE_LENGTH_KEY",
      # "AGE_SPECIFIC_K",
      # "BIOLOGY",
      # "Biology_at_age_in_endyr",
      "Growth_Parameters",
      # "MEAN_BODY_WT(Begin)",
      "MOVEMENT",
      "Natural_Mortality",
      # "RECRUITMENT_DIST",
      # "Seas_Effects",
      # "SIZEFREQ_TRANSLATION",
      "Dynamic_Bzero",
      # "GLOBAL_MSY",
      "Kobe_Plot",
      "SPR/YPR_Profile"
    )
    # SS3 Groupings - manually done
    # Notes on the side indicate those removed since the information is not needed
    # std_set <- c(2,6,13,21,23,24,27,29,31,32,33,38,40,45,46,55) # Removing - 7
    # std2_set <- c(4,8) # can I make it so it falls into above set?
    # cha_set <- 53
    # rand_set <- c(9,10,22,28,30,39)
    # unkn_set <- c(3,25,34,48,51,52) # needs to be found and recategorized
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
             # "FIT_LEN_COMPS",
             # "FIT_AGE_COMPS",
             # "FIT_SIZE_COMPS",
             "SELEX_database",
             # "Biology_at_age_in_endyr",
             # "Growth_Parameters",
             "Kobe_Plot")
    std2 <- c("OVERALL_COMPS")
    cha <- c("Dynamic_Bzero")
    rand <- c(# "SPR_SERIES",
              # "selparm(Size)_By_Year_after_adjustments",
              # "selparm(Age)_By_Year_after_adjustments"
      )
    # info <- c("LIKELIHOOD")
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
    # nn <- NA

    # Loop for all identified parameters to extract for plotting and use
    # Create list of parameters that were not found in the output file
    # 1,4,10,17,19,20,22,32,37
    factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "season", "time", "era", "subseas", "subseason", "platoon", "platoo","growth_pattern", "gp")
    errors <- c("StdDev","sd","se","SE","cv","CV")
    miss_parms <- c()
    out_list <- list()
    # add progress bar for each SS3 variable
    # pb = txtProgressBar(min = 0, max = length(param_names), initial = 0)
    # Start loop over variables
    for (i in 1:length(param_names)) {
      # Indication for progress bar
      # svMisc::progress(i,)
      # Start processing data frame
      parm_sel <- param_names[i]
      extract <- suppressMessages(SS3_extract_df(dat, parm_sel))
      message("Processed ", parm_sel)
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
          if (any(colnames(df3) %in% c("Yr", "yr"))) {
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

            df4 <- df3 |>
              tidyr::pivot_longer(
                !tidyselect::any_of(c(factors, errors)),
                names_to = "label",
                values_to = "estimate") |> # , colnames(dplyr::select(df3, tidyselect::matches(errors)))
              dplyr::mutate(area = dplyr::case_when(grepl("_[0-9]_", label) ~ stringr::str_extract(label, "(?<=_)[0-9]+"),
                                                    grepl(":_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]+"),
                                                    grepl(":_[0-9][0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9][0-9]+"),
                                                    TRUE ~ NA),
                            sex = dplyr::case_when(grepl("_fem_", label) ~ "female",
                                                   grepl("_mal_", label) ~ "male",
                                                   grepl("_sx:1$", label) ~ "female",
                                                   grepl("_sx:2$", label) ~ "male",
                                                   grepl("_sx:1_", label) ~ "female",
                                                   grepl("_sx:2_", label) ~ "male",
                                                   TRUE ~ NA),
                            growth_pattern = dplyr::case_when(grepl("_gp_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
                                                              grepl("_gp:[0-9]$", label) ~ stringr::str_extract(label, "(?<=:)[0-9]$"),
                                                              grepl("_gp:[0-9][0-9]$", label) ~ stringr::str_extract(label, "(?<=:)[0-9][0-9]$"),
                                                              TRUE ~ NA))

            if("fleet" %in% colnames(df3)){
              df4 <- df4 |>
                dplyr::mutate(fleet = dplyr::case_when("fleet" %in% colnames(df3) ~ fleet,
                                                       # grepl("):_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
                                                       # grepl("):_[0-9][0-9]+$", label) ~ stringr::str_extract(label, "(?<=_)[0-9][0-9]$"),
                                                       TRUE ~ NA),
                              label = stringr::str_extract(label, "^.*?(?=_\\d|_gp|_fem|_mal|_sx|:|$)"))
            } else {
              df4 <- df4 |>
                dplyr::mutate(fleet = dplyr::case_when(grepl("):_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
                                                       grepl("):_[0-9][0-9]+$", label) ~ stringr::str_extract(label, "(?<=_)[0-9][0-9]$"),
                                                       TRUE ~ NA),
                              label = stringr::str_extract(label, "^.*?(?=_\\d|_gp|_fem|_mal|_sx|:|$)"))
            }
          } else {
            warning("Data frame not compatible.")
          }
          if(any(colnames(df4) %in% c("value"))) df4 <- dplyr::rename(df4, estimate = value)

          # Check if error values are in the labels column and extract out
          if (any(sapply(errors, function(x) grepl(x, unique(df4$label))))) {
            err_names <- unique(df4$label)[grepl(paste(errors, collapse = "|"), unique(df4$label)) & !unique(df4$label) %in% errors]
            if(any(grepl("sel", err_names))){
              df4 <- df4
            } else if (length(intersect(errors, colnames(df4))) == 1) {
              df4 <- df4[-grep(paste(errors, "_", collapse = "|", sep = ""), df4$label),]
            } else {
              df42 <- df4 |>
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
                if(any(grepl(paste(err_names, collapse = "|"), colnames(df4)))){
                df4 <- df4 |>
                  dplyr::select(-tidyselect::all_of(c(err_names[2:length(err_names)])))
                } else {
                  df4 <- df4 |>
                    dplyr::filter(!(label %in% err_names[2:length(err_names)]))
                }
                find_error_value <- function(column_names, to_match_vector){
                  vals <- sapply(column_names, function(col_name) {
                    match <- sapply(to_match_vector, function(err) if (grepl(err, col_name)) err else NA)
                    na.omit(match)[1]
                    })
                  # only unique values and those that intersect with values vector
                  intersect(unique(vals), to_match_vector)
                }
                if(any(grepl(paste(err_names, collapse = "|"), colnames(df4)))){
                  err_name <- find_error_value(names(df4), errors)
                  if(length(err_name)>1){
                    err_name <- stringr::str_extract(err_names[1], paste(errors, collapse = "|"))
                  }
                  colnames(df4)[grepl(err_name, colnames(df4))] <- err_name
                } else {
                  err_name <- find_error_value(unique(df4$label), errors)
                  colnames(df4)[grepl(paste(errors, collapse = "|"), colnames(df4))] <- err_name
                }
              }
            }
          }

          df5 <- df4 |>
            dplyr::select(tidyselect::any_of(c("label", "estimate", "year", factors, errors))) |>
            dplyr::mutate(module_name = parm_sel)

          if(any(colnames(df5) %in% errors)){
            df5 <- df5 |>
              dplyr::mutate(uncertainty_label = intersect(names(df5), errors)) |>
              dplyr::rename(uncertainty = intersect(colnames(df5), errors))
            colnames(df5) <- tolower(names(df5))
          } else {
            df5 <- df5 |>
              dplyr::mutate(uncertainty_label = NA,
                            uncertainty = NA)
            colnames(df5) <- tolower(names(df5))
          }
          # param_df <- df5
          # if (ncol(out_new) < ncol(df5)){
          #   warning(paste0("Transformed data frame for ", parm_sel, " has more columns than default."))
          # } else if (ncol(out_new) > ncol(df5)){
          #   warning(paste0("Transformed data frame for ", parm_sel, " has less columns than default."))
          # }
          if("seas" %in% colnames(df5)){
            df5 <- df5 |>
              dplyr::rename(season = seas)
          }
          if("subseas" %in% colnames(df5)) df5 <- dplyr::rename(df5, subseason = subseas)
          df5[setdiff(tolower(names(out_new)), tolower(names(df5)))] <- NA
          if (ncol(out_new) < ncol(df5)){
            diff <- setdiff(names(df5), names(out_new))
            message("FACTORS REMOVED: ", parm_sel, " - ", paste(diff, collapse = ", "))
            # warning(parm_sel, " has more columns than the output data frame. The column(s) ", paste(diff, collapse = ", ")," are not found in the standard file. It was excluded from the resulting output. Please open an issue for developer fix.")
            df5 <- dplyr::select(df5, -c(diff))
            out_list[[parm_sel]] <- df5
          } else {
            out_list[[parm_sel]] <- df5
          }
        } else if (parm_sel %in% std2) {
          # 4, 8
          # remove first row - naming
          df1 <- extract[-1,]
          # Find first row without NAs = headers
          df2 <- df1[complete.cases(df1), ]
          # rotate data
          # identify first row
          row <- tolower(df2[1,])
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
            colnames(df3) <- stringr::str_replace(colnames(df3), "label", std2_id)
          # }
          if(any(duplicated(tolower(names(df3))))){
            repd_name <- names(which(table(tolower(names(df3))) > 1))
            df3 <- df3 |>
              dplyr::select(-tidyselect::all_of(repd_name))
            colnames(df3) <- tolower(names(df3))
          }
          df4 <- df3 |>
            tidyr::pivot_longer(
              cols = -intersect(c(factors, errors, "len_bins"), colnames(df1)),
              names_to = "label",
              values_to = "estimate"
            ) |>
            dplyr::mutate(module_name = parm_sel)
          if("seas" %in% colnames(df4)){
            df4 <- df4 |>
              dplyr::rename(season = seas)
          }
          # Add to new dataframe
          df4[setdiff(tolower(names(out_new)), tolower(names(df4)))] <- NA
          if (ncol(out_new) < ncol(df4)){
            diff <- setdiff(names(df4), names(out_new))
            message("FACTORS REMOVED: ", parm_sel, " - ", paste(diff, collapse = ", "))
            # warning(parm_sel, " has more columns than the output data frame. The column(s) ", paste(diff, collapse = ", ")," are not found in the standard file. It was excluded from the resulting output. Please open an issue for developer fix.")
            df4 <- dplyr::select(df4, -tidyselect::all_of(diff))
            out_list[[parm_sel]] <- df4
          } else {
            out_list[[parm_sel]] <- df4
          }
        } else if (parm_sel %in% cha) {
          # Only one keyword characterized as this
          area_row <- which(apply(extract, 1, function(row) any(row == "Area:")))
          area_val <- extract[area_row, 3]
          gp_row <- which(apply(extract, 1, function(row) any(row == "GP:")))
          gp_val <- extract[gp_row, 3]
          df1 <- extract[-c(1:4),]
          colnames(df1) <- c("year","era","estimate")
          df2 <- df1 |>
            dplyr::mutate(label = parm_sel,
                          area = area_val$X3,
                          growth_pattern = gp_val$X3,
                          module_name = parm_sel)
          df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
          if (ncol(out_new) < ncol(df2)){
            diff <- setdiff(names(df2), names(out_new))
            message("FACTORS REMOVED: ", parm_sel, " - ", paste(diff, collapse = ", "))
            df2 <- dplyr::select(df2, -tidyselect::all_of(diff))
            out_list[[parm_sel]] <- df2
          } else {
            out_list[[parm_sel]] <- df2
          }
        # } else if (parm_sel %in% rand) {
        #   miss_parms <- c(miss_parms, parm_sel)
        #   next
        # } else if (parm_sel %in% info) {
        #   miss_parms <- c(miss_parms, parm_sel)
        #   next
        } else if (parm_sel %in% aa.al) {
          # remove first row - naming
          df1 <- extract[-1,]
          # Find first row without NAs = headers
          df2 <- df1[complete.cases(df1), ]
          # identify first row
          row <- df2[1,]
          if(any(row %in% "XX")){
            loc_xx <- grep("XX", row)
            row <- row[row!="XX"]
            df1 <- df1[ ,-loc_xx]
          }
          # make row the header names for first df
          colnames(df1) <- row
          # find row number that matches 'row'
          rownum <- prodlim::row.match(row, df1)
          # Subset data frame
          df3 <- df1[-c(1:rownum),]
          colnames(df3) <- tolower(row)

          # aa.al names
          naming <- c("biomass", "discard", "catch", "f", "mean_size", "numbers","sel","mean_body_wt","natural_mortality")
          if(stringr::str_detect(tolower(parm_sel), paste(naming, collapse = "|"))){
            label = stringr::str_extract(tolower(parm_sel), paste(naming, collapse = "|"))
            if(length(label) > 1) warning("Length of label is > 1.")
            if(label == "f"){
              label = "F"
            }
          }
          if(grepl("age", tolower(parm_sel))){
            fac <- "age"
          } else if (any(grepl("length|len", tolower(parm_sel)))) {
            fac <- "len_bins"
          } else if (any(grepl("size", tolower(parm_sel)))) {
            fac <- "age"
          } else {
            fac <- "age"
          }
          # Reformat dataframe
          if (any(colnames(df3) %in% c("Yr", "yr", "year"))) {
            df3 <- df3 |>
              dplyr::rename(year = yr)
          }
          if(any(colnames(df3) %in% c("seas"))){
            df3 <- df3 |>
              dplyr::rename(season = seas)
          }
          if(any(colnames(df3) %in% c("subseas"))){
            df3 <- df3 |>
              dplyr::rename(subseason = subseas)
          }
          if("label" %in% colnames(df3)){
            df3 <- dplyr::select(df3, -"label")
          }
          # Pivot table long
          other_factors <- c("bio_pattern", "birthseas", "settlement", "morph", "beg/mid", "type", "label", "factor", "platoon", "month","sexes","part","bin","kind")
          df4 <- df3 |>
            tidyr::pivot_longer(
              cols = -intersect(c(factors, errors, other_factors), colnames(df3)),
              names_to = fac[1],
              values_to = "estimate"
            ) |>
            dplyr::mutate(label = label[1],
                          module_name = parm_sel[1])
          if(any(grepl("morph", colnames(df4)))){
            df4 <- df4 |>
              dplyr::rename(growth_pattern = morph)
          }
          if("label" %in% colnames(df4) & "factor" %in% colnames(df4)){
            df4 <- df4 |>
              dplyr::select(-label) |>
              dplyr::mutate(label = dplyr::case_when(grepl("sel", factor) ~ "sel",
                                                     TRUE ~ factor)) |>
              dplyr::select(-factor)

          }
          # Bind to new df
          df4[setdiff(tolower(names(out_new)), tolower(names(df4)))] <- NA
          if (ncol(out_new) < ncol(df4)){
            diff <- setdiff(names(df4), names(out_new))
            message("FACTORS REMOVED: ", parm_sel, " - ", paste(diff, collapse = ", "))
            # warning(parm_sel, " has more columns than the output data frame. The column(s) ", paste(diff, collapse = ", ")," are not found in the standard file. It was excluded from the resulting output. Please open an issue for developer fix.")
            df4 <- dplyr::select(df4, -tidyselect::all_of(diff))
            out_list[[parm_sel]] <- df4
          } else {
            out_list[[parm_sel]] <- df4
          }
        # } else if (parm_sel %in% nn) {
        #   miss_parms <- c(miss_parms, parm_sel)
        #   next
        } else {
          miss_parms <- c(miss_parms, parm_sel)
          next
        }
      } # close if param is in output file
      # Close progress bar for iteration
      # close(pb)
    # Sys.sleep(0.01)
    # if(i == max(length(param_names))) cat("Done! \n")
    } # close loop
    if(length(miss_parms)>0){
      message("Some parameters were not found or included in the output file. The inital release of this converter only inlcudes to most necessary parameters and values. The following parameters were not added into the new output file: \n", paste(miss_parms, collapse = "\n"))
    }
    out_new <- Reduce(rbind, out_list)
  } # close SS3 if statement

  if (model %in% c("bam", "BAM")) {
    # check fleet names are input
    if (is.null(fleet_names)) {
      message("No fleet names were added as an argument. Fleets will not be extracted from the data.")
      fleet_names <- NA
    }
    # Extract values from BAM output - model file after following ADMB2R
    dat <- dget(output.file)
    # Create list for morphed dfs to go into (for rbind later)
    out_list <- list()

    factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "season", "time", "era", "subseas", "subseason", "platoon", "platoo","growth_pattern", "gp")
    errors <- c("StdDev","sd","se","SE","cv","CV")
    # argument for function when model == BAM
    # fleet_names <- c("cl", "cL","cp","mrip","ct", "hb", "HB", "comm","Mbft","CVID")

    #### Start loop ####
    # Extract data from list fit to output df
    # Loop over all items to output each object/transform
    # Not transforming or inclusing info chunk
    for (p in 2:length(dat)) {
      extract <- dat[p]
      # is the object class matrix, list, or vector
      if (is.vector(extract[[1]])) {
        if(is.list(extract[[1]])){ # indicates vector and list
          if(any(sapply(extract[[1]], is.matrix))) {
            extract_list <- list()
            for(i in 1:length(extract[[1]])){
              if(is.vector(extract[[1]][[i]])) {
                df <- as.data.frame(extract[[1]][i]) |>
                  tibble::rownames_to_column(var = "age") |>
                  tidyr::pivot_longer(
                    cols = -age,
                    values_to = "estimate",
                    names_to = "label"
                  ) |>
                  dplyr::mutate(label = names(extract[[1]][i]),
                                # label_init = names(extract[[1]][i]),
                                fleet = dplyr::case_when(grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                                                         TRUE ~ NA),
                                module_name = names(extract),
                                label = dplyr::case_when(grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), ""),
                                                         TRUE ~ label))
                df[setdiff(tolower(names(out_new)), tolower(names(df)))] <- NA
                extract_list[[names(extract[[1]][i])]] <- df
              } else {
                df <- as.data.frame(extract[[1]][[i]]) |>
                  tibble::rownames_to_column(var = "year")
                if (grepl("lcomp", names(extract[[1]][i]))) {
                  namesto = "len_bins"
                } else if (grepl("acomp", names(extract[[1]][i]))) {
                  namesto = "age"
                } else {
                  # Default
                  namesto = "age"
                }
                df2 <- df |>
                  tidyr::pivot_longer(
                    cols = -year,
                    names_to = namesto,
                    values_to = "estimate") |>
                  dplyr::mutate(module_name = names(extract),
                                label = names(extract[[1]][i]),
                                # label_init = names(extract[[1]][i]),
                                fleet = dplyr::case_when(
                                  grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                                  TRUE ~ NA), # stringr::str_extract(module_name, "(?<=\\.)\\w+(?=\\.)"),
                                label = dplyr::case_when(is.na(fleet) ~ names(extract[[1]][i]),
                                                         TRUE ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), "")))# stringr::str_replace(module_name, "\\.[^.]+\\.", "."))
                df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
                extract_list[[names(extract[[1]][i])]] <- df2
              } # close if statement
            } # close for loop
            new_df <- Reduce(rbind, extract_list)
            out_list[[names(extract)]] <- new_df
          } else if(any(sapply(extract[[1]], is.vector))){ # all must be a vector to work - so there must be conditions for dfs with a mix
            df <- data.frame(extract[[1]])
            if (length(intersect(colnames(df), c(factors, errors))) > 0) {
              df2 <- df |>
                tidyr::pivot_longer(
                  cols = -intersect(colnames(df), c(factors, errors)),
                  names_to = "label",
                  values_to = "estimate"
                ) |>
                dplyr::mutate(module_name = names(extract))
            } else {
              df2 <- df |>
                tidyr::pivot_longer(
                  cols = everything(),
                  names_to = "label",
                  values_to = "estimate"
                ) |>
                dplyr::mutate(module_name = names(extract))
            }
            if (any(grepl(paste(fleet_names, collapse = "|"), unique(df2$label)))) {
              df2 <- df2 |>
                dplyr::mutate(fleet = dplyr::case_when(grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names,collapse="|")),
                                                       # grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(ex, paste(fleet_names,collapse="|")),
                                                       TRUE ~ NA),
                              # Number after fleet name is what? variable among df?
                              age = dplyr::case_when(grepl("[0-9]$", label) ~ stringr::str_extract(label, "[0-9]$"),
                                                     TRUE ~ NA)
                              # label = dplyr::case_when(grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(ex, paste("^(.*)", fleet_names, "[0-9]", sep = "",collapse = "|")),
                              #                          grepl(paste(fleet_names, "[0-9$]", collapse = "|"), label) ~ stringr::str_extract(label, "[0-9]$"),
                              #                          TRUE ~ label)
                )
            } else if (any(grepl("[0-9]$", unique(df2$label)))) {
              df2 <- df2 |>
                dplyr::mutate(fleet = NA,
                              # Number after fleet name is what? variable among df?
                              age = dplyr::case_when(grepl("[0-9]$", label) ~ stringr::str_extract(label, "[0-9]$"),
                                                     TRUE ~ NA),
                              # label_init = label,
                              label = stringr::str_remove(label, ".[0-9]+$")
                )
            } else {
              df2 <- df2 |>
                dplyr::mutate(fleet = NA,
                              age = NA,
                              label = label,
                              module_name = names(extract))
            }
            df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
            out_list[[names(extract)]] <- df2
          } else {
            message("Not compatible.")
          }
        } else { # vector only
          df <- as.data.frame(extract) |>
            tibble::rownames_to_column(var = "label") |>
            tidyr::pivot_longer(
              cols = -label,
              names_to = "module_name",
              values_to = "estimate"
            )
          if (any(grepl("age", colnames(df)))){
            df <- df |>
              tidyr::pivot_longer(
                cols = -intersect(c(factors, errors), colnames(df)),
                names_to = "age",
                values_to = "estimate"
              ) |>
              dplyr::mutate(label = names(extract[[1]]),
                            age = dplyr::case_when(grepl(".[0-9]$", age) ~ stringr::str_extract(age, "[0-9]"),
                                                   grepl(".[0-9][0-9]$", age) ~ stringr::str_extract(age, "[0-9][0-9]"),
                                                   TRUE ~ NA),
                            module_name = names(extract))
          }
          if (any(grepl(paste(fleet_names, collapse = "|"), df$label))) {
            df <- df |>
              dplyr::mutate(fleet = dplyr::case_when(grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                                                     TRUE ~ NA),
                            label = dplyr::case_when(is.na(fleet) ~ label,
                                                     TRUE ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), "")))
          }
          df[setdiff(tolower(names(out_new)), tolower(names(df)))] <- NA
          out_list[[names(extract)]] <- df
        }
      } else if (is.list(extract[[1]])) { # list only
        if(any(sapply(extract[[1]], is.matrix))) {
          extract_list <- list()
          for(i in 1:length(extract[[1]])){
            if(is.vector(extract[[1]][[i]])) {
              df <- as.data.frame(extract[[1]][i]) |>
                tibble::rownames_to_column(var = "age") |>
                tidyr::pivot_longer(
                  cols = -age,
                  values_to = "estimate",
                  names_to = "label"
                ) |>
                dplyr::mutate(label = names(extract[[1]][i]),
                              # label_init = names(extract[[1]][i]),
                              fleet = dplyr::case_when(grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                                                       TRUE ~ NA),
                              module_name = names(extract),
                              label = dplyr::case_when(grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), ""),
                                                       TRUE ~ label))
              df[setdiff(tolower(names(out_new)), tolower(names(df)))] <- NA
              extract_list[[names(extract[[1]][i])]] <- df
            } else {
              df <- as.data.frame(extract[[1]][[i]]) |>
                tibble::rownames_to_column(var = "year")
              if (grepl("lcomp", names(extract[[1]][i]))) {
                namesto = "len_bins"
              } else if (grepl("acomp", names(extract[[1]][i]))) {
                namesto = "age"
              } else {
                # Default
                namesto = "age"
              }
              df2 <- df |>
                tidyr::pivot_longer(
                  cols = -year,
                  names_to = namesto,
                  values_to = "estimate") |>
                dplyr::mutate(module_name = names(extract),
                              label = names(extract[[1]][i]),
                              # label_init = names(extract[[1]][i]),
                              fleet = dplyr::case_when(
                                grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                                TRUE ~ NA), # stringr::str_extract(module_name, "(?<=\\.)\\w+(?=\\.)"),
                              label = dplyr::case_when(is.na(fleet) ~ names(extract[[1]][i]),
                                                       TRUE ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), "")))# stringr::str_replace(module_name, "\\.[^.]+\\.", "."))
              df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
              extract_list[[names(extract[[1]][i])]] <- df2
            } # close if statement
          } # close for loop
          new_df <- Reduce(rbind, extract_list)
          out_list[[names(extract)]] <- new_df
        } else if(any(sapply(extract[[1]], is.vector))){ # all must be a vector to work - so there must be conditions for dfs with a mix
          df <- data.frame(extract[[1]])
          if (length(intersect(colnames(df), c(factors, errors))) > 0) {
            df2 <- df |>
              tidyr::pivot_longer(
                cols = -intersect(colnames(df), c(factors, errors)),
                names_to = "label",
                values_to = "estimate"
              ) |>
              dplyr::mutate(module_name = names(extract))
          } else {
            df2 <- df |>
              tidyr::pivot_longer(
                cols = everything(),
                names_to = "label",
                values_to = "estimate"
              ) |>
              dplyr::mutate(module_name = names(extract))
          }
          if (any(grepl(paste(fleet_names, collapse = "|"), unique(df2$label)))) {
            df2 <- df2 |>
              dplyr::mutate(fleet = dplyr::case_when(grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names,collapse="|")),
                                                     # grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(ex, paste(fleet_names,collapse="|")),
                                                     TRUE ~ NA),
                            # Number after fleet name is what? variable among df?
                            age = dplyr::case_when(grepl("[0-9]+$", label) & stringr::str_extract(label, "[0-9]+$") < 30 ~ stringr::str_extract(label, "[0-9]+$"),
                                                   TRUE ~ NA),
                            label = dplyr::case_when(stringr::str_extract(label, "[0-9]+$") == 0 ~ label,
                                                     stringr::str_extract(label, "[0-9]+$") < 30 ~ stringr::str_remove(label, "[0-9]+$"),
                                                     TRUE ~ label)
              )
          } else if (any(grepl("[0-9]$", unique(df2$label)))) {
            df2 <- df2 |>
              dplyr::mutate(fleet = NA,
                            # Number after fleet name is what? variable among df?
                            age = dplyr::case_when(grepl("[0-9]+$", label) & stringr::str_extract(label, "[0-9]+$") < 30 ~ stringr::str_extract(label, "[0-9]+$"),
                                                   TRUE ~ NA),
                            # label_init = label,
                            label = dplyr::case_when(stringr::str_extract(label, "[0-9]+$") == 0 ~ label,
                                                     stringr::str_extract(label, "[0-9]+$") < 30 ~ stringr::str_remove(label, "[0-9]+$"),
                                                     TRUE ~ label)
              )
          } else {
            df2 <- df2 |>
              dplyr::mutate(fleet = NA,
                            age = NA,
                            label = label,
                            module_name = names(extract))
          }
          df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
          out_list[[names(extract)]] <- df2
        } else {
          message("Not compatible.")
        }
      } else if (is.matrix(extract[[1]])) { # matrix only
        df <- as.data.frame(extract[[1]]) |>
          tibble::rownames_to_column(var = "year")
        if (grepl("length", names(extract))) {
          namesto = "len_bins"
        } else if (grepl("age", names(extract))) {
          namesto = "age"
        } else {
          # Default
          namesto = "age"
        }
        df2 <- df |>
          tidyr::pivot_longer(
            cols = -year,
            names_to = namesto,
            values_to = "estimate") |>
          dplyr::mutate(module_name = names(extract),
                        label = names(extract),
                        # label_init = names(extract[[1]][i]),
                        fleet = dplyr::case_when(
                          grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                          TRUE ~ NA), # stringr::str_extract(module_name, "(?<=\\.)\\w+(?=\\.)"),
                        label = dplyr::case_when(is.na(fleet) ~ names(extract),
                                                 TRUE ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), ""))) # stringr::str_replace(module_name, "\\.[^.]+\\.", "."))
        df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
        out_list[[names(extract)]] <- df2
      } else {
        warning(paste(names(extract), " not compatible.", sep = ""))
      } # close if statement
    } # close loop over objects listed in dat file

    # Combind DFs into one
    out_new <- Reduce(rbind, out_list)

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

