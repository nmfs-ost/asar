#' Convert Output
#'
#' Format stock assessment output files to a standardized format.
#'
#' @param output_file Assessment model output file (e.g., the
#'  Report.sso file for SS3, the rdat file for BAM, etc.)
#' @param outdir Directory of the assessment model output file.
#' @param model Assessment model used in evaluation ("ss3", "bam",
#'  "asap", "fims", "amak", "ms-java", "wham", "mas").
#' @param fleet_names Names of fleets in the assessment model as
#'  shortened in the output file. Required for converting BAM
#'  model output.
#' @param file_save TRUE/FALSE; Save the formatted object rather
#'  than calling the function and adding the formatted object to
#'  the global environment? Default is false.
#' @param savedir Directory to save the converted output file.
#' @param save_name Name of the converted output file (do not use
#'  spaces).
#'
#' @author Samantha Schiano
#'
#' @return A reformatted and standardized version of assessment model results
#'         for application in building a stock assessment reports and to easily
#'         adapt results among regional assessments. The resulting object is
#'         simply a transformed and machine readable version of a model output file.
#'         There are 2 options for adding data to the function. (1) Add the full
#'         path with the file name in output.file or (2) output.file is the file
#'         name and outdir is the path to the file without a trailing forward slash.
#'
#'
#' @export
#'
convert_output <- function(
    output_file = NULL,
    outdir = NULL,
    model = NULL,
    fleet_names = NULL,
    file_save = FALSE,
    savedir = NULL,
    save_name = "std_model_output") {
  #### out_new ####
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
    initial = NA,
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
  out_new <- out_new[-1, ]

  # pull together path
  if (!is.null(outdir)) {
    output_file <- paste(outdir, "/", output_file, sep = "")
  } else {
    output_file <- output_file
  }

  # Check if can locate output file
  if (!file.exists(output_file)) {
    stop("output file path is invalid.")
  }

  #### SS3 ####
  # Convert SS3 output Report.sso file
  if (model %in% c("ss3", "SS3")) {
    # read SS3 report file
    dat <- utils::read.table(
      file = output_file,
      col.names = 1:get_ncol(output_file),
      fill = TRUE,
      quote = "",
      colClasses = "character", # reads all data as characters
      nrows = -1,
      comment.char = "",
      blank.lines.skip = FALSE
    )
    # Check SS3 model version
    vers <- as.numeric(stringr::str_extract(dat[1, 1], "[0-9].[0-9][0-9]"))
    if (vers < 3.3) {
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
      # "SELEX_database",
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
    std <- c(
      "DERIVED_QUANTITIES",
      "MGparm_By_Year_after_adjustments",
      "CATCH",
      "SPAWN_RECRUIT",
      "TIME_SERIES",
      "DISCARD_OUTPUT",
      "INDEX_2",
      # "FIT_LEN_COMPS",
      # "FIT_AGE_COMPS",
      # "FIT_SIZE_COMPS",
      # "SELEX_database",
      # "Biology_at_age_in_endyr",
      # "Growth_Parameters",
      "Kobe_Plot"
    )
    std2 <- c("OVERALL_COMPS")
    cha <- c("Dynamic_Bzero")
    rand <- c( # "SPR_SERIES",
      # "selparm(Size)_By_Year_after_adjustments",
      # "selparm(Age)_By_Year_after_adjustments"
    )
    # info <- c("LIKELIHOOD")
    aa.al <- c(
      "BIOMASS_AT_AGE",
      "BIOMASS_AT_LENGTH",
      "DISCARD_AT_AGE",
      "CATCH_AT_AGE",
      "F_AT_AGE",
      "MEAN_SIZE_TIMESERIES",
      "NUMBERS_AT_AGE",
      "NUMBERS_AT_LENGTH",
      "AGE_SELEX",
      "LEN_SELEX"
    )
    # nn <- NA

    # Loop for all identified parameters to extract for plotting and use
    # Create list of parameters that were not found in the output file
    # 1,4,10,17,19,20,22,32,37
    factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "season", "time", "era", "subseas", "subseason", "platoon", "platoo", "growth_pattern", "gp")
    errors <- c("StdDev", "sd", "se", "SE", "cv", "CV")
    miss_parms <- c()
    out_list <- list()
    ### SS3 loop ####
    for (i in 1:length(param_names)) {
      # Processing data frame
      parm_sel <- param_names[i]
      if (parm_sel %in% c(std, std2, cha, rand, aa.al)) {
        message("Processing ", parm_sel)
        extract <- suppressMessages(SS3_extract_df(dat, parm_sel))
        if (!is.data.frame(extract)) {
          miss_parms <- c(miss_parms, parm_sel)
          message("Skipped ", parm_sel)
          next
        } else {
          ##### STD ####
          # 1,4,10,17,19,36
          if (parm_sel %in% std) {
            # remove first row - naming
            df1 <- extract[-1, ]
            # Find first row without NAs = headers
            # temp fix for catch df
            df2 <- df1[stats::complete.cases(df1), ]
            if (any(c("#") %in% df2[, 1])) {
              full_row <- which(apply(df1, 1, function(row) is.na(row) | row == " " | row == "-" | row == "#"))[1]
              df1 <- df1[-full_row[1], ]
              df1 <- Filter(function(x) !all(is.na(x)), df1)
              df2 <- df1[stats::complete.cases(df1), ]
            }
            # identify first row
            row <- df2[1, ]
            # make row the header names for first df
            colnames(df1) <- row
            # find row number that matches 'row'
            rownum <- prodlim::row.match(row, df1)
            # Subset data frame
            df3 <- df1[-c(1:rownum), ]
            colnames(df3) <- tolower(row)
            # Reformat data frame
            if (any(colnames(df3) %in% c("Yr", "yr"))) {
              df3 <- df3 |>
                dplyr::rename(year = yr)
            }
            if ("label" %in% colnames(df3)) {
              if (any(grepl("_[0-9]+$", df3$label))) {
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
                  values_to = "estimate"
                ) |> # , colnames(dplyr::select(df3, tidyselect::matches(errors)))
                dplyr::mutate(
                  area = dplyr::case_when(
                    grepl("?_area_[0-9]_?", label) ~ stringr::str_extract(label, "(?<=area_)[0-9]+"),
                    grepl("_[0-9]_", label) ~ stringr::str_extract(label, "(?<=_)[0-9]+"),
                    grepl(":_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]+"),
                    grepl(":_[0-9][0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9][0-9]+"),
                    TRUE ~ NA
                  ),
                  sex = dplyr::case_when(
                    grepl("_fem_", label) ~ "female",
                    grepl("_mal_", label) ~ "male",
                    grepl("_sx:1$", label) ~ "female",
                    grepl("_sx:2$", label) ~ "male",
                    grepl("_sx:1_", label) ~ "female",
                    grepl("_sx:2_", label) ~ "male",
                    TRUE ~ NA
                  ),
                  growth_pattern = dplyr::case_when(
                    grepl("_gp_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
                    grepl("_gp:[0-9]$", label) ~ stringr::str_extract(label, "(?<=:)[0-9]$"),
                    grepl("_gp:[0-9][0-9]$", label) ~ stringr::str_extract(label, "(?<=:)[0-9][0-9]$"),
                    TRUE ~ NA
                  ),
                  month = dplyr::case_when(
                    grepl("_month_[0-9]+$", label) ~ stringr::str_extract(label, "(?<=month_)[0-9]+$"),
                    TRUE ~ NA
                  )
                )

              if ("fleet" %in% colnames(df3)) {
                df4 <- df4 |>
                  dplyr::mutate(
                    fleet = dplyr::case_when(
                      "fleet" %in% colnames(df3) ~ fleet,
                      # grepl("):_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
                      # grepl("):_[0-9][0-9]+$", label) ~ stringr::str_extract(label, "(?<=_)[0-9][0-9]$"),
                      TRUE ~ NA
                    ),
                    label = stringr::str_extract(label, "^.*?(?=_\\d|_gp|_fem|_mal|_sx|:|$)")
                  )
              } else {
                df4 <- df4 |>
                  dplyr::mutate(
                    fleet = dplyr::case_when(
                      grepl("):_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
                      grepl("):_[0-9][0-9]+$", label) ~ stringr::str_extract(label, "(?<=_)[0-9][0-9]$"),
                      TRUE ~ NA
                    ),
                    label = dplyr::case_when(
                      grepl("?_month_[0-9]_?", label) ~ stringr::str_replace(label, "_?month_\\d?", ""),
                      grepl("?_area_[0-9]_?", label) ~ stringr::str_replace(label, "_?area_\\d?", ""),
                      TRUE ~ stringr::str_extract(label, "^.*?(?=_\\d|_gp|_fem|_mal|_sx|:|$)")
                    )
                  )
              }
            } else {
              warning("Data frame not compatible in ", parm_sel, ".")
            }
            if (any(colnames(df4) %in% c("value"))) df4 <- dplyr::rename(df4, estimate = value)

            # Check if error values are in the labels column and extract out
            if (any(sapply(errors, function(x) grepl(x, unique(df4$label))))) {
              err_names <- unique(df4$label)[grepl(paste(errors, collapse = "|"), unique(df4$label)) & !unique(df4$label) %in% errors]
              if (any(grepl("sel", err_names))) {
                df4 <- df4
              } else if (length(intersect(errors, colnames(df4))) == 1) {
                df4 <- df4[-grep(paste(errors, "_", collapse = "|", sep = ""), df4$label), ]
              } else if (parm_sel == "MGparm_By_Year_after_adjustments") { # this is too specific
                df4 <- df4
                message("Error values are present, but are unique to the data frame and not to a selected parameter.")
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
                if (length(err_names) > 1) {
                  warning("There are multiple reported error metrics.")
                  if (any(grepl(paste(err_names, collapse = "|"), colnames(df4)))) {
                    df4 <- df4 |>
                      dplyr::select(-tidyselect::all_of(c(err_names[2:length(err_names)])))
                  } else {
                    df4 <- df4 |>
                      dplyr::filter(!(label %in% err_names[2:length(err_names)]))
                  }
                  find_error_value <- function(column_names, to_match_vector) {
                    vals <- sapply(column_names, function(col_name) {
                      match <- sapply(to_match_vector, function(err) if (grepl(err, col_name)) err else NA)
                      stats::na.omit(match)[1]
                    })
                    # only unique values and those that intersect with values vector
                    intersect(unique(vals), to_match_vector)
                  }
                  if (any(grepl(paste(err_names, collapse = "|"), colnames(df4)))) {
                    err_name <- find_error_value(names(df4), errors)
                    if (length(err_name) > 1) {
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
              dplyr::mutate(
                module_name = parm_sel,
                label = dplyr::case_when(
                  label == "f" ~ "fishing_mortality",
                  TRUE ~ label
                )
              )

            if (any(colnames(df5) %in% errors)) {
              df5 <- df5 |>
                dplyr::mutate(uncertainty_label = intersect(names(df5), errors)) |>
                dplyr::rename(uncertainty = intersect(colnames(df5), errors))
              colnames(df5) <- tolower(names(df5))
            } else {
              df5 <- df5 |>
                dplyr::mutate(
                  uncertainty_label = NA,
                  uncertainty = NA
                )
              colnames(df5) <- tolower(names(df5))
            }
            # param_df <- df5
            # if (ncol(out_new) < ncol(df5)){
            #   warning(paste0("Transformed data frame for ", parm_sel, " has more columns than default."))
            # } else if (ncol(out_new) > ncol(df5)){
            #   warning(paste0("Transformed data frame for ", parm_sel, " has less columns than default."))
            # }
            if ("seas" %in% colnames(df5)) {
              df5 <- df5 |>
                dplyr::rename(season = seas)
            }
            if ("subseas" %in% colnames(df5)) df5 <- dplyr::rename(df5, subseason = subseas)
            df5[setdiff(tolower(names(out_new)), tolower(names(df5)))] <- NA
            if (ncol(out_new) < ncol(df5)) {
              diff <- setdiff(names(df5), names(out_new))
              message("FACTORS REMOVED: ", parm_sel, " - ", paste(diff, collapse = ", "))
              # warning(parm_sel, " has more columns than the output data frame. The column(s) ", paste(diff, collapse = ", ")," are not found in the standard file. It was excluded from the resulting output. Please open an issue for developer fix.")
              df5 <- dplyr::select(df5, -tidyselect::all_of(c(diff)))
              out_list[[parm_sel]] <- df5
            } else {
              out_list[[parm_sel]] <- df5
            }
            #### STD2 ####
          } else if (parm_sel %in% std2) {
            # 4, 8
            # remove first row - naming
            df1 <- extract[-1, ]
            # check for age and length comps
            rows <- df1 |> dplyr::filter_all(dplyr::any_vars(. %in% c("age_bins", "len_bins")))
            if (length(rows) > 1) {
              matchr <- prodlim::row.match(rows, df1)
              df1a <- df1[matchr[1]:matchr[2] - 1, ]
              df1a <- Filter(function(x) !all(is.na(x)), df1a)
              df1b <- df1[matchr[2]:nrow(df1), ]
              df1b <- Filter(function(x) !all(is.na(x)), df1b)
              df_list <- list(df1a, df1b)
            } else {
              # Find first row without NAs = headers
              df_sel <- df1[stats::complete.cases(df1), ]
              df_list <- list(df_sel)
            }
            comps_list <- list()
            for (i in 1:length(df_list)) {
              df2 <- df_list[[i]]
              # identify first row
              row <- tolower(df2[1, ])
              # make row the header names for first df
              colnames(df2) <- row
              df2 <- df2[-1, ]
              # Defining columns for the grouping
              if (any(grepl("len_bins", colnames(df2)))) {
                std2_id <- "len_bins"
              } else if (any(grepl("age_bins", colnames(df2)))) {
                std2_id <- "age_bins"
              } else {
                std2_id <- "fleet"
              }
              # pivot data
              if (any(grepl(":", df2[max(nrow(df2)), ]))) {
                df2 <- df2[-max(nrow(df2)), ]
              } else {
                df2 <- df2
              }
              df3 <- df2 |>
                tidyr::pivot_longer(
                  cols = -intersect(c(factors, errors, std2_id, "n_obs"), colnames(df2)),
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
              if (any(duplicated(tolower(names(df3))))) {
                repd_name <- names(which(table(tolower(names(df3))) > 1))
                df3 <- df3 |>
                  dplyr::select(-tidyselect::all_of(repd_name))
                colnames(df3) <- tolower(names(df3))
              }
              if ("age_bins" %in% colnames(df3)) df3 <- dplyr::rename(df3, age = age_bins)
              if (nrow(df3) > 0) {
                df4 <- df3 |>
                  tidyr::pivot_longer(
                    cols = -intersect(c(factors, errors, std2_id, "n_obs"), colnames(df3)),
                    names_to = "label",
                    values_to = "estimate"
                  ) |>
                  dplyr::mutate(module_name = parm_sel)
              } else {
                df4 <- df3
              }
              if ("seas" %in% colnames(df4)) {
                df4 <- df4 |>
                  dplyr::rename(season = seas)
              }
              df4[setdiff(tolower(names(out_new)), tolower(names(df4)))] <- NA
              if (ncol(out_new) < ncol(df4)) {
                diff <- setdiff(names(df4), names(out_new))
                message("FACTORS REMOVED: ", parm_sel, " - ", paste(diff, collapse = ", "))
                # warning(parm_sel, " has more columns than the output data frame. The column(s) ", paste(diff, collapse = ", ")," are not found in the standard file. It was excluded from the resulting output. Please open an issue for developer fix.")
                df4 <- dplyr::select(df4, -tidyselect::all_of(diff))
              }
              comps_list[[i]] <- df4
            }
            # Add to new dataframe
            df5 <- Reduce(rbind, comps_list)
            out_list[[parm_sel]] <- df5

            ##### cha ####
          } else if (parm_sel %in% cha) {
            # Only one keyword characterized as this
            area_row <- which(apply(extract, 1, function(row) any(row == "Area:")))
            area_val <- c(t(extract[area_row, 3:ncol(extract)]))
            gp_row <- which(apply(extract, 1, function(row) any(row == "GP:")))
            gp_val <- c(t(extract[gp_row, 3:ncol(extract)]))
            df1 <- extract[-c(1:4), ]
            col_name_repl <- paste(parm_sel, "_", area_val, "_", gp_val, sep = "")
            colnames(df1) <- c("year", "era", col_name_repl)
            df2 <- df1 |>
              tidyr::pivot_longer(
                cols = -intersect(colnames(df1), c(factors, errors)),
                names_to = "label",
                values_to = "estimate"
              ) |>
              dplyr::mutate(
                area = stringr::str_extract(label, "(?<=_)[0-9]+"),
                growth_pattern = stringr::str_extract(label, "(?<=_)[0-9]+$"),
                label = stringr::str_extract(label, "^.*?(?=_[0-9]+)")
              )
            df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
            if (ncol(out_new) < ncol(df2)) {
              diff <- setdiff(names(df2), names(out_new))
              message("FACTORS REMOVED: ", parm_sel, " - ", paste(diff, collapse = ", "))
              df2 <- dplyr::select(df2, -tidyselect::all_of(diff))
              out_list[[parm_sel]] <- df2
            } else {
              out_list[[parm_sel]] <- df2
            }
            ##### rand ####
            # } else if (parm_sel %in% rand) {
            #   miss_parms <- c(miss_parms, parm_sel)
            #   next
            # } else if (parm_sel %in% info) {
            #   miss_parms <- c(miss_parms, parm_sel)
            #   next
            #### aa.al ####
          } else if (parm_sel %in% aa.al) {
            # 8,9,11,12,13,14,15,16,28,29
            # remove first row - naming
            df1 <- extract[-1, ]
            # Find first row without NAs = headers
            df2 <- df1[stats::complete.cases(df1), ]
            # identify first row
            row <- df2[1, ]
            if (any(row %in% "XX")) {
              loc_xx <- grep("XX", row)
              row <- row[row != "XX"]
              df1 <- df1[, -loc_xx]
            }
            # make row the header names for first df
            colnames(df1) <- row
            # find row number that matches 'row'
            rownum <- prodlim::row.match(row, df1)
            # Subset data frame
            df3 <- df1[-c(1:rownum), ]
            colnames(df3) <- tolower(row)

            # aa.al names
            naming <- c("biomass", "discard", "catch", "f", "mean_size", "numbers", "sel", "mean_body_wt", "natural_mortality")
            if (stringr::str_detect(tolower(parm_sel), paste(naming, collapse = "|"))) {
              label <- stringr::str_extract(tolower(parm_sel), paste(naming, collapse = "|"))
              if (length(label) > 1) warning("Length of label is > 1.")
              if (label == "f") {
                label <- "F"
              }
            }
            if (grepl("age", tolower(parm_sel))) {
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
            if (any(colnames(df3) %in% c("seas"))) {
              df3 <- df3 |>
                dplyr::rename(season = seas)
            }
            if (any(colnames(df3) %in% c("subseas"))) {
              df3 <- df3 |>
                dplyr::rename(subseason = subseas)
            }
            if ("label" %in% colnames(df3)) {
              df3 <- dplyr::select(df3, -tidyselect::any_of("label"))
            }
            # Change all columns to chatacters to a avoid issues in pivoting - this will be changed in final df anyway
            df3 <- df3 |>
              dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))
            # Pivot table long
            other_factors <- c("bio_pattern", "birthseas", "settlement", "morph", "beg/mid", "type", "label", "factor", "platoon", "month", "sexes", "part", "bin", "kind")
            df4 <- df3 |>
              tidyr::pivot_longer(
                cols = -intersect(c(factors, errors, other_factors), colnames(df3)),
                names_to = fac[1],
                values_to = "estimate"
              ) |>
              dplyr::mutate(
                label = label[1],
                module_name = parm_sel[1]
              )
            if (any(grepl("morph", colnames(df4)))) {
              df4 <- df4 |>
                dplyr::rename(growth_pattern = morph)
            }
            if ("label" %in% colnames(df4) & "factor" %in% colnames(df4)) {
              df4 <- df4 |>
                dplyr::select(-label) |>
                dplyr::mutate(label = dplyr::case_when(
                  grepl("sel", factor) ~ "sel",
                  TRUE ~ factor
                )) |>
                dplyr::select(-factor)
            }
            # Bind to new df
            df4[setdiff(tolower(names(out_new)), tolower(names(df4)))] <- NA
            if (ncol(out_new) < ncol(df4)) {
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
            message("Processing ", parm_sel)
            miss_parms <- c(miss_parms, parm_sel)
            next
          }
        } # close if param is in output file
      } else {
        message("Skipped ", parm_sel)
        next
      }
    } # close loop
    if (length(miss_parms) > 0) {
      message("Some parameters were not found or included in the output file. The inital release of this converter only inlcudes to most necessary parameters and values. The following parameters were not added into the new output file: \n", paste(miss_parms, collapse = "\n"))
    }
    out_new <- Reduce(rbind, out_list)

    #### BAM ####
  } else if (model %in% c("bam", "BAM")) {
    # check fleet names are input
    if (is.null(fleet_names)) {
      message("No fleet names were added as an argument. Fleets will not be extracted from the data.")
      fleet_names <- NA
    }
    # Extract values from BAM output - model file after following ADMB2R
    dat <- dget(output_file)
    # Create list for morphed dfs to go into (for rbind later)
    out_list <- list()

    factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "season", "time", "era", "subseas", "subseason", "platoon", "platoo", "growth_pattern", "gp")
    errors <- c("StdDev", "sd", "se", "SE", "cv", "CV")
    # argument for function when model == BAM
    # fleet_names <- c("cl", "cL","cp","mrip","ct", "hb", "HB", "comm","Mbft","CVID")

    # Extract data from list fit to output df
    # Loop over all items to output each object/transform
    # Not transforming or inclusing info chunk
    for (p in 2:length(dat)) {
      extract <- dat[p]
      # is the object class matrix, list, or vector
      if (is.vector(extract[[1]])) {
        if (is.list(extract[[1]])) { # indicates vector and list
          if (any(sapply(extract[[1]], is.matrix))) {
            extract_list <- list()
            for (i in 1:length(extract[[1]])) {
              if (is.vector(extract[[1]][[i]])) {
                df <- as.data.frame(extract[[1]][i]) |>
                  tibble::rownames_to_column(var = "age") |>
                  tidyr::pivot_longer(
                    cols = -age,
                    values_to = "estimate",
                    names_to = "label"
                  ) |>
                  dplyr::mutate(
                    label = names(extract[[1]][i]),
                    # label_init = names(extract[[1]][i]),
                    fleet = dplyr::case_when(
                      grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                      TRUE ~ NA
                    ),
                    module_name = names(extract),
                    label = dplyr::case_when(
                      grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), ""),
                      TRUE ~ label
                    )
                  )
                df[setdiff(tolower(names(out_new)), tolower(names(df)))] <- NA
                extract_list[[names(extract[[1]][i])]] <- df
              } else {
                df <- as.data.frame(extract[[1]][[i]]) |>
                  tibble::rownames_to_column(var = "year")
                if (grepl("lcomp", names(extract[[1]][i]))) {
                  namesto <- "len_bins"
                } else if (grepl("acomp", names(extract[[1]][i]))) {
                  namesto <- "age"
                } else {
                  # Default
                  namesto <- "age"
                }
                df2 <- df |>
                  tidyr::pivot_longer(
                    cols = -year,
                    names_to = namesto,
                    values_to = "estimate"
                  ) |>
                  dplyr::mutate(
                    module_name = names(extract),
                    label = names(extract[[1]][i]),
                    # label_init = names(extract[[1]][i]),
                    fleet = dplyr::case_when(
                      grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                      TRUE ~ NA
                    ), # stringr::str_extract(module_name, "(?<=\\.)\\w+(?=\\.)"),
                    label = dplyr::case_when(
                      is.na(fleet) ~ names(extract[[1]][i]),
                      TRUE ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), "")
                    )
                  ) # stringr::str_replace(module_name, "\\.[^.]+\\.", "."))
                df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
                extract_list[[names(extract[[1]][i])]] <- df2
              } # close if statement
            } # close for loop
            new_df <- Reduce(rbind, extract_list)
            out_list[[names(extract)]] <- new_df
          } else if (any(sapply(extract[[1]], is.vector))) { # all must be a vector to work - so there must be conditions for dfs with a mix
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
                  cols = tidyselect::everything(),
                  names_to = "label",
                  values_to = "estimate"
                ) |>
                dplyr::mutate(module_name = names(extract))
            }
            if (any(grepl(paste(fleet_names, collapse = "|"), unique(df2$label)))) {
              df2 <- df2 |>
                dplyr::mutate(
                  fleet = dplyr::case_when(
                    grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                    # grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(ex, paste(fleet_names,collapse="|")),
                    TRUE ~ NA
                  ),
                  # Number after fleet name is what? variable among df?
                  age = dplyr::case_when(
                    grepl(".Age[0-9]+.", label) ~ stringr::str_extract(label, "(?<=.Age?)[0-9]+"),
                    grepl("[0-9]+$", label) & as.numeric(stringr::str_extract(label, "[0-9]+$")) < 30 ~ stringr::str_extract(label, "[0-9]+$"),
                    TRUE ~ NA
                  ),
                  label = dplyr::case_when(
                    grepl(paste(".", fleet_names, "d[0-9]+", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, "d[0-9]+", sep = "", collapse = "|"), ".d"),
                    grepl(paste(".", fleet_names, "[0-9]+$", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, "[0-9]+", sep = "", collapse = "|"), ""),
                    grepl(paste(".", fleet_names, "$", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), ""),
                    grepl(".Age[0-9]+.[a-z]+", label) ~ stringr::str_replace(label, ".Age[0-9]+.[a-z]+", ""),
                    grepl("[0-9]+$", label) ~ stringr::str_replace(label, "[0-9]+$", ""),
                    # !is.na(fleet) | !is.na(age) ~ stringr::str_replace(label, paste(c(paste(".", fleet_names, "[0-9]+", sep = ""), ".Age[0-9]+.[a-z]+", "[0-9]+$"), collapse = "|"), ""),
                    # as.numeric(stringr::str_extract(label, "[0-9]+$")) == 0 ~ label,
                    # as.numeric(stringr::str_extract(label, "[0-9]+$")) < 30 ~ stringr::str_remove(label, "[0-9]+$"),
                    TRUE ~ label
                  )
                )
            } else if (any(grepl("[0-9]$", unique(df2$label)))) {
              df2 <- df2 |>
                dplyr::mutate(
                  fleet = NA,
                  # Number after fleet name is what? variable among df?
                  age = dplyr::case_when(
                    grepl("[0-9]+$", label) & stringr::str_extract(label, "[0-9]+$") < 30 ~ stringr::str_extract(label, "[0-9]+$"),
                    TRUE ~ NA
                  ),
                  # label_init = label,
                  label = dplyr::case_when(
                    as.numeric(stringr::str_extract(label, "[0-9]+$")) == 0 ~ label,
                    as.numeric(stringr::str_extract(label, "[0-9]+$")) < 30 ~ stringr::str_remove(label, "[0-9]+$"),
                    TRUE ~ label
                  )
                )
            } else {
              df2 <- df2 |>
                dplyr::mutate(
                  fleet = NA,
                  age = NA,
                  label = label,
                  module_name = names(extract)
                )
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
          if (any(grepl("age", colnames(df)))) {
            df <- df |>
              tidyr::pivot_longer(
                cols = -intersect(c(factors, errors), colnames(df)),
                names_to = "age",
                values_to = "estimate"
              ) |>
              dplyr::mutate(
                label = names(extract[[1]]),
                age = dplyr::case_when(
                  grepl(".[0-9]$", age) ~ stringr::str_extract(age, "[0-9]"),
                  grepl(".[0-9][0-9]$", age) ~ stringr::str_extract(age, "[0-9][0-9]"),
                  TRUE ~ NA
                ),
                module_name = names(extract)
              )
          }
          if (any(grepl(paste(fleet_names, collapse = "|"), df$label))) {
            df <- df |>
              dplyr::mutate(
                fleet = dplyr::case_when(
                  grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                  TRUE ~ NA
                ),
                label = dplyr::case_when(
                  is.na(fleet) ~ label,
                  TRUE ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), "")
                )
              )
          }
          df[setdiff(tolower(names(out_new)), tolower(names(df)))] <- NA
          out_list[[names(extract)]] <- df
        }
      } else if (is.list(extract[[1]])) { # list only
        if (any(sapply(extract[[1]], is.matrix))) {
          extract_list <- list()
          for (i in 1:length(extract[[1]])) {
            if (is.vector(extract[[1]][[i]])) {
              df <- as.data.frame(extract[[1]][i]) |>
                tibble::rownames_to_column(var = "age") |>
                tidyr::pivot_longer(
                  cols = -age,
                  values_to = "estimate",
                  names_to = "label"
                ) |>
                dplyr::mutate(
                  label = names(extract[[1]][i]),
                  # label_init = names(extract[[1]][i]),
                  fleet = dplyr::case_when(
                    grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                    TRUE ~ NA
                  ),
                  module_name = names(extract),
                  label = dplyr::case_when(
                    grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), ""),
                    TRUE ~ label
                  )
                )
              df[setdiff(tolower(names(out_new)), tolower(names(df)))] <- NA
              extract_list[[names(extract[[1]][i])]] <- df
            } else {
              df <- as.data.frame(extract[[1]][[i]]) |>
                tibble::rownames_to_column(var = "year")
              if (grepl("lcomp", names(extract[[1]][i]))) {
                namesto <- "len_bins"
              } else if (grepl("acomp", names(extract[[1]][i]))) {
                namesto <- "age"
              } else {
                # Default
                namesto <- "age"
              }
              df2 <- df |>
                tidyr::pivot_longer(
                  cols = -year,
                  names_to = namesto,
                  values_to = "estimate"
                ) |>
                dplyr::mutate(
                  module_name = names(extract),
                  label = names(extract[[1]][i]),
                  # label_init = names(extract[[1]][i]),
                  fleet = dplyr::case_when(
                    grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                    TRUE ~ NA
                  ), # stringr::str_extract(module_name, "(?<=\\.)\\w+(?=\\.)"),
                  label = dplyr::case_when(
                    is.na(fleet) ~ names(extract[[1]][i]),
                    TRUE ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), "")
                  )
                ) # stringr::str_replace(module_name, "\\.[^.]+\\.", "."))
              df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
              extract_list[[names(extract[[1]][i])]] <- df2
            } # close if statement
          } # close for loop
          new_df <- Reduce(rbind, extract_list)
          out_list[[names(extract)]] <- new_df
        } else if (any(sapply(extract[[1]], is.vector))) { # all must be a vector to work - so there must be conditions for dfs with a mix
          df <- data.frame(extract[[1]])
          if (max(as.numeric(row.names(df))) < 1800) {
            fac <- "age"
          } else {
            fac <- "year"
          }
          if (any(colnames(df) %in% c("age", "year"))) {
            df <- df
          } else {
            if (fac == "year") {
              df <- tibble::rowid_to_column(df, var = fac) |>
                dplyr::mutate(year = as.character(year))
            } else if (fac == "age") {
              df <- tibble::rowid_to_column(df, var = fac) |>
                dplyr::mutate(age = as.character(age))
            } else {
              warning("not compatible")
            }
          }
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
                cols = tidyselect::everything(),
                names_to = "label",
                values_to = "estimate"
              ) |>
              dplyr::mutate(module_name = names(extract))
          }
          if (any(grepl(paste(fleet_names, collapse = "|"), unique(df2$label)))) {
            if ("age" %in% colnames(df2)) {
              df2 <- df2
            } else {
              df2 <- df2 |>
                dplyr::mutate(age = NA)
            }
            df2 <- df2 |>
              dplyr::mutate(
                fleet = dplyr::case_when(
                  grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                  # grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(ex, paste(fleet_names,collapse="|")),
                  TRUE ~ NA
                ),
                # Number after fleet name is what? variable among df?
                age = dplyr::case_when(
                  is.na(age) & grepl("_age[0-9]+_", label) ~ stringr::str_extract(label, "(?<=age:?)[0-9]+"),
                  is.na(age) & grepl("[0-9]+$", label) ~ stringr::str_extract(label, "[0-9]+$"), # this is not age
                  TRUE ~ age
                ),
                # area = dplyr::case_when(is.na(age) & grepl("[0-9]+$", label) ~ stringr::str_extract(label, "[0-9]+$"), # this is not age
                #                         TRUE ~ NA),
                label = dplyr::case_when(
                  grepl("_age[0-9]_", label) & grepl(paste("_", fleet_names, sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste("_age[0-9]+_", fleet_names, sep = "", collapse = "|"), ""),
                  grepl("_age[0-9]_", label) ~ stringr::str_replace(label, "_age[0-9]+_", ""),
                  grepl(paste("_", fleet_names, sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste("_", fleet_names, sep = "", collapse = "|"), ""),
                  grepl(paste(".", fleet_names, "[0-9]+$", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, "[0-9]+$", sep = "", collapse = "|"), ""),
                  grepl(paste(".", fleet_names, "d[0-9]+$", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, "d[0-9]+$", sep = "", collapse = "|"), ""),
                  grepl(paste(".", fleet_names, sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), ""),
                  grepl(paste(fleet_names, "[0-9]+$", collapse = "|"), label) ~ stringr::str_replace(label, "[0-9]+$", ""),
                  TRUE ~ label
                )
              )
          } else if (any(grepl("[0-9]$", unique(df2$label)))) {
            df2 <- df2 |>
              dplyr::mutate(
                fleet = NA,
                # Number after fleet name is what? variable among df?
                age = dplyr::case_when(
                  grepl("[0-9]+$", label) & stringr::str_extract(label, "[0-9]+$") < 30 ~ stringr::str_extract(label, "[0-9]+$"),
                  TRUE ~ NA
                ),
                # label_init = label,
                label = dplyr::case_when(
                  as.numeric(stringr::str_extract(label, "[0-9]+$")) == 0 ~ label,
                  # issue here that stocks with ages > 29 will not be extracted - maybe add condition if F, t or other preceeeds the number then don't extract?
                  as.numeric(stringr::str_extract(label, "[0-9]+$")) < 30 ~ stringr::str_remove(label, "[0-9]+$"),
                  TRUE ~ label
                )
              )
          } else {
            df2 <- df2 |>
              dplyr::mutate(
                fleet = NA,
                age = NA,
                label = label,
                module_name = names(extract)
              )
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
          namesto <- "len_bins"
        } else if (grepl("age", names(extract))) {
          namesto <- "age"
        } else {
          # Default
          namesto <- "age"
        }
        df2 <- df |>
          tidyr::pivot_longer(
            cols = -year,
            names_to = namesto,
            values_to = "estimate"
          ) |>
          dplyr::mutate(
            module_name = names(extract),
            label = names(extract),
            # label_init = names(extract[[1]][i]),
            fleet = dplyr::case_when(
              grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
              TRUE ~ NA
            ), # stringr::str_extract(module_name, "(?<=\\.)\\w+(?=\\.)"),
            label = dplyr::case_when(
              is.na(fleet) ~ names(extract),
              TRUE ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), "")
            )
          ) # stringr::str_replace(module_name, "\\.[^.]+\\.", "."))
        df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
        out_list[[names(extract)]] <- df2
      } else {
        warning(paste(names(extract), " not compatible.", sep = ""))
      } # close if statement
    } # close loop over objects listed in dat file
    #### WHAM ####
  } else if (model == "wham") {
    # This is how Bai read the ASAP output
    # asap_output conversion written by Bai Li
    # asap_output <- dget(file.path(casedir, "output", subdir, paste("s", keep_sim_id[om_sim], sep = ""), "asap3.rdat"))
    # setwd(file.path(casedir, "output", subdir, paste("s", keep_sim_id[om_sim], sep = "")))
    # asap_std <- readRep("asap3", suffix = ".std")
    stop("Model not currently compatible.")

    #### AMAK ####
  } else if (model == "amak") {
    stop("Model not currently compatible.")
  } else {
    stop("Model not compatible.")
  }

  #### Exporting ####
  # Combind DFs into one
  out_new <- Reduce(rbind, out_list) # |>
  # dplyr::mutate(estimate = as.numeric(estimate),
  #               uncertainty = as.numeric(uncertainty),
  #               initial = as.numeric(initial))
  if (tolower(model) == "ss3") {
    con_file <- system.file("resources", "ss3_var_names.xlsx", package = "asar", mustWork = TRUE)
    var_names_sheet <- openxlsx::read.xlsx(con_file)
  } else if (tolower(model) == "bam") {
    con_file <- system.file("resources", "bam_var_names.xlsx", package = "asar", mustWork = TRUE)
    var_names_sheet <- openxlsx::read.xlsx(con_file)
  }
  if (file.exists(con_file)) {
    out_new <- dplyr::inner_join(out_new, var_names_sheet, by = "label") |>
      dplyr::mutate(label = dplyr::case_when(
        !is.na(alt_label) ~ alt_label,
        TRUE ~ label
      )) |>
      dplyr::select(-alt_label)
  }
  if (file_save) {
    save_path <- paste(savedir, "/", save_name, ".csv", sep = "")
    utils::write.csv(out_new, file = save_path, row.names = FALSE)
  } else {
    out_new
  }
} # close function
