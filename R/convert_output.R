#' Convert Output
#'
#' Format stock assessment output files to a standardized format.
#'
#' @param file Assessment model output file path
#' @param model Assessment model used in evaluation ("ss3", "bam",
#'  "asap", "fims", "amak", "ms-java", "wham", "mas").
#' @param fleet_names Names of fleets in the assessment model as
#'  shortened in the output file. If fleet names are not properly read, then
#'  indicate the fleets names as an acronym in a vector
#' @param save_dir File path to save the converted output file.
#'
#' @author Samantha Schiano
#'
#' @return A reformatted and standardized version of assessment model results
#'         for application in building a stock assessment reports and to easily
#'         adapt results among regional assessments. The resulting object is
#'         simply a transformed and machine readable version of a model output file.
#'         Converted data frame is always returned. It will also be saved if save_dir
#'         is not NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' convert_output(
#'   file = here::here("model1", "Report.sso"),
#'   model = "ss3",
#'   fleet_names = c("TWL", "NONTWL"),
#'   save_dir = here::here("standard_output.rda")
#' )
#' }
convert_output <- function(
  file,
  model = NULL,
  fleet_names = NULL,
  save_dir = NULL
) {
  # check if entered save_dir exists so doesn't waste user time finding this out at the end
  if (!is.null(save_dir)) {
    # if (!dir.exists(stringr::str_extract(save_dir, "^.*/(?=[^/]+\\.[^/]+$)") |> stringr::str_remove("/$"))) {
    #   cli::cli_abort("save_dir not a valid path.")
    # } else {
    #   # create new save_dir with file name
    #   save_dir <- file.path(save_dir, "std_output.rda")
    # }
    if (!grepl(".rda", save_dir)) {
      cli::cli_alert("save_dir does not contain a file name. Saved output will be named `std_output.rda`.")
      save_dir <- file.path(save_dir, "std_output.rda")
    }
  }

  #### out_new ####
  # Blank dataframe and set up to mold output into
  out_new <- data.frame(
    module_name = NA,
    label = NA,
    time = NA,
    era = NA,
    year = NA,
    month = NA,
    season = NA,
    subseason = NA,
    birthseas = NA,
    initial = NA,
    estimate = NA,
    # units = NA,
    uncertainty = NA,
    uncertainty_label = NA,
    likelihood = NA,
    fleet = NA,
    platoon = NA,
    area = NA,
    age = NA,
    sex = NA,
    growth_pattern = NA,
    # gradient = NA,
    # estimated = NA, # TRUE/FALSE
    # Additional factors from SS3
    bio_pattern = NA,
    settlement = NA,
    morph = NA,
    # beg/mid = NA, # need to identify df where this is applicable
    type = NA,
    factor = NA,
    # sexes = NA, # remove sexes to see how it impacts the results
    part = NA,
    kind = NA,
    nsim = NA,
    # TODO: add age_a and len_bins into just bins - add the age value in age
    bin = NA,
    age_a = NA,
    len_bins = NA,
    count = NA,
    block = NA
  )
  out_new <- out_new[-1, ]

  # Check if path links to a valid file
  url_pattern <- "^(https?|ftp|file):\\/\\/[-A-Za-z0-9+&@#\\/%?=~_|!:,.;]*[-A-Za-z0-9+&@#\\/%=~_|]$"
  if (grepl(url_pattern, file)) {
    check <- httr::HEAD(file)
    url <- httr::status_code(check)
    if (url == 404) cli::cli_abort(c(message = "Invalid URL."))
  } else {
    if (!file.exists(file)) {
      cli::cli_abort(c(
        message = "`file` not found.",
        "i" = "`file` entered as {file}"
      ))
    }
  }

  # Recognize model through file extension
  if (is.null(model)) {
    model <- switch(stringr::str_extract(file, "\\.([^.]+)$"),
      ".sso" = {
        cli::cli_alert_info("Processing Stock Synthesis output file...")
        "ss3"
      },
      ".rdat" = {
        cli::cli_alert_info("Processing BAM output file...")
        "bam"
      },
      ".rds" = {
        cli::cli_alert_info("Processing WHAM output file...")
        "wham"
      },
      ".RDS" = {
        cli::cli_alert_info("Processing FIMS output file...")
        "fims"
      },
      cli::cli_abort("Unknown file type. Please indicate model.")
    )
  }

  #### SS3 ####
  # Convert SS3 output Report.sso file
  if (tolower(model) == "ss3") {
    # read SS3 report file
    dat <- utils::read.table(
      file = file,
      col.names = 1:get_ncol(file),
      fill = TRUE,
      quote = "",
      colClasses = "character", # reads all data as characters
      nrows = -1,
      comment.char = "",
      blank.lines.skip = FALSE
    )
    # Check SS3 model version
    vers <- stringr::str_extract(dat[1, 1], "[0-9].[0-9][0-9].[0-9][0-9].[0-9]")
    if (vers < 3.3) {
      cli::cli_abort("This function in its current state can not process the data.")
    }

    # Extract fleet names
    if (is.null(fleet_names)) {
      fleet_names <- SS3_extract_fleet(dat, vers)
    }
    # Output fleet names in console
    cli::cli_alert_info("Identified fleet names:")
    cli::cli_alert_info("{fleet_names}")

    # Extract units


    # Estimated and focal parameters to put into reformatted output df - naming conventions from SS3
    # Extract keywords from ss3 file
    # Find row where keywords start
    keywords_start_row <- which(apply(dat, 1, function(row) any(grepl("#_KeyWords_of_tables_available_in_report_sso", row))))
    # Extract this first chunk of keywords to identify the first reported df - most likely DEFINITIONS
    first_blank_after <- which(apply(dat, 1, function(row) all(is.na(row) | row == "")) & (seq_len(nrow(dat)) > keywords_start_row))[1]
    rows <- c(keywords_start_row, first_blank_after)
    # Extract the metric using the rows from above as a guide and clean up empty columns
    keyword_1 <- dat[rows[1]:(rows[2] - 1), ] |>
      naniar::replace_with_na_all(condition = ~ .x == "")
    keyword_1 <- Filter(function(x) !all(is.na(x)), keyword_1)[-c(1:3), ]
    colnames(keyword_1) <- c("output", "keyword", "output_order")
    keyword_1 <- keyword_1 |>
      dplyr::mutate(output_order = as.numeric(stringr::str_extract(output_order, "\\d+$"))) |>
      dplyr::filter(output_order == 1) |>
      dplyr::pull(keyword)
    # identify first reported keyword
    keywords_end_row <- which(apply(dat, 1, function(row) any(grepl(keyword_1, row))))[2] - 12 # 12 rows behind is the last entry for keywords
    # always extract the second entry bc the first is just in the list of keywords

    keywords <- dat[keywords_start_row:keywords_end_row, ][-c(1:3), c(1:3)] |>
      naniar::replace_with_na_all(condition = ~ .x == "")
    keywords <- Filter(function(x) !all(is.na(x)), keywords)
    colnames(keywords) <- c("output", "keyword", "output_order")
    param_names <- keywords |>
      dplyr::mutate(output_order = as.numeric(stringr::str_extract(output_order, "\\d+$"))) |>
      dplyr::filter(output == "Y") |>
      dplyr::pull(keyword)

    # Group parameters base on table pattern
    std <- c(
      "DERIVED_QUANTITIES",
      "MGparm_By_Year_after_adjustments",
      "CATCH",
      "SPAWN_RECRUIT",
      "TIME_SERIES",
      "DISCARD_OUTPUT",
      "INDEX_2",
      "FIT_LEN_COMPS",
      "FIT_AGE_COMPS",
      "FIT_SIZE_COMPS",
      "SELEX_database",
      "Growth_Parameters",
      "Kobe_Plot"
    )
    std2 <- c("OVERALL_COMPS")
    cha <- c("Dynamic_Bzero")
    rand <- c(
      "Input_Variance_Adjustment",
      "SPR_SERIES",
      "selparm(Size)_By_Year_after_adjustments",
      "selparm(Age)_By_Year_after_adjustments",
      "BIOLOGY",
      "SPR/YPR_Profile",
      "Biology_at_age_in_endyr",
      "SPAWN_RECR_CURVE",
      "PARAMETERS"
    )
    info <- c(
      "LIKELIHOOD",
      "DEFINITIONS"
    )
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
      "LEN_SELEX",
      "MEAN_BODY_WT(Begin)",
      "Natural_Mortality"
    )
    nn <- c(
      "MORPH_INDEXING",
      "EXPLOITATION",
      "DISCARD_SPECIFICATION",
      "INDEX_1",
      "MEAN_BODY_WT_OUTPUT"
    )

    # Loop for all identified parameters to extract for plotting and use
    # Create list of parameters that were not found in the output file
    # 1,4,10,17,19,20,22,32,37
    factors <- c(
      "era", "year", "fleet",
      "fleet_name", "age", "sex",
      "area", "seas", "season",
      "time", "era", "subseas",
      "subseason", "platoon", "platoo",
      "growth_pattern", "gp", "month",
      "like", "morph", "bio_pattern",
      "settlement", "birthseas", "count",
      "kind"
    )

    errors <- c(
      "StdDev", "sd", "std", "stddev",
      "se", "SE",
      "cv", "CV"
    )

    miss_parms <- c()
    out_list <- list()
    #### SS3 loop ####
    for (i in seq_along(param_names)) {
      # Processing data frame
      parm_sel <- param_names[i]
      if (parm_sel %in% c(std, std2, cha, rand, aa.al)) {
        cli::cli_alert(glue::glue("Processing {parm_sel}"))
        extract <- SS3_extract_df(dat, parm_sel)
        if (!is.data.frame(extract)) {
          miss_parms <- c(miss_parms, parm_sel)
          cli::cli_alert(glue::glue("Skipped {parm_sel}"))
          next
        } else {
          ##### STD ####
          # 1,4,10,17,19,36
          if (parm_sel %in% std) {
            # remove first row - naming
            df1 <- extract[-1, ]
            # Find first row without NAs = headers
            # temp fix for catch df - I have only seen this issue for Hake example
            if (any(is.na(df1[1, ])) & parm_sel == "CATCH") {
              df1 <- df1[-1, ]
              cols_to_keep <- which(sapply(df1, function(x) !all(is.na(x))))
              df1 <- df1 |> dplyr::select(dplyr::all_of(c(names(cols_to_keep))))
              df2 <- df1
            } else {
              df2 <- df1[stats::complete.cases(df1), ]
            }
            if (any(c("#") %in% df2[, 1])) {
              full_row <- which(apply(df1, 1, function(row) is.na(row) | row == " " | row == "-" | row == "#"))[1]
              df1 <- df1[-full_row[1], ]
              df1 <- Filter(function(x) !all(is.na(x)), df1)
              df2 <- df1[stats::complete.cases(df1), ]
            }
            # identify first row
            row <- df2[1, ]
            # find row number that matches 'row'
            rownum <- prodlim::row.match(as.vector(row), df1)
            # make row the header names for first df
            colnames(df1) <- row
            # Subset data frame
            df3 <- df1[-c(1:rownum), ]
            colnames(df3) <- tolower(row)
            # Remove any leftover NA columns if still present
            NA_cols <- which(sapply(df3, function(x) all(is.na(x))))
            if (length(NA_cols) > 0) df3 <- df3[, -NA_cols]
            # Remove suprper + use from df
            if (any(grepl("suprper|use", colnames(df3)))) {
              df3 <- df3 |>
                dplyr::select(-tidyselect::any_of(c("suprper", "use")))
            }
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

              if ("sexes" %in% colnames(df3)) {
                df3 <- df3 |>
                  # add in case if sexes is present and add sex as na if so
                  dplyr::mutate(
                    sex = dplyr::case_when(
                      any(grepl("^sexes$", colnames(df3))) ~ sexes,
                      TRUE ~ NA
                    )
                  ) |>
                  dplyr::select(-sexes)
              } else {
                df3 <- dplyr::mutate(df3, sex = NA)
              }

              df4 <- df3 |>
                tidyr::pivot_longer(
                  !tidyselect::any_of(c(factors, errors)),
                  names_to = "label",
                  values_to = "estimate"
                ) |> # , colnames(dplyr::select(df3, tidyselect::matches(errors)))
                dplyr::mutate(
                  fleet = if ("fleet" %notin% colnames(df3)) {
                    dplyr::case_when(
                      # "fleet" %in% colnames(.data) ~ fleet,
                      grepl(":_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]+"),
                      grepl(":_[0-9][0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9][0-9]+"),
                      TRUE ~ NA
                    )
                  } else {
                    fleet
                  },
                  area = if ("area" %notin% colnames(df3)) {
                    dplyr::case_when(
                      grepl("?_area_[0-9]_?", label) ~ stringr::str_extract(label, "(?<=area_)[0-9]+"),
                      grepl("_[0-9]_", label) ~ stringr::str_extract(label, "(?<=_)[0-9]+"),
                      grepl(":_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]+"),
                      grepl(":_[0-9][0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9][0-9]+"),
                      TRUE ~ NA
                    )
                  } else {
                    area
                  },
                  sex = if ("sex" %notin% colnames(df3)) {
                    dplyr::case_when(
                      grepl("_fem_", label) ~ "female",
                      grepl("_mal_", label) ~ "male",
                      grepl("_sx:1$", label) ~ "female",
                      grepl("_sx:2$", label) ~ "male",
                      grepl("_sx:1_", label) ~ "female",
                      grepl("_sx:2_", label) ~ "male",
                      TRUE ~ NA
                    )
                  } else {
                    dplyr::case_when(
                      sex == 1 ~ "female",
                      sex == 2 ~ "male",
                      sex == 3 ~ "both",
                      TRUE ~ sex
                    )
                  },
                  growth_pattern = dplyr::case_when(
                    grepl("_gp_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
                    grepl("_gp:[0-9]$", label) ~ stringr::str_extract(label, "(?<=:)[0-9]$"),
                    grepl("_gp:[0-9][0-9]$", label) ~ stringr::str_extract(label, "(?<=:)[0-9][0-9]$"),
                    TRUE ~ NA
                  ),
                  month = dplyr::case_when(
                    grepl("_month_[0-9]+$", label) ~ stringr::str_extract(label, "(?<=month_)[0-9]+$"),
                    TRUE ~ ifelse(any(grepl("^month$", colnames(df3))), month, NA) # this might remove month
                  )
                )

              # if ("fleet" %in% colnames(df3)) {
              #   df4 <- df4 |>
              #     dplyr::mutate(
              #       fleet = dplyr::case_when(
              #         "fleet" %in% colnames(df3) ~ fleet,
              #         # grepl("):_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
              #         # grepl("):_[0-9][0-9]+$", label) ~ stringr::str_extract(label, "(?<=_)[0-9][0-9]$"),
              #         TRUE ~ NA
              #       ),
              #       label = stringr::str_extract(label, "^.*?(?=_\\d|_gp|_fem|_mal|_sx|:|$)")
              #     )
              # } else {
              df4 <- df4 |>
                dplyr::mutate(
                  # fleet = dplyr::case_when(
                  #   grepl("):_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
                  #   grepl("):_[0-9][0-9]+$", label) ~ stringr::str_extract(label, "(?<=_)[0-9][0-9]$"),
                  #   TRUE ~ NA
                  # ),
                  label = dplyr::case_when(
                    grepl("?_month_[0-9]_?", label) ~ stringr::str_replace(label, "_?month_\\d?", ""),
                    grepl("?_area_[0-9]_?", label) ~ stringr::str_replace(label, "_?area_\\d?", ""),
                    TRUE ~ stringr::str_extract(label, "^.*?(?=_\\d|_gp|_fem|_mal|_sx|:|$)")
                  )
                )
              # }
            } else {
              cli::cli_alert_warning(glue::glue("Data frame not compatible in {parm_sel}."))
            }
            if (any(colnames(df4) %in% c("value"))) df4 <- dplyr::rename(df4, estimate = value)

            # Check if error values are in the labels column and extract out
            if (any(sapply(paste0("(^|[_.])", errors, "($|[_.])"), function(x) grepl(x, unique(df4$label))))) {
              err_names <- unique(df4$label)[grepl(paste(paste0("(^|[_.])", errors, "($|[_.])"), collapse = "|"), unique(df4$label)) & !unique(df4$label) %in% errors]
              if (any(grepl("sel", err_names))) {
                df4 <- df4
              } else if (length(intersect(errors, colnames(df4))) == 1) {
                df4 <- df4[-grep(paste(errors, "_", collapse = "|", sep = ""), df4$label), ]
              } else if (parm_sel == "MGparm_By_Year_after_adjustments") { # this is too specific
                df4 <- df4
                cli::cli_alert_info("Error values are present, but are unique to the data frame and not to a selected parameter.")
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
                  # warning("There are multiple reported error metrics.")
                  if (any(grepl(paste(err_names, collapse = "|"), colnames(df4)))) {
                    df4 <- df4 |>
                      dplyr::select(-tidyselect::all_of(err_names[2:length(err_names)]))
                    cli::cli_alert_info(
                      glue::glue("Multiple error metrics reported in {parm_sel}.")
                    )
                    cli::cli_alert_info(
                      glue::glue("Error label(s) removed: \n {paste(err_names[-1], sep = '\n')}")
                    )
                  } else {
                    df4 <- df4 |>
                      dplyr::filter(!(label %in% err_names[2:length(err_names)]))
                  }
                  # Find overlapping error values if still present
                  find_error_value <- function(column_names, to_match_vector) {
                    vals <- vapply(column_names, function(col_name) {
                      match <- vapply(to_match_vector, function(err) {
                        pattern <- paste0("(^|[_.])", err, "($|[_.])")
                        if (grepl(pattern, col_name)) err else NA_character_
                      }, FUN.VALUE = character(1))
                      stats::na.omit(match)[1]
                    }, FUN.VALUE = character(1))
                    # }
                    # only unique values and those that intersect with values vector
                    intersect(unique(vals), to_match_vector)
                  }
                  # SS: I am not entirely sure what this step is doing, but is a good check
                  if (any(grepl(paste(err_names, collapse = "|"), colnames(df4)))) {
                    err_name <- find_error_value(names(df4), errors)
                    if (length(err_name) > 1) {
                      err_name <- stringr::str_extract(err_names[1], paste(errors, collapse = "|"))
                      # cli::cli_alert_info(
                      #   glue::glue("Multiple error metrics reported in {parm_sel}. Error label(s) removed: {err_names[-1]}"
                      #   )
                      # )
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
            if ("seas" %in% colnames(df5)) df5 <- dplyr::rename(df5, season = seas)

            if ("subseas" %in% colnames(df5)) df5 <- dplyr::rename(df5, subseason = subseas)

            if ("like" %in% colnames(df5)) df5 <- dplyr::rename(df5, likelihood = like)

            df5[setdiff(tolower(names(out_new)), tolower(names(df5)))] <- NA
            if (ncol(out_new) < ncol(df5)) {
              diff <- setdiff(names(df5), names(out_new))
              cli::cli_alert_info(paste0("FACTORS REMOVED: ", parm_sel, " - ", paste(diff, collapse = ", ")))
              # warning(parm_sel, " has more columns than the output data frame. The column(s) ", paste(diff, collapse = ", ")," are not found in the standard file. It was excluded from the resulting output. Please open an issue for developer fix.")
              df5 <- dplyr::select(df5, -tidyselect::all_of(c(diff)))
              out_list[[parm_sel]] <- df5
            } else {
              out_list[[parm_sel]] <- df5
            }
            ##### STD2 ####
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
            for (i in seq_along(df_list)) {
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
              df4 <- dplyr::mutate(df4, module_name = parm_sel)
              df4[setdiff(tolower(names(out_new)), tolower(names(df4)))] <- NA
              if (ncol(out_new) < ncol(df4)) {
                diff <- setdiff(names(df4), names(out_new))
                cli::cli_alert_info(paste0("FACTORS REMOVED: ", parm_sel, " - ", paste(diff, collapse = ", ")))
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
                label = stringr::str_extract(label, "^.*?(?=_[0-9]+)"),
                module_name = parm_sel
              )
            df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
            if (ncol(out_new) < ncol(df2)) {
              diff <- setdiff(names(df2), names(out_new))
              cli::cli_alert_info(paste0("FACTORS REMOVED: ", parm_sel, " - ", paste(diff, collapse = ", ")))
              df2 <- dplyr::select(df2, -tidyselect::all_of(diff))
              out_list[[parm_sel]] <- df2
            } else {
              out_list[[parm_sel]] <- df2
            }
            ##### rand ####
          } else if (parm_sel %in% rand) {
            # KEYWORDS in RAND
            # "Input_Variance_Adjustment"
            # "SPR_SERIES"
            # "selparm(Size)_By_Year_after_adjustments"
            # "selparm(Age)_By_Year_after_adjustments"
            # "BIOLOGY"
            # "SPR/YPR_Profile"
            # "SPAWN_RECR_CURVE"
            # "Biology_at_age_in_endyr"
            # "PARAMETERS"

            if (parm_sel == "SPAWN_RECR_CURVE") {
              # 32
              # TODO: add this to converter
              # set labels to "fitted_line_x"
              # remove first row - naming
              # df1 <- extract[-1, ]
              # # Find first row without NAs = headers
              # df2 <- df1[stats::complete.cases(df1), ]
              # # identify first row
              # row <- df2[1, ]
              # # make row the header names for first df
              # colnames(df1) <- row
              # # find row number that matches 'row'
              # rownum <- prodlim::row.match(row, df1)
              # # Subset data frame
              # df3 <- df1[-c(1:rownum), ]
              # colnames(df3) <- tolower(row)
              # # add year and manipulate df
              # # Extract first year of recruitment
              # fyr_row <- which(apply(dat, 1, function(row) any(row == "Start_year:")))
              # first_year <- dat[fyr_row,2]
              # # Extract last year
              # endyr_row <- which(apply(dat, 1, function(row) any(row == "End_year:")))
              # end_year <- dat[endyr_row,2]
              # df4 <- df3 |>
              #   dplyr::mutate(
              #     year = seq(first_year, end_year, by = 1),
              #   )
              # skipping for now cuz not needed
              miss_parms <- c(miss_parms, parm_sel)
              next
            } else if (parm_sel == "SPR_SERIES") {
              # split into the 3 dfs then stack - make sure all factors are present
              # remove first row - naming
              df1 <- extract[-1, ]
              # Find first row without NAs = headers
              df2 <- df1[stats::complete.cases(df1), ]
              # identify first row
              row <- df2[1, ]
              # make row the header names for first df
              colnames(df1) <- row
              # find row number that matches 'row'
              rownum <- prodlim::row.match(row, df1)
              # Subset data frame
              df3 <- df1[-(1:rownum), ]
              colnames(df3) <- tolower(row)
              # check if there are estimate and acual values within the df
              if (any(grepl("actual", colnames(df3)) | grepl("more_f", colnames(df3)))) {
                actual_col <- grep("actual", colnames(df3))
                moref_col <- grep("more_f", colnames(df3))
                # Separate out parts of the dataframe
                sub_df1 <- df3[, 1:(actual_col - 1)] |>
                  tidyr::pivot_longer(
                    cols = -c(yr, era),
                    names_to = "label",
                    values_to = "estimate"
                  ) |>
                  dplyr::rename(year = yr) |>
                  dplyr::mutate(
                    label = dplyr::case_when(
                      label == "bio_all" ~ "biomass",
                      label == "bio_smry" ~ "biomass_midyear",
                      label == "ssbzero" ~ "spawning_biomass_zero",
                      # label == "ssbfished" ~ "spawning_biomass",
                      label == "ssbfished/r" ~ "ssbfished_r",
                      TRUE ~ label
                    ),
                    # label = paste("estimate_", label, sep = ""),
                    morph = NA
                  )

                sub_df2 <- df3[, c(1:2, (actual_col + 1):(moref_col - 1))] |>
                  tidyr::pivot_longer(
                    cols = -c(yr, era),
                    names_to = "label",
                    values_to = "initial"
                  ) |>
                  dplyr::rename(year = yr) |>
                  dplyr::mutate(
                    label = dplyr::case_when(
                      label == "bio_all" ~ "biomass",
                      label == "bio_smry" ~ "biomass_midyear",
                      label == "num_smry" ~ "abundance_midyear",
                      label == "dead_catch" ~ "total_catch", # dead + retained
                      label == "retain_catch" ~ "landings",
                      label == "ssb" ~ "spawning_biomass",
                      label == "recruits" ~ "recruitment",
                      TRUE ~ label
                    ),
                    # change so that the values are placed in the column "initial" then combine w/ estimates
                    # label = paste("actual_", label, sep = ""),
                    morph = NA
                  )
                # combine above 2 since they show the estimate and actual values - matching
                sub_df12 <- dplyr::full_join(
                  sub_df1,
                  sub_df2,
                  by = c("year", "era", "label", "morph")
                )
                # extract last part of df
                sub_df3 <- df3[, c(1:2, (moref_col + 1):ncol(df3))] |>
                  tidyr::pivot_longer(
                    cols = -c(yr, era),
                    names_to = "label",
                    values_to = "estimate"
                  ) |>
                  dplyr::rename(year = yr) |>
                  dplyr::mutate(
                    morph = dplyr::case_when(
                      grepl("avef_|maxf_", label) ~ stringr::str_extract(label, "[0-9]+$"),
                      TRUE ~ NA_character_
                    ),
                    label = dplyr::case_when(
                      grepl("avef_", label) ~ stringr::str_remove(label, "_[0-9]+"),
                      grepl("maxf_", label) ~ stringr::str_remove(label, "_[0-9]+"),
                      label == "f=z-m" ~ "fishing_mortality",
                      TRUE ~ label
                    ),
                    initial = NA
                  )
                # combine all subdataframes
                df4 <- rbind(sub_df12, sub_df3) |>
                  dplyr::mutate(module_name = parm_sel)
              } else {
                df4 <- df3 |>
                  tidyr::pivot_longer(
                    cols = -c(intersect(colnames(df3), c("Yr", "yr", "era"))),
                    names_to = "label",
                    values_to = "estimate"
                  ) |>
                  dplyr::rename(year = intersect(colnames(df3), c("Yr", "yr", "Year"))) |>
                  dplyr::mutate(
                    label = dplyr::case_when(
                      grepl("bio_all", label) ~ "biomass",
                      grepl("bio_smry", label) ~ "biomass_midyear",
                      label == "ssbzero" ~ "spawning_biomass_zero",
                      # label == "ssbfished" ~ "spawning_biomass",
                      grepl("SSB_unfished", label) ~ "SSB_unfished",
                      grepl("SSBfished_eq", label) ~ "SSB_fished",
                      label == "ssbfished/r" ~ "ssbfished_r",
                      TRUE ~ label
                    ),
                    # label = paste("estimate_", label, sep = ""),
                    morph = NA,
                    module_name = parm_sel
                  )
              }
              # match to out_new
              df4[setdiff(tolower(names(out_new)), tolower(names(df4)))] <- NA
              # Add to out list
              out_list[[parm_sel]] <- df4
            } else if (parm_sel == "selparm(Size)_By_Year_after_adjustments") {
              # TODO: revisit one day in group work
              # skipping this one because there are no headers
              miss_parms <- c(miss_parms, parm_sel)
              next
            } else if (parm_sel == "selparm(Age)_By_Year_after_adjustments") {
              # also skipping
              miss_parms <- c(miss_parms, parm_sel)
              next
            } else if (parm_sel == "BIOLOGY") {
              # not sure how this output is helpful
              miss_parms <- c(miss_parms, parm_sel)
              next
            } else if (parm_sel == "SPR/YPR_Profile") {
              miss_parms <- c(miss_parms, parm_sel)
              next
            } else if (parm_sel == "Biology_at_age_in_endyr") {
              miss_parms <- c(miss_parms, parm_sel)
              next
            } else if (parm_sel == "PARAMETERS") {
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
              # Pull out indexing variables and remove from labels
              df4 <- df3 |>
                dplyr::select(intersect(colnames(df3), c("label", "value", "init", "parm_stdev"))) |>
                dplyr::rename(
                  estimate = value,
                  initial = init,
                  uncertainty = parm_stdev
                ) |>
                dplyr::mutate(
                  uncertainty = dplyr::case_when(
                    uncertainty == "_" ~ NA,
                    TRUE ~ uncertainty
                  ),
                  uncertainty_label = dplyr::case_when(
                    is.na(uncertainty) ~ NA,
                    TRUE ~ "sd"
                  ),
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
                    grepl("Male", label) ~ "male",
                    grepl("Female", label) ~ "female",
                    TRUE ~ NA
                  ),
                  growth_pattern = dplyr::case_when(
                    grepl("_gp_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
                    grepl("_gp:[0-9]$", label) ~ stringr::str_extract(label, "(?<=:)[0-9]$"),
                    grepl("_gp:[0-9][0-9]$", label) ~ stringr::str_extract(label, "(?<=:)[0-9][0-9]$"),
                    grepl("_GP_[0-9]$", label) ~ stringr::str_extract(label, "(?<=_)[0-9]$"),
                    grepl("_GP:[0-9]$", label) ~ stringr::str_extract(label, "(?<=:)[0-9]$"),
                    grepl("_GP:[0-9][0-9]$", label) ~ stringr::str_extract(label, "(?<=:)[0-9][0-9]$"),
                    TRUE ~ NA
                  ),
                  month = dplyr::case_when(
                    grepl("_month_[0-9]+$", label) ~ stringr::str_extract(label, "(?<=month_)[0-9]+$"),
                    TRUE ~ ifelse(any(grepl("^month$", colnames(df3))), month, NA)
                  ),
                  age = dplyr::case_when(
                    grepl("InitAge", label) ~ stringr::str_extract(label, "(?<=InitAge_)[0-9]+"),
                    TRUE ~ NA
                  ),
                  year = dplyr::case_when(
                    grepl("RecrDev", label) ~ stringr::str_extract(label, "(?<=RecrDev_)[0-9]+$"),
                    grepl("_[0-9]{4}$", label) ~ stringr::str_extract(label, "[0-9]{4}$"),
                    TRUE ~ NA
                  ),
                  era = dplyr::case_when(
                    grepl("InitAge", label) ~ stringr::str_extract(label, "^.*?(?=_InitAge_[0-9]+$)"),
                    grepl("RecrDev", label) ~ stringr::str_extract(label, "^.*?(?=_RecrDev)"),
                    TRUE ~ NA
                  ),
                  fleet = dplyr::case_when(
                    grepl(
                      paste(
                        fleet_names,
                        collapse = "|"
                      ),
                      label
                    ) ~ stringr::str_extract(label, paste0("(^.*?_)?<=|", paste(fleet_names, collapse = "|"))),
                    TRUE ~ NA
                  ),
                  block = dplyr::case_when(
                    grepl("_BLK[0-9]repl_[0-9]{4}$", label) ~ stringr::str_extract(label, "(?<=_BLK)[0-9](?=repl_[0-9]{4}$)"),
                    TRUE ~ NA
                  ),
                  # Must be last step in mutate bc all info comes from the
                  label = dplyr::case_when(
                    grepl("?_month_[0-9]_?", label) ~ stringr::str_replace(label, "_?month_\\d?", ""),
                    grepl("?_area_[0-9]_?", label) ~ stringr::str_replace(label, "_?area_\\d?", ""),
                    grepl("InitAge", label) ~ "initial_age",
                    grepl("RecrDev", label) ~ "recruitment_deviations",
                    grepl(
                      paste0(
                        paste0(
                          "_",
                          fleet_names,
                          collapse = "|"
                        ),
                        "\\([0-9]+\\)_BLK[0-9]repl_[0-9]{4}$"
                      ),
                      label
                    ) ~ stringr::str_remove_all(label, "(Male_|Female_)|(_[[:alnum:]]+\\([0-9]+\\))|(_BLK[0-9]repl_[0-9]{4})"),
                    grepl("Male_|Female_", label) & grepl(paste0(paste0("_", fleet_names, collapse = "|"), "\\([0-9]+\\)"), label) ~ stringr::str_remove_all(label, "(Male_|Female_)|(_[[:alnum:]]+\\([0-9]+\\))"),
                    grepl(paste0(paste0("_", fleet_names, collapse = "|"), "\\([0-9]+\\)"), label) ~ stringr::str_remove(label, paste0(paste0("_", fleet_names, "\\([0-9]+\\)", collapse = "|"))), # , paste0("_", fleet_names, collapse = "|")
                    TRUE ~ stringr::str_extract(label, "^.*?(?=_\\d|_gp|_fem|_mal|_sx|:|$)")
                  ),
                  # fix remaining labels
                  label = ifelse(grepl("_GP$", label), stringr::str_remove(label, "_GP$"), label),
                  label = ifelse(grepl("_Fem|_Mal", label), stringr::str_remove(label, "_Fem$|_Mal$"), label),
                  module_name = parm_sel
                )
              # match to out_new
              df4[setdiff(tolower(names(out_new)), tolower(names(df4)))] <- NA
              # Add to out list
              out_list[[parm_sel]] <- df4
            } else {
              miss_parms <- c(miss_parms, parm_sel)
              next
            }
            #   miss_parms <- c(miss_parms, parm_sel)
            #   next
            #### info ####
          } else if (parm_sel %in% info) {
            if (parm_sel == "LIKELIHOOD") {
              df1 <- extract[-1, ]
              # make row the header names for first df
              colnames(df1) <- df1[1, ]
              # Remove first row containing column names
              df2 <- df1[-1, ] |>
                dplyr::rename(label = Component) |>
                tidyr::pivot_longer(
                  cols = -label,
                  names_to = "type",
                  values_to = "likelihood"
                )
              # match to out_new
              df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
              # Add to out list
              out_list[[parm_sel]] <- df2
            } else if (parm_sel == "DEFINITIONS") {
              # Pull out only the first two columns and remove first row keyword
              df1 <- extract[-1, 1:2]
              colnames(df1) <- c("label", "estimate")
              # find where rescale is located
              col_rescale <- grep("rescaled_to_sum_to:", extract)
              rescaled_months <- extract[4, row_rescale + 1] |> dplyr::pull()
              # remove : from all labels and tolower and add in rescaled months
              df2 <- df1 |>
                rbind(data.frame(
                  label = "rescaled_months",
                  estimate = rescaled_months
                )) |>
                dplyr::mutate(
                  label = tolower(stringr::str_remove_all(label, ":")),
                  module_name = parm_sel
                )
              # match to out_new
              df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
              # Add to out list
              out_list[[parm_sel]] <- df2
            } else {
              miss_parms <- c(miss_parms, parm_sel)
              next
            }
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
            naming <- c(
              "biomass", "discard", "catch",
              "f", "mean_size", "numbers", "sel",
              "mean_body_wt", "natural_mortality"
            )
            if (stringr::str_detect(tolower(parm_sel), paste(naming, collapse = "|"))) {
              label <- stringr::str_extract(tolower(parm_sel), paste(naming, collapse = "|"))
              if (length(label) > 1) cli::cli_alert_warning("Length of label is > 1.")
              if (label == "f") {
                label <- "fishing_mortality"
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
            # if ("label" %in% colnames(df3)) {
            #   df3 <- dplyr::select(df3, -tidyselect::any_of("label"))
            # }
            # If factor exists, set to label
            if ("factor" %in% colnames(df3)) {
              df3 <- df3 |>
                dplyr::select(-tidyselect::any_of("label")) |>
                dplyr::rename(label = factor)
            } else {
              df3 <- dplyr::mutate(df3, label = label[1])
            }
            # Change all columns to chatacters to a avoid issues in pivoting - this will be changed in final df anyway
            df3 <- df3 |>
              dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))
            # Set all columns after factors as numeric
            # Identify last factor col
            other_factors <- c(
              "bio_pattern", "birthseas",
              "settlement", "morph", "beg/mid",
              "type", "label", "label",
              "platoon", "month",
              "sexes", "part", "bin", "kind"
            )
            num_cols <- setdiff(colnames(df3), c(factors, other_factors))
            df3 <- df3 |>
              dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), as.numeric))
            # Pivot table long
            df4 <- df3 |>
              tidyr::pivot_longer(
                cols = -intersect(c(factors, errors, other_factors), colnames(df3)),
                names_to = fac[1],
                values_to = "estimate"
              ) |>
              dplyr::mutate(
                # label = label[1],
                module_name = parm_sel[1]
              )
            if ("sex" %in% colnames(df4)) {
              df4 <- df4 |>
                dplyr::mutate(
                  sex = dplyr::case_when(
                    "sex" %in% colnames(df3) & sex == 1 ~ "female",
                    "sex" %in% colnames(df3) & sex == 2 ~ "male"
                  )
                )
            }
            if (any(grepl("morph", colnames(df4)))) {
              df4 <- df4 |>
                dplyr::rename(growth_pattern = morph)
            }
            # Check if likelihood is in the df
            if (any(grepl("like", colnames(df4)))) {
              df4 <- df4 |>
                dplyr::rename(likelihood = like)
            }
            # Check for error in the df
            if (any(grepl(paste0("^", errors, "$", collapse = "|"), colnames(df4)))) {
              error_col <- colnames(df4)[grep(paste0("^", errors, "$", collapse = "|"), colnames(df4))]
              if (length(error_col) > 1) {
                cli::cli_alert("Multiple error columns present. Error will not be added to module.")
              } else {
                df4 <- df4 |>
                  dplyr::mutate(
                    uncertainty_label = tolower(unique(error_col)),
                    uncertainty = df4[[error_col]]
                  )
              }
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
              cli::cli_alert_info(paste0("FACTORS REMOVED: ", parm_sel, " - ", paste(diff, collapse = ", ")))
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
            cli::cli_alert_info(glue::glue("Processing {parm_sel}"))
            miss_parms <- c(miss_parms, parm_sel)
            next
          }
        } # close if param is in output file
      } else {
        cli::cli_alert(glue::glue("Skipped {parm_sel}"))
        next
      }
    } # close loop
    if (length(miss_parms) > 0) {
      cli::cli_alert_info(
        paste0("Some parameters were not found or included in the output file. The following parameters were not added into the new output file: \n", paste(miss_parms, collapse = "\n"))
      )
    }
    out_new <- Reduce(rbind, out_list) |>
      dplyr::mutate(fleet = fleet_names[fleet])
  } else if (model %in% c("bam", "BAM")) {
    #### BAM ####
    # Extract values from BAM output - model file after following ADMB2R
    dat <- dget(file)

    # Find fleet names
    if (is.null(fleet_names)) {
      # Extract names from indices
      indices <- dat$t.series |>
        dplyr::select(dplyr::contains("U.") & dplyr::contains(".ob"))
      fleets_ind <- stringr::str_extract(as.vector(colnames(indices)), "(?<=U\\.)\\w+(?=\\.ob)")
      # Extract names from landings
      landings <- dat$t.series |>
        dplyr::select(dplyr::contains("L.") & dplyr::contains(".ob") |
          dplyr::contains("D.") & dplyr::contains(".ob"))
      fleets_land <- stringr::str_extract(as.vector(colnames(landings)), "(?<=L\\.)\\w+(?=\\.ob)")
      fleets_disc <- stringr::str_extract(as.vector(colnames(landings)), "(?<=D\\.)\\w+(?=\\.ob)")
      # Extract names from lof F dev
      parm <- dat$parm.tvec |>
        dplyr::select(dplyr::contains("log.F.dev.") |
          dplyr::contains("log.F.dev.") & dplyr::contains(".D"))
      # fleets_parm_D <- stringr::str_extract(as.vector(colnames(parm)), "(?<=log\\.F\\.dev\\.)\\w+(?=\\.D)")
      fleets_parm <- stringr::str_extract(as.vector(colnames(parm)), "(?<=log\\.F\\.dev\\.)\\w+")
      fleets <- unique(tolower(c(fleets_ind, fleets_land, fleets_disc, fleets_parm)))
      fleet_names <- fleets[!is.na(fleets)]
      if (any(is.na(fleet_names))) {
        cli::cli_abort("No fleet names found in dataframe. Please indicate the abbreviations of fleet names using fleet_names arg.")
      }
    } # else {
    #   # check fleet names are input
    #   # if (any(is.na(fleet_names))) {
    #   fleet_names <- fleet_names
    # }
    # Output fleet names in console
    cli::cli_alert_info("Identified fleet names:")
    cli::cli_alert_info("{fleet_names}")
    # Create list for morphed dfs to go into (for rbind later)
    out_list <- list()

    factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "season", "time", "era", "subseas", "subseason", "platoon", "platoo", "growth_pattern", "gp", "nsim", "age_a")
    errors <- c("StdDev", "sd", "se", "SE", "cv", "CV", "stddev")
    units <- c("mt", "lbs", "eggs")
    # argument for function when model == BAM
    # fleet_names <- c("cl", "cL","cp","mrip","ct", "hb", "HB", "comm","Mbft","CVID")

    ##### BAM loop ----
    # Extract data from list fit to output df
    # Loop over all items to output each object/transform
    # Not transforming or inclusing info chunk
    for (p in 2:length(dat)) {
      extract <- dat[p]
      cli::cli_alert_info(glue::glue("Processing {names(extract)}"))
      # is the object class matrix, list, or vector
      if (is.vector(extract[[1]])) {
        if (is.list(extract[[1]])) { # indicates vector and list
          if (any(vapply(extract[[1]], is.matrix, FUN.VALUE = logical(1)))) {
            extract_list <- list()
            for (i in seq_along(extract[[1]])) {
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
                df <- df |>
                  dplyr::rename_with(
                    ~ ifelse(max(as.numeric(df$year)) < 50,
                      c("age_a"), c("year")
                    ),
                    year
                  )
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
                    cols = -intersect(colnames(df), c("year", "age_a")),
                    names_to = namesto,
                    values_to = "estimate"
                  ) |>
                  dplyr::mutate(
                    module_name = names(extract),
                    label = names(extract[[1]][i]),
                    fleet = dplyr::case_when(
                      grepl(paste(fleet_names, collapse = "|"), tolower(label)) ~ stringr::str_extract(tolower(label), paste(fleet_names, collapse = "|")),
                      TRUE ~ NA
                    ), # stringr::str_extract(module_name, "(?<=\\.)\\w+(?=\\.)"),
                    label = dplyr::case_when(
                      grepl(paste0(fleet_names, collapse = "|"), tolower(label)) ~ stringr::str_replace(tolower(label), paste0(".", fleet_names, collapse = "|"), ""),
                      TRUE ~ names(extract[[1]][i])
                    )
                    # label_init = names(extract[[1]][i]),

                    # label = dplyr::case_when(
                    #   is.na(fleet) ~ names(extract[[1]][i]),
                    #   TRUE ~ stringr::str_replace(tolower(label), paste(".", fleet_names, sep = "", collapse = "|"), "")
                    # )
                  ) # stringr::str_replace(module_name, "\\.[^.]+\\.", "."))
                df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
                extract_list[[names(extract[[1]][i])]] <- df2
              } # close if statement
            } # close for loop
            new_df <- Reduce(rbind, extract_list)
            out_list[[names(extract)]] <- new_df
          } else if (any(vapply(extract[[1]], is.vector, FUN.VALUE = logical(1)))) { # all must be a vector to work - so there must be conditions for dfs with a mix
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
                    module_name != "parms" & grepl(".Age[0-9]+.", label) ~ stringr::str_extract(label, "(?<=.Age?)[0-9]+"),
                    module_name != "parms" & grepl("[0-9]+$", label) & as.numeric(stringr::str_extract(label, "[0-9]+$")) < 30 ~ stringr::str_extract(label, "[0-9]+$"),
                    TRUE ~ NA
                  ),
                  label = dplyr::case_when(
                    # below will only work properly if there are age varying parameters without fleet names in it
                    module_name == "parms" & !grepl(paste(".", fleet_names, sep = "", collapse = "|"), tolower(label)) ~ label,
                    grepl(paste(".", fleet_names, "d[0-9]+", sep = "", collapse = "|"), tolower(label)) ~ stringr::str_replace(tolower(label), paste(".", fleet_names, "d[0-9]+", sep = "", collapse = "|"), ".d"),
                    grepl(paste(".", fleet_names, "[0-9]+$", sep = "", collapse = "|"), tolower(label)) ~ stringr::str_replace(tolower(label), paste(fleet_names, sep = "", collapse = "|"), ""), # "[0-9]+",
                    grepl(paste(".", fleet_names, "$", sep = "", collapse = "|"), tolower(label)) ~ stringr::str_replace(tolower(label), paste(".", fleet_names, sep = "", collapse = "|"), ""),
                    grepl(paste(".", fleet_names, ".d$", sep = "", collapse = "|"), tolower(label)) ~ stringr::str_replace(tolower(label), paste(".", fleet_names, sep = "", collapse = "|"), ""),
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
                  fleet = ifelse("age" %in% colnames(df2), fleet, NA),
                  age = ifelse("age" %in% colnames(df2), age, NA),
                  label = label,
                  module_name = names(extract)
                )
            }
            df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
            out_list[[names(extract)]] <- df2
          } else {
            cli::cli_alert_warning("Not compatible.")
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
        if (any(vapply(extract[[1]], is.matrix, FUN.VALUE = logical(1)))) {
          extract_list <- list()
          for (i in seq_along(extract[[1]])) {
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
          if (max(as.numeric(row.names(df))) < 1000) {
            fac <- "age"
          } else if (1000 < max(as.numeric(row.names(df))) & max(as.numeric(row.names(df))) < 2100) {
            fac <- "year"
          } else {
            fac <- "nsim"
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
            } else if (fac == "nsim") {
              df <- tibble::rowid_to_column(df, var = fac) |>
                dplyr::mutate(nsim = as.character(nsim))
            } else {
              cli::cli_alert_warning("Not compatible")
            }
          }
          # # Check if any col names contain error then select
          # if (any(grepl(paste0(errors, collapse = "|"), colnames(df)))) {
          #   error_cols <- grep(paste0(errors, ".", collapse = "|"), colnames(df))
          # } else {
          #   error_cols <- NULL
          # }
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
          if (any(grepl(paste(fleet_names, collapse = "|"), tolower(unique(df2$label))))) {
            if ("age" %in% colnames(df2)) {
              df2 <- df2
            } else {
              df2 <- df2 |>
                dplyr::mutate(age = NA)
            }
            df2 <- df2 |>
              dplyr::mutate(
                label = tolower(label),
                fleet = dplyr::case_when(
                  grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                  # grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(ex, paste(fleet_names,collapse="|")),
                  TRUE ~ NA
                ),
                # Number after fleet name is what? variable among df?
                age = dplyr::case_when(
                  is.na(as.numeric(age)) & grepl("_age[0-9]+_", label) ~ as.numeric(stringr::str_extract(label, "(?<=age:?)[0-9]+")),
                  is.na(as.numeric(age)) & grepl("[0-9]+$", label) ~ as.numeric(stringr::str_extract(label, "[0-9]+$")), # this is not age
                  TRUE ~ as.numeric(age)
                ),
                # area = dplyr::case_when(is.na(age) & grepl("[0-9]+$", label) ~ stringr::str_extract(label, "[0-9]+$"), # this is not age
                #                         TRUE ~ NA),
                # Uncomment when units gets added in
                # units = dplyr::case_when(
                #   grepl(paste(units, collapse = "|"), label) ~ stringr::str_extract(label, paste(units, collapse = "|")),
                #   TRUE ~ NA
                # ),
                label = dplyr::case_when(
                  # grepl("_age[0-9]_", label) & grepl(paste("_", fleet_names, "[0-9]+", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste("_age[0-9]+_", fleet_names, "[0-9]+", sep = "", collapse = "|"), ""),
                  # grepl("_age[0-9]_", label) & grepl(paste("_", fleet_names, sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste("_age[0-9]+_", fleet_names, sep = "", collapse = "|"), ""),
                  # grepl("_age[0-9]_", label) ~ stringr::str_replace(label, "_age[0-9]+_", ""),
                  # grepl(paste0(rep(fleet_names, times = length(units)), ".", units, collapse = "|"), label) ~ stringr::str_replace(label, paste0(".", rep(fleet_names, times = length(units)), ".", units, collapse = "|"), ""),
                  grepl(paste("_", fleet_names, "[0-9]+$", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste("_", fleet_names, "[0-9]+$", sep = "", collapse = "|"), ""),
                  grepl(paste("_", fleet_names, sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste("_", fleet_names, sep = "", collapse = "|"), ""),
                  grepl(paste(".", fleet_names, "[0-9]+$", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste(fleet_names, sep = "", collapse = "|"), ""),
                  grepl(paste(fleet_names, ".", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste(fleet_names, ".", sep = "", collapse = "|"), ""),
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
                fleet = ifelse(any(colnames(df2) %in% c("fleet")), fleet, NA),
                age = ifelse(any(colnames(df2) %in% c("age")), fleet, NA),
                label = label,
                module_name = names(extract)
              )
          }
          # Move error into column
          if (any(grepl(paste0("^", errors, "\\.", collapse = "|"), unique(df2$label)))) {
            # Pivot wider only the cv labels and align with their counter part estimate label
            error_data <- df2 |>
              dplyr::filter(stringr::str_detect(label, paste0("^", errors, ".", collapse = "|"))) |>
              dplyr::mutate(match_key = stringr::str_remove(label, paste0("^", errors, ".", collapse = "|"))) |>
              dplyr::mutate(uncertainty_label = stringr::str_extract(label, paste0(errors, collapse = "|"))) |>
              dplyr::select(year, fleet, match_key, uncertainty = estimate, uncertainty_label)
            # filter out error labels
            df2_filtered <- df2 |>
              dplyr::filter(!stringr::str_detect(label, paste0("^", errors, collapse = "|"))) |>
              dplyr::mutate(match_key = stringr::str_extract(label, "^[A-Za-z]+"))
            # Join the errors to the final df
            df2 <- df2_filtered |>
              dplyr::left_join(error_data, by = c("year", "fleet", "match_key")) |>
              dplyr::select(-match_key)
          }
          df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
          out_list[[names(extract)]] <- df2
        } else {
          cli::cli_alert_warning("Not compatible.")
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
        cli::cli_alert_warning(paste(names(extract), " not compatible.", sep = ""))
      } # close if statement
    } # close loop over objects listed in dat file

    # Place out_list into a single data frame
    # VIRG, INIT, TIME, FORE
    out_new <- Reduce(rbind, out_list) |>
      # Add era as factor into BAM conout
      dplyr::mutate(
        # TODO: replace all periods with underscore if naming convention is different
        label = tolower(label)
      )
    if (model == "bam") {
      out_new <- dplyr::mutate(
        out_new,
        era = dplyr::case_when(
          year < dat$parms$styr ~ "init",
          year >= dat$parms$styr & year <= dat$parms$endyr ~ "time",
          year > dat$parms$endyr ~ "fore",
          TRUE ~ NA
        )
      )
    }

    #### WHAM ----
  } else if (model == "wham") {
    # This is how Bai read the ASAP output
    # asap_output conversion written by Bai Li
    # asap_output <- dget(file.path(casedir, "output", subdir, paste("s", keep_sim_id[om_sim], sep = ""), "asap3.rdat"))
    # setwd(file.path(casedir, "output", subdir, paste("s", keep_sim_id[om_sim], sep = "")))
    # asap_std <- readRep("asap3", suffix = ".std")
    cli::cli_abort("File not currently compatible.")
    #### AMAK ####
  } else if (model == "amak") {
    cli::cli_abort("WHAM output not currently compatible.")
    #### JABBA ####
  } else if (tolower(model) == "jabba") {
    cli::cli_abort("JABBA output not currently compatible.")
  } else if (model == "fims") {
    if (grepl(".RDS", file)) {
      fims_output <- readRDS(file)
    } else {
      fims_output <- file
    }
    # TEMPORARY -- add in era to df
    if ("era" %notin% colnames(fims_output)) {
      fims_output$era <- NA
    }
    fims_output[setdiff(tolower(names(out_new)), tolower(names(fims_output)))] <- NA
    out_new <- fims_output
  } else {
    cli::cli_abort(c(
      message = "Output file not compatible.",
      "i" = "`model` entered as {model}.",
      "i" = "Accepted `model` options: SS3, BAM, WHAM, AMAK, JABBA."
    ))
  }

  #### Exporting ####
  # Combind DFs into one
  out_new <- out_new |>
    dplyr::mutate(
      estimate = dplyr::case_when(
        is.na(estimate) ~ NA_real_,
        TRUE ~ as.numeric(estimate)
      ),
      uncertainty = dplyr::case_when(
        is.na(uncertainty) ~ NA_real_,
        TRUE ~ as.numeric(uncertainty)
      ),
      initial = dplyr::case_when(
        is.na(initial) ~ NA_real_,
        TRUE ~ as.numeric(initial)
      ),
      year = dplyr::case_when(
        is.na(year) ~ NA_real_,
        TRUE ~ as.numeric(year)
      ),
      likelihood = dplyr::case_when(
        is.na(likelihood) ~ NA_real_,
        TRUE ~ as.numeric(likelihood)
      ),
      # change era name to keep standard
      era = dplyr::case_when(
        era == "Main" ~ "time",
        TRUE ~ tolower(era)
      )
    ) |>
    suppressWarnings()
  if (tolower(model) == "ss3") {
    con_file <- system.file("resources", "ss3_var_names.csv", package = "asar", mustWork = TRUE)
    var_names_sheet <- utils::read.csv(con_file, na.strings = "")
  } else if (tolower(model) == "bam") {
    con_file <- system.file("resources", "bam_var_names.csv", package = "asar", mustWork = TRUE)
    var_names_sheet <- utils::read.csv(con_file, na.strings = "") |>
      dplyr::mutate(
        label = tolower(label)
      )
  } else if (tolower(model) == "fims") {
    con_file <- system.file("resources", "fims_var_names.csv", package = "asar", mustWork = TRUE)
    var_names_sheet <- utils::read.csv(con_file, na.strings = "")
  }

  if (file.exists(con_file)) {
    # Remove 'X' column if it exists
    var_names_sheet <- var_names_sheet |>
      dplyr::select(-dplyr::any_of("X"))
    out_new <- dplyr::left_join(out_new, var_names_sheet, by = c("module_name", "label")) |>
      dplyr::mutate(label = dplyr::case_when(
        !is.na(alt_label) ~ alt_label,
        # TODO: add this to ss3_var_names.xlsx
        label == "dev" & module_name == "SPAWN_RECRUIT" ~ "recruitment_deviation",
        # TODO: add this to ss3_var_names.xlsx
        label == "dev" & module_name == "SPAWN_RECRUIT_CURVE" ~ "recruitment_deviation",
        # TODO: add this to ss3_var_names.xlsx
        label == "dev" & module_name == "DISCARD_OUTPUT" ~ "discard_deviation",
        TRUE ~ label
      )) |>
      dplyr::select(-alt_label) |>
      dplyr::rename(length_bins = len_bins)
  }
  cli::cli_alert_success("Conversion finished!")

  # save if indicated
  if (!is.null(save_dir)) {
    # add check if save_dir does not end in .rda
    if (!grepl("\\.rda$", save_dir)) {
      cli::cli_alert_warning("save_dir does not contain .rda extension.")
      cli::cli_alert_info("Saving file as std_output.rda")
    }
    # save_name <- stringr::str_extract(save_dir, "(?<=/)[^/]+(?=\\.rda$)")
    # assign(save_name, out_new)
    save(out_new, file = save_dir)
  }
  tibble::as_tibble(out_new)
} # close function
