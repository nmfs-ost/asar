#' Create Quarto Document of Figures
#'
#' @inheritParams create_template
#' @param year End year for assessment, for inclusion in plotting
#' @param subdir Location of subdirectory storing the assessment report template
#' @param include_all TRUE/FALSE; Option to include all default
#' figures for a stock assessment report. Default is true.
#'
#' @return A quarto document with pre-loaded R chunk that adds the
#' stock assessment tables from the nmfs-ost/satf R package. The
#' quarto document will become part of the stock assessment outline.
#' @export
#'
create_figures_doc <- function(resdir = NULL,
                               model_results = NULL,
                               model = c("SS3", "BAM", "ASAP", "AMAK", "WHAM"),
                               year = NULL,
                               subdir = NULL,
                               include_all = TRUE) {

  model <- match.arg(model, several.ok = FALSE)

  # import pre-written captions and alt text
  captions_alttext <- utils::read.csv(
    system.file("resources", "captions_alttext.csv", package = "asar")
  )

  # import converted model output
  output <- utils::read.csv(
    paste0(resdir, "/", model_results)
  )

  # extract quantities
  # using the <<- exports the object to the R environment!
  start_year <<- output |>
    dplyr::select(year) |>
    dplyr::filter(year == as.numeric(year)) |>
    dplyr::filter(year == min(year)) |>
    unique() |>
    as.numeric()

  Fend <<- output |>
    dplyr::filter(label == 'fishing_mortality' & year == 2023 & age == 1 & fleet == 1 & sex == 1 & module_name == "F_AT_AGE") |>
    dplyr::select(estimate) |>
    as.numeric()


  # create the figure chunks
  if (include_all) {
    # Create tables quarto doc - maybe should add this as separate fxn - same with figs
    figures_doc <- paste0("## Figures \n \n")
    # Recruitment ts figure
    figures_doc <- paste0(
      figures_doc,
      add_chunk(
        paste0("satf::plot_recruitment(dat = '", resdir, "/", model_results, "', model = '", model, "')"),
        label = "recruitment",
        eval = "false",
        add_option = TRUE,
        chunk_op = c(
          glue::glue(
            "fig-cap: '",
            captions_alttext |>
              dplyr::filter(label == "recruitment" & type == "figure") |>
              dplyr::select(caption) |>
              as.character(),
            "'"
          ),
          glue::glue(
            "fig-alt: '",
            captions_alttext |>
              dplyr::filter(label == "recruitment" & type == "figure") |>
              dplyr::select(alt_text) |>
              as.character(),
            "'"
          )
        )
      ),
      "\n"
    )


    # SB figure
    plot_code <- paste0(
      "satf::plot_spawning_biomass(dat = '", resdir, "/", model_results,
      "', model = '", model,
      "', ref_line = 'target', endyr = ", year, ")"
    )


    figures_doc <- paste0(
      figures_doc,
      add_chunk(plot_code,
                label = "spawn_bio",
                eval = "false",
                add_option = TRUE,
                chunk_op = c(
                  glue::glue(
                    "fig-cap: '",
                    captions_alttext |>
                      dplyr::filter(label == "spawning_biomass" & type == "figure") |>
                      dplyr::select(caption) |>
                      as.character(),
                    "'"
                  ),
                  glue::glue(
                    "fig-alt: '",
                    captions_alttext |>
                      dplyr::filter(label == "spawning_biomass" & type == "figure") |>
                      dplyr::select(alt_text) |>
                      as.character(),
                    "'"
                    )
                  )
                ),
      "\n"
    )
  } else {
    # add option for only adding specified figures
  }

  # substitute quantity placeholders in the captions/alt text with
  # the real values, extracted above
  figures_doc <- gsub('start_year',
                      start_year,
                 gsub('Fend',
                      Fend,
                      figures_doc)
                 )

  # Save figures doc to template folder
  utils::capture.output(cat(figures_doc),
                        file = paste0(subdir, "/", "09_figures.qmd"),
                        append = FALSE)
}
