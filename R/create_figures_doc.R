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

  captions_alttext <- utils::read.csv(
    system.file("resources", "captions_alttext.csv", package = "asar")
  )

  if (include_all) {
    # Create tables quarto doc - maybe should add this as separate fxn - same with figs
    figures_doc <- paste0("## Figures \n \n")
    # Recruitment ts figure
    figures_doc <- paste0(
      figures_doc,
      add_chunk(
        paste0("satf::plot_recruitment(dat = output)"),
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
      "satf::plot_spawning_biomass(dat = output, ref_line = 'target', end_year = ", year, ")"
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

    # B figure
    plot_code <- paste0(
      "satf::plot_biomass(dat = '", resdir, "/", model_results,
      "', model = '", model,
      "', ref_line = 'target', endyr = ", year, ")"
    )


    figures_doc <- paste0(
      figures_doc,
      add_chunk(plot_code,
                label = "fig-bio",
                eval = "true",
                add_option = TRUE,
                chunk_op = c(
                  glue::glue(
                    "fig-cap: '",
                    captions_alttext |>
                      dplyr::filter(label == "tot_b" & type == "figure") |>
                      dplyr::select(caption) |>
                      as.character(),
                    "'"
                  ),
                  glue::glue(
                    "fig-alt: '",
                    captions_alttext |>
                      dplyr::filter(label == "tot_b" & type == "figure") |>
                      dplyr::select(alt_text) |>
                      as.character(),
                    "'"
                  )
                )
      ),
      "\n"
    )


    # Landings figure
    plot_code <- paste0(
      "satf::plot_landings(dat = '", resdir, "/", model_results,
      "', model = '", model,
      "', ref_line = 'target', endyr = ", year, ")"
    )


    figures_doc <- paste0(
      figures_doc,
      add_chunk(plot_code,
                label = "fig-landings",
                eval = "true",
                add_option = TRUE,
                chunk_op = c(
                  glue::glue(
                    "fig-cap: '",
                    captions_alttext |>
                      dplyr::filter(label == "landings" & type == "figure") |>
                      dplyr::select(caption) |>
                      as.character(),
                    "'"
                  ),
                  glue::glue(
                    "fig-alt: '",
                    captions_alttext |>
                      dplyr::filter(label == "landings" & type == "figure") |>
                      dplyr::select(alt_text) |>
                      as.character(),
                    "'"
                  )
                )
      ),
      "\n"
    )


    # Recruitment deviations figure
    plot_code <- paste0(
      "satf::plot_recruitment_deviations(dat = '", resdir, "/", model_results,
      "', model = '", model,
      "', ref_line = 'target', endyr = ", year, ")"
    )


    figures_doc <- paste0(
      figures_doc,
      add_chunk(plot_code,
                label = "fig-recruitment_deviations",
                eval = "true",
                add_option = TRUE,
                chunk_op = c(
                  glue::glue(
                    "fig-cap: '",
                    captions_alttext |>
                      dplyr::filter(label == "recruitment_deviations" & type == "figure") |>
                      dplyr::select(caption) |>
                      as.character(),
                    "'"
                  ),
                  glue::glue(
                    "fig-alt: '",
                    captions_alttext |>
                      dplyr::filter(label == "recruitment_deviations" & type == "figure") |>
                      dplyr::select(alt_text) |>
                      as.character(),
                    "'"
                  )
                )
      ),
      "\n"
    )


    # spawning recruitment figure
    plot_code <- paste0(
      "satf::plot_spawning_recruitment(dat = '", resdir, "/", model_results,
      "', model = '", model,
      "', ref_line = 'target', endyr = ", year, ")"
    )


    figures_doc <- paste0(
      figures_doc,
      add_chunk(plot_code,
                label = "fig-spawn_recruitment",
                eval = "true",
                add_option = TRUE,
                chunk_op = c(
                  glue::glue(
                    "fig-cap: '",
                    captions_alttext |>
                      dplyr::filter(label == "est_stock_recruitment" & type == "figure") |>
                      dplyr::select(caption) |>
                      as.character(),
                    "'"
                  ),
                  glue::glue(
                    "fig-alt: '",
                    captions_alttext |>
                      dplyr::filter(label == "est_stock_recruitment" & type == "figure") |>
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

  # Save figures doc to template folder
  utils::capture.output(cat(figures_doc),
    file = paste0(subdir, "/", "09_figures.qmd"),
    append = FALSE
  )
}
