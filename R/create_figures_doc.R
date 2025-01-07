#' Create Quarto Document of Figures
#'
#' @inheritParams create_template
#' @param subdir Location of subdirectory storing the assessment report template
#' @param include_all TRUE/FALSE; Option to include all default
#' figures for a stock assessment report. Default is true.
#'
#' @return A quarto document with pre-loaded R chunk that adds the
#' stock assessment tables from the nmfs-ost/satf R package. The
#' quarto document will become part of the stock assessment outline.
#' @export
#'
create_figures_doc <- function(subdir = NULL,
                               include_all = TRUE,
                               rda_dir = NULL) {
  if (include_all) {
    # add header
    figures_doc <- paste0("## Figures \n \n")

    # add chunk that creates object as the directory of all rdas
    figures_doc <- paste0(
      figures_doc,
      add_chunk(
        paste0("rda_dir <- '", rda_dir, "/rda_files'"),
        label = "set-rda-dir-figs",
        eval = "true"
      ),
      "\n"
    )

    # Recruitment ts figure
    if (any(grepl("recruitment_figure.rda", list.files(file.path(rda_dir, "rda_files"))))) {
      ## import plot, caption, alt text
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("# if the recruitment figure rda exists:
if (file.exists(file.path(rda_dir, 'recruitment_figure.rda'))){\n
  # load rda
  load(file.path(rda_dir, 'recruitment_figure.rda'))\n
  # save rda with plot-specific name
  recruitment_plot_rda <- rda\n
  # remove generic rda object
  rm(rda)\n
  # save figure, caption, and alt text as separate objects; set eval to TRUE
  recruitment_plot <- recruitment_plot_rda$figure
  recruitment_cap <- recruitment_plot_rda$cap
  recruitment_alt_text <- recruitment_plot_rda$alt_text
  eval_recruitment <- TRUE\n
# if the recruitment figure rda does not exist, don't evaluate the next chunk
} else {eval_recruitment <- FALSE}"),
          label = "fig-recruitment-setup",
          eval = "true"
        ),
        "\n"
      )

      ## add figure
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("recruitment_plot"),
          label = "fig-recruitment-plot",
          eval = "!expr eval_recruitment",
          add_option = TRUE,
          chunk_op = c(
            glue::glue(
              "fig-cap: !expr if(eval_recruitment) recruitment_cap"
            ),
            glue::glue(
              "fig-alt: !expr if(eval_recruitment) recruitment_alt_text"
            )
          )
        ),
        "\n"
      )
    } else {
      message("Recruitment time series figure not created.")
    }

    # SB figure
    if (any(grepl("spawning.biomass_figure.rda", list.files(file.path(rda_dir, "rda_files"))))) {
      ## import plot, caption, alt text
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("# if the spawning biomass figure rda exists:
if (file.exists(file.path(rda_dir, 'spawning.biomass_figure.rda'))){\n
  # load rda
  load(file.path(rda_dir, 'spawning.biomass_figure.rda'))\n
  # save rda with plot-specific name
  spawning_biomass_plot_rda <- rda\n
  # remove generic rda object
  rm(rda)\n
  # save figure, caption, and alt text as separate objects; set eval to TRUE
  spawning_biomass_plot <- spawning_biomass_plot_rda$figure
  spawning_biomass_cap <- spawning_biomass_plot_rda$cap
  spawning_biomass_alt_text <- spawning_biomass_plot_rda$alt_text
  eval_spawning_biomass <- TRUE\n
# if the spawning biomass figure rda does not exist, don't evaluate the next chunk
} else {eval_spawning_biomass <- FALSE}"),
          label = "fig-spawning_biomass-setup",
          eval = "true"
        ),
        "\n"
      )

      ## add figure
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("spawning_biomass_plot"),
          label = "fig-spawning_biomass-plot",
          eval = "!expr eval_spawning_biomass",
          add_option = TRUE,
          chunk_op = c(
            glue::glue(
              "fig-cap: !expr if(eval_spawning_biomass) spawning_biomass_cap"
            ),
            glue::glue(
              "fig-alt: !expr if(eval_spawning_biomass) spawning_biomass_alt_text"
            )
          )
        ),
        "\n"
      )
    } else {
      message("Spawning biomass time series figure not created.")
    }

    # B figure
    if (any(grepl("^biomass_figure.rda", list.files(file.path(rda_dir, "rda_files"))))) {
      ## import plot, caption, alt text
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("# if the biomass figure rda exists:
if (file.exists(file.path(rda_dir, 'biomass_figure.rda'))){\n
  # load rda
  load(file.path(rda_dir, 'biomass_figure.rda'))\n
  # save rda with plot-specific name
  biomass_plot_rda <- rda\n
  # remove generic rda object
  rm(rda)\n
  # save figure, caption, and alt text as separate objects; set eval to TRUE
  biomass_plot <- biomass_plot_rda$figure
  biomass_cap <- biomass_plot_rda$cap
  biomass_alt_text <- biomass_plot_rda$alt_text
  eval_biomass <- TRUE\n
# if the biomass figure rda does not exist, don't evaluate the next chunk
} else {eval_biomass <- FALSE}"),
          label = "fig-biomass-setup",
          eval = "true"
        ),
        "\n"
      )

      ## add figure
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("biomass_plot"),
          label = "fig-biomass-plot",
          eval = "!expr eval_biomass",
          add_option = TRUE,
          chunk_op = c(
            glue::glue(
              "fig-cap: !expr if(eval_biomass) biomass_cap"
            ),
            glue::glue(
              "fig-alt: !expr if(eval_biomass) biomass_alt_text"
            )
          )
        ),
        "\n"
      )
    } else {
      message("Biomass time series figure not created.")
    }


    # Landings figure
    if (any(grepl("landings_figure.rda", list.files(file.path(rda_dir, "rda_files"))))) {
      ## import plot, caption, alt text
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("# if the landings figure rda exists:
if (file.exists(file.path(rda_dir, 'landings_figure.rda'))){\n
  # load rda
  load(file.path(rda_dir, 'landings_figure.rda'))\n
  # save rda with plot-specific name
  landings_plot_rda <- rda\n
  # remove generic rda object
  rm(rda)\n
  # save figure, caption, and alt text as separate objects; set eval to TRUE
  landings_plot <- landings_plot_rda$figure
  landings_cap <- landings_plot_rda$cap
  landings_alt_text <- landings_plot_rda$alt_text
  eval_landings <- TRUE\n
# if the landings figure rda does not exist, don't evaluate the next chunk
} else {eval_landings <- FALSE}"),
          label = "fig-landings-setup",
          eval = "true"
        ),
        "\n"
      )

      ## add figure
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("landings_plot"),
          label = "fig-landings-plot",
          eval = "!expr eval_landings",
          add_option = TRUE,
          chunk_op = c(
            glue::glue(
              "fig-cap: !expr if(eval_landings) landings_cap"
            ),
            glue::glue(
              "fig-alt: !expr if(eval_landings) landings_alt_text"
            )
          )
        ),
        "\n"
      )
    } else {
      message("Landings time series figure not created.")
    }

    # recruitment deviations figure
    if (any(grepl("recruitment.deviations_figure.rda", list.files(file.path(rda_dir, "rda_files"))))) {
      ## import plot, caption, alt text
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("# if the recruitment deviations figure rda exists:
if (file.exists(file.path(rda_dir, 'recruitment.deviations_figure.rda'))){\n
  # load rda
  load(file.path(rda_dir, 'recruitment.deviations_figure.rda'))\n
  # save rda with plot-specific name
  recruitment_deviations_plot_rda <- rda\n
  # remove generic rda object
  rm(rda)\n
  # save figure, caption, and alt text as separate objects; set eval to TRUE
  recruitment_deviations_plot <- recruitment_deviations_plot_rda$figure
  recruitment_deviations_cap <- recruitment_deviations_plot_rda$cap
  recruitment_deviations_alt_text <- recruitment_deviations_plot_rda$alt_text
  eval_recruitment_deviations <- TRUE\n
# if the recruitment deviations figure rda does not exist, don't evaluate the next chunk
} else {eval_recruitment_deviations <- FALSE}"),
          label = "fig-recruitment_deviations-setup",
          eval = "true"
        ),
        "\n"
      )

      ## add figure
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("recruitment_deviations_plot"),
          label = "fig-recruitment_deviations-plot",
          eval = "!expr eval_recruitment_deviations",
          add_option = TRUE,
          chunk_op = c(
            glue::glue(
              "fig-cap: !expr if(eval_recruitment_deviations) recruitment_deviations_cap"
            ),
            glue::glue(
              "fig-alt: !expr if(eval_recruitment_deviations) recruitment_deviations_alt_text"
            )
          )
        ),
        "\n"
      )
    } else {
      message("Recruitment deviations figure not created.")
    }

    # stock recruitment figure
    if (any(grepl("sr_figure.rda", list.files(file.path(rda_dir, "rda_files"))))) {
      ## import plot, caption, alt text
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("# if the spawning recruitment figure rda exists:
if (file.exists(file.path(rda_dir, 'sr_figure.rda'))){\n
  # load rda
  load(file.path(rda_dir, 'sr_figure.rda'))\n
  # save rda with plot-specific name
  sr_plot_rda <- rda\n
  # remove generic rda object
  rm(rda)\n
  # save figure, caption, and alt text as separate objects; set eval to TRUE
  sr_plot <- sr_plot_rda$figure
  sr_cap <- sr_plot_rda$cap
  sr_alt_text <- sr_plot_rda$alt_text
  eval_sr <- TRUE\n
# if the spawning recruitment figure rda does not exist, don't evaluate the next chunk
} else {eval_sr <- FALSE}"),
          label = "fig-sr-setup",
          eval = "true"
        ),
        "\n"
      )

      ## add figure
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("sr_plot"),
          label = "fig-sr-plot",
          eval = "!expr eval_sr",
          add_option = TRUE,
          chunk_op = c(
            glue::glue(
              "fig-cap: !expr if(eval_sr) sr_cap"
            ),
            glue::glue(
              "fig-alt: !expr if(eval_sr) sr_alt_text"
            )
          )
        ),
        "\n"
      )
    } else {
      message("Stock-Recruitment figure not created.")
    }

    # indices figure
    if (any(grepl("indices_figure.rda", list.files(file.path(rda_dir, "rda_files"))))) {
      ## import plot, caption, alt text
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("# if the indices figure rda exists:
if (file.exists(file.path(rda_dir, 'indices_figure.rda'))){\n
  # load rda
  load(file.path(rda_dir, 'indices_figure.rda'))\n
  # save rda with plot-specific name
  indices_plot_rda <- rda\n
  # remove generic rda object
  rm(rda)\n
  # save figure, caption, and alt text as separate objects; set eval to TRUE
  indices_plot <- indices_plot_rda$figure
  indices_cap <- indices_plot_rda$cap
  indices_alt_text <- indices_plot_rda$alt_text
  eval_indices <- TRUE\n
# if the indices figure rda does not exist, don't evaluate the next chunk
} else {eval_indices <- FALSE}"),
          label = "fig-indices-setup",
          eval = "true"
        ),
        "\n"
      )

      ## add figure
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("indices_plot"),
          label = "fig-indices-plot",
          eval = "!expr eval_indices",
          add_option = TRUE,
          chunk_op = c(
            glue::glue(
              "fig-cap: !expr if(eval_indices) indices_cap"
            ),
            glue::glue(
              "fig-alt: !expr if(eval_indices) indices_alt_text"
            )
          )
        ),
        "\n"
      )
    } else {
      message("Indices of abundance figure not created.")
    }
  } else {
    # add option for only adding specified figures
    warning("Functionality for adding specific figures is still in development. Please set 'include_all' to true and edit the 09_figures.qmd file to remove specific figures from the report.")
  }

  # Save figures doc to template folder
  utils::capture.output(cat(figures_doc),
    file = paste0(subdir, "/", "09_figures.qmd"),
    append = FALSE
  )
}
