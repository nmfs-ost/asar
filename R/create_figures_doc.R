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
                               include_all = TRUE,
                               rda_dir = NULL) {

  model <- match.arg(model, several.ok = FALSE)

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
    ## import plot, caption, alt text
    figures_doc <- paste0(
      figures_doc,
      add_chunk(
        paste0("# load rda
load(file.path(rda_dir, 'recruitment_figure.rda'))\n
# save rda with plot-specific name
recruitment_plot_rda <- rda\n
# remove generic rda object
rm(rda)\n
# save figure, caption, and alt text as separate objects
recruitment_plot <- recruitment_plot_rda$figure
recruitment_cap <- recruitment_plot_rda$cap
recruitment_alt_text <- recruitment_plot_rda$alt_text"),
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
        eval = "true",
        add_option = TRUE,
        chunk_op = c(
          glue::glue(
            "fig-cap: recruitment_cap"
          ),
          glue::glue(
            "fig-alt: recruitment_alt_text"
          )
        )
      ),
      "\n"
    )

    # SB figure
    ## import plot, caption, alt text
    figures_doc <- paste0(
      figures_doc,
      add_chunk(
        paste0("# load rda
load(file.path(rda_dir, 'spawning_biomass_figure.rda'))\n
# save rda with plot-specific name
spawning_biomass_plot_rda <- rda\n
# remove generic rda object
rm(rda)\n
# save figure, caption, and alt text as separate objects
spawning_biomass_plot <- spawning_biomass_plot_rda$figure
spawning_biomass_cap <- spawning_biomass_plot_rda$cap
spawning_biomass_alt_text <- spawning_biomass_plot_rda$alt_text"),
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
        eval = "true",
        add_option = TRUE,
        chunk_op = c(
          glue::glue(
            "fig-cap: spawning_biomass_cap"
          ),
          glue::glue(
            "fig-alt: spawning_biomass_alt_text"
          )
        )
      ),
      "\n"
    )

    # B figure
    ## import plot, caption, alt text
    figures_doc <- paste0(
      figures_doc,
      add_chunk(
        paste0("# load rda
load(file.path(rda_dir, 'biomass_figure.rda'))\n
# save rda with plot-specific name
biomass_plot_rda <- rda\n
# remove generic rda object
rm(rda)\n
# save figure, caption, and alt text as separate objects
biomass_plot <- biomass_plot_rda$figure
biomass_cap <- biomass_plot_rda$cap
biomass_alt_text <- biomass_plot_rda$alt_text"),
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
        eval = "true",
        add_option = TRUE,
        chunk_op = c(
          glue::glue(
            "fig-cap: biomass_cap"
          ),
          glue::glue(
            "fig-alt: biomass_alt_text"
          )
        )
      ),
      "\n"
    )


    # Landings figure
    ## import plot, caption, alt text
    figures_doc <- paste0(
      figures_doc,
      add_chunk(
        paste0("# load rda
load(file.path(rda_dir, 'landings_figure.rda'))\n
# save rda with plot-specific name
landings_plot_rda <- rda\n
# remove generic rda object
rm(rda)\n
# save figure, caption, and alt text as separate objects
landings_plot <- landings_plot_rda$figure
landings_cap <- landings_plot_rda$cap
landings_alt_text <- landings_plot_rda$alt_text"),
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
        eval = "true",
        add_option = TRUE,
        chunk_op = c(
          glue::glue(
            "fig-cap: landings_cap"
          ),
          glue::glue(
            "fig-alt: landings_alt_text"
          )
        )
      ),
      "\n"
    )

    # recruitment deviations figure
    ## import plot, caption, alt text
    figures_doc <- paste0(
      figures_doc,
      add_chunk(
        paste0("# load rda
load(file.path(rda_dir, 'recruitment_deviations_figure.rda'))\n
# save rda with plot-specific name
recruitment_deviations_plot_rda <- rda\n
# remove generic rda object
rm(rda)\n
# save figure, caption, and alt text as separate objects
recruitment_deviations_plot <- recruitment_deviations_plot_rda$figure
recruitment_deviations_cap <- recruitment_deviations_plot_rda$cap
recruitment_deviations_alt_text <- recruitment_deviations_plot_rda$alt_text"),
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
        eval = "true",
        add_option = TRUE,
        chunk_op = c(
          glue::glue(
            "fig-cap: recruitment_deviations_cap"
          ),
          glue::glue(
            "fig-alt: recruitment_deviations_alt_text"
          )
        )
      ),
      "\n"
    )

    # spawning recruitment figure
    ## import plot, caption, alt text
    figures_doc <- paste0(
      figures_doc,
      add_chunk(
        paste0("# load rda
load(file.path(rda_dir, 'est_stock_recruitment_figure.rda'))\n
# save rda with plot-specific name
spawning_recruitment_plot_rda <- rda\n
# remove generic rda object
rm(rda)\n
# save figure, caption, and alt text as separate objects
spawning_recruitment_plot <- spawning_recruitment_plot_rda$figure
spawning_recruitment_cap <- spawning_recruitment_plot_rda$cap
spawning_recruitment_alt_text <- spawning_recruitment_plot_rda$alt_text"),
        label = "fig-spawning_recruitment-setup",
        eval = "true"
      ),
      "\n"
    )

    ## add figure
    figures_doc <- paste0(
      figures_doc,
      add_chunk(
        paste0("spawning_recruitment_plot"),
        label = "fig-spawning_recruitment-plot",
        eval = "true",
        add_option = TRUE,
        chunk_op = c(
          glue::glue(
            "fig-cap: spawning_recruitment_cap"
          ),
          glue::glue(
            "fig-alt: spawning_recruitment_alt_text"
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
