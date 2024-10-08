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

  if (include_all) {
    # Create tables quarto doc - maybe should add this as separate fxn - same with figs
    figures_doc <- paste0("## Figures \n \n")
    # Recruitment ts figure
    figures_doc <- paste0(
      figures_doc,
      add_chunk(
        paste0("satf::plot_recruitment(dat = '", resdir, "/", model_results, "', model = '", model, "')"),
        label = "recruitment",
        eval = "false"
        add_option = TRUE,
        chunk_op = "fig-cap: 'this is the caption for the figure.'"
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
      add_chunk(plot_code, label = "spawn_bio", eval = "false"),
      "\n"
    )
  } else {
    # add option for only adding specified tables
  }

  # Save tables doc to template folder
  utils::capture.output(cat(figures_doc), file = paste0(subdir, "/", "figures.qmd"), append = FALSE)
}
