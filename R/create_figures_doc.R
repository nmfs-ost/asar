#' Create Quarto Document of Figures
#'
#' @param resdir directory where the results file is located
#' @param model_results name of the results file of assessment output
#' @param model stock assessment model
#' @param year end year for assessment to include in plotting
#' @param subdir subdirectory where the assessment report template is being stored
#' @param include_all include all default figures for a stock assessment report
#'
#' @return Create a quarto document as part of a stock assessment outline with
#' pre-loaded R chunk adding the stock assessment tables from the nmfs-ost/satf R package
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
        label = "recruitment"
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
      add_chunk(plot_code, label = "spawn_bio"),
      "\n"
    )
  } else {
    # add option for only adding specified tables
  }

  # Save tables doc to template folder
  utils::capture.output(cat(figures_doc), file = paste0(subdir, "/", "figures.qmd"), append = FALSE)
}
