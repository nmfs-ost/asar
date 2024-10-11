#' Create Quarto Document of Figures
#'
#' @inheritParams create_template
#' @param year The terminal year of the assessment model, which is
#' included in plotting.
#' @param subdir Location of subdirectory storing the assessment report template
#' @param include_all TRUE/FALSE; Option to include all default
#' figures (create_figures_doc.R) or tables (create_tables_doc.R)
#' for a stock assessment report. Default is true.
#'
#' @return A quarto document with pre-loaded R chunk that adds the
#' stock assessment tables from the nmfs-ost/satf R package. The
#' quarto document will become part of the stock assessment outline.
#' @export
#'
#' @examples
#' \dontrun{
#' create_figures_doc(
#'  resdir = "my_save_dir",
#'  model_results = "std_model_output.csv",
#'  model = "SS3",
#'  year = 2024,
#'  subdir = "my_subdir",
#'  include_all = TRUE)
#' }
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
      add_chunk(plot_code, label = "spawn_bio"),
      "\n"
    )
  } else {
    # add option for only adding specified tables
  }

  # Save tables doc to template folder
  utils::capture.output(cat(figures_doc), file = paste0(subdir, "/", "figures.qmd"), append = FALSE)
}
