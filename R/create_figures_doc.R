#' Create Quarto Document of Figures
#'
#' @inheritParams create_template
#' @param subdir Location of subdirectory storing the assessment report template
#' @param include_all TRUE/FALSE; Option to include all default
#' figures for a stock assessment report. Default is true.
#'
#' @return A quarto document with pre-loaded R chunk that adds the
#' stock assessment tables from the nmfs-ost/stockplotr R package. The
#' quarto document will become part of the stock assessment outline.
#' @export
#'
#' @examples
#' \dontrun{
#' create_figures_doc(
#' subdir = getwd(),
#' rda_dir = here::here())
#' }
create_figures_doc <- function(subdir = getwd(),
                               include_all = TRUE,
                               rda_dir = getwd()) {
  if (include_all) {

    # add chunk that creates object as the directory of all rdas
    figures_doc_setup <- paste0(
      # add header
      "## Figures {#sec-figures}\n \n",
      add_chunk(
        paste0("rda_dir <- '", rda_dir, "/rda_files'"),
        label = "set-rda-dir-figs",
        eval = "true"
      ),
      "\n"
    )

    # current list of figures that can be produced with stockplotr
    stockplotr_fig_list <- c(
      "recruitment_figure.rda",
      "spawning.biomass_figure.rda",
      "biomass_figure.rda",
      "landings_figure.rda",
      "recruitment.deviations_figure.rda",
      "sr_figure.rda",
      "indices_figure.rda",
      "pop.naa_figure.rda",
      "pop.baa_figure.rda"
    )

    # create two-chunk system to plot each figure
    create_fig_chunks <- function(stockplotr_fig = NA,
                                  rda_dir = getwd()){

        if (any(grepl(stockplotr_fig,
                      list.files(file.path(rda_dir, "rda_files"))))) {

          fig_shortname <- stringr::str_remove(stockplotr_fig, "_figure.rda")

        ## import plot, caption, alt text
        figures_doc_plot_setup1 <- paste0(
         # figures_doc,
          add_chunk(
            paste0("# if the figure rda exists:
if (file.exists(file.path(rda_dir, '", stockplotr_fig, "'))){\n
  # load rda
  load(file.path(rda_dir, '", stockplotr_fig, "'))\n
  # save rda with plot-specific name
  ", fig_shortname, "_plot_rda <- rda\n
  # remove generic rda object
  rm(rda)\n
  # save figure, caption, and alt text as separate objects; set eval to TRUE
  ", fig_shortname, "_plot <- ", fig_shortname, "_plot_rda$figure
  ", fig_shortname, "_cap <- ", fig_shortname, "_plot_rda$cap
  ", fig_shortname, "_alt_text <- ", fig_shortname, "_plot_rda$alt_text
  eval_", fig_shortname, " <- TRUE\n
# if the figure rda does not exist, don't evaluate the next chunk
} else {eval_", fig_shortname, " <- FALSE}"),
            label = paste0("fig-", fig_shortname, "-setup"),
            eval = "true"
            ),
          "\n"
         )

        ## make figure chunk
        figures_doc_plot_setup2 <- paste0(
         # figures_doc_plot_setup1,
          add_chunk(
            paste0(fig_shortname, "_plot"),
            label = paste0("fig-", fig_shortname),
            eval = paste0("!expr eval_", fig_shortname),
            add_option = TRUE,
            chunk_op = c(
              glue::glue(
                "fig-cap: !expr if(eval_", fig_shortname, ") ", fig_shortname, "_cap"
              ),
              glue::glue(
                "fig-alt: !expr if(eval_", fig_shortname, ") ", fig_shortname, "_alt_text"
              )
            )
          ),
          "\n"
         )

        return(paste0(figures_doc_plot_setup1,
                      figures_doc_plot_setup2))
        } else {
          message(paste0(stockplotr_fig, " not found. Figure not created."))
        }
    }

    # paste together figures_doc setup and code chunks into one object
    figures_doc <- paste0("")
    for (i in 1:length(stockplotr_fig_list)){
      fig_chunk <- create_fig_chunks(stockplotr_fig = stockplotr_fig_list[i],
                                       rda_dir = rda_dir)

      figures_doc <- ifelse(is.null(fig_chunk),
                            figures_doc,
                            paste0(figures_doc, fig_chunk))

     if (i == length(stockplotr_fig_list)){
       figures_doc <- paste0(figures_doc_setup, figures_doc)
     }
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
