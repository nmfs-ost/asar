#' Create Quarto Document of Figures
#'
#' @param figures_dir The location of the "figures" folder, which contains
#' figures files.
#' @param subdir Location of subdirectory storing the assessment report template
#'
#' @return A quarto document with pre-loaded R chunk that adds the
#' stock assessment tables from the nmfs-ost/stockplotr R package. The
#' quarto document will become part of the stock assessment outline.
#' @export
#'
#' @examples
#' \dontrun{
#' create_figures_doc(
#'   subdir = getwd(),
#'   figures_dir = here::here()
#' )
#' }
create_figures_doc <- function(subdir = getwd(),
                               figures_dir = getwd()) {
  figures_doc_header <- "# Figures {#sec-figures}\n \n"

  # add chunk that creates object as the directory of all rdas
  figures_doc_setup <- paste0(
    add_chunk(
      glue::glue("figures_dir <- fs::path('{figures_dir}', 'figures')"),
      label = "set-rda-dir-figs"
    ),
    "\n"
  )

  figures_doc <- ""

  # list all files in figures
  file_list <- list.files(file.path(figures_dir, "figures"))
  # create sublist of only figure files
  file_fig_list <- file_list[grepl("_figure", file_list)]

  # create sublist of only rda figure files
  rda_fig_list <- file_fig_list[grepl("_figure.rda", file_fig_list)]
  # create sublist of only non-rda figure files
  non.rda_fig_list <- file_fig_list[!grepl(".rda", file_fig_list)]

  # create two-chunk system to plot each rda figure
  create_fig_chunks <- function(fig = NA,
                                figures_dir = getwd()) {
    fig_shortname <- stringr::str_remove(fig, "_figure.rda")

    ## import plot, caption, alt text
    figures_doc_plot_setup1 <- paste0(
      # figures_doc,
      add_chunk(
        paste0(
          "# load rda
load(file.path(figures_dir, '", fig, "'))\n
# save rda with plot-specific name
", fig_shortname, "_plot_rda <- rda\n
# remove generic rda object
rm(rda)\n
# save figure, caption, and alt text as separate objects
", fig_shortname, "_plot <- ", fig_shortname, "_plot_rda$figure
", fig_shortname, "_cap <- ", fig_shortname, "_plot_rda$cap
", fig_shortname, "_alt_text <- ", fig_shortname, "_plot_rda$alt_text"
        ),
        label = glue::glue("fig-{fig_shortname}-setup")
      ),
      "\n"
    )

    ## make figure chunk
    figures_doc_plot_setup2 <- paste0(
      # figures_doc_plot_setup1,
      add_chunk(
        paste0(fig_shortname, "_plot"),
        label = glue::glue("fig-{fig_shortname}"),
        # add_option = TRUE,
        chunk_option = c(
          "echo: false",
          "warning: false",
          glue::glue(
            "fig-cap: !expr {fig_shortname}_cap"
          ),
          glue::glue(
            "fig-alt: !expr {fig_shortname}_alt_text"
          )
        )
      ),
      "\n"
    )

    return(paste0(
      figures_doc_plot_setup1,
      figures_doc_plot_setup2
    ))
  }

  if (length(file_fig_list) == 0) {
    cli::cli_alert_warning("Found zero figure files in {fs::path(figures_dir, 'figures')}.",
      wrap = TRUE
    )
    figures_doc <- "# Figures {#sec-figures}"
  } else {
    # paste rda figure code chunks into one object
    if (length(rda_fig_list) > 0) {
      cli::cli_alert_success("Found {length(rda_fig_list)} figure{?s} in an rda format (i.e., .rda) in {fs::path(figures_dir, 'figures')}.",
        wrap = TRUE
      )
      rda_figures_doc <- ""
      for (i in 1:length(rda_fig_list)) {
        fig_chunk <- create_fig_chunks(
          fig = rda_fig_list[i],
          figures_dir = figures_dir
        )

        rda_figures_doc <- paste0(rda_figures_doc, fig_chunk)
      }
    } else {
      cli::cli_alert_warning("Found zero figures in an rda format (i.e., .rda) in {fs::path(figures_dir, 'figures')}.",
        wrap = TRUE
      )
    }
    if (length(non.rda_fig_list) > 0) {
      cli::cli_alert_success("Found {length(non.rda_fig_list)} figure{?s} in a non-rda format (e.g., .jpg, .png) in {fs::path(figures_dir, 'figures')}.",
        wrap = TRUE
      )
      non.rda_figures_doc <- ""
      for (i in 1:length(non.rda_fig_list)) {
        # remove file extension
        fig_name <- stringr::str_extract(
          non.rda_fig_list[i],
          "^[^.]+"
        )
        # remove "_figure", if present
        fig_name <- sub("_figure", "", fig_name)
        fig_chunk <- paste0(
          "![Your caption here](", fs::path(
            "figures",
            non.rda_fig_list[i]
          ),
          "){#fig-",
          fig_name,
          "}\n\n"
        )

        non.rda_figures_doc <- paste0(non.rda_figures_doc, fig_chunk)
      }
    } else {
      cli::cli_alert_warning("Found zero figure files in a non-rda format (e.g., .jpg, .png) in {fs::path(figures_dir, 'figures')}.",
        wrap = TRUE
      )
    }

    # combine figures_doc setup with figure chunks
    figures_doc <- paste0(
      figures_doc_header,
      figures_doc_setup,
      ifelse(exists("rda_figures_doc"),
        rda_figures_doc,
        ""
      ),
      ifelse(exists("non.rda_figures_doc"),
        non.rda_figures_doc,
        ""
      )
    )
  }
  # Save figures doc to template folder
  utils::capture.output(cat(figures_doc),
    file = paste0(
      subdir, "/",
      ifelse(
        any(grepl("_figures.qmd$", list.files(subdir))),
        list.files(subdir)[grep("_figures.qmd", list.files(subdir))],
        "09_figures.qmd"
      )
    ),
    append = FALSE
  )
}
