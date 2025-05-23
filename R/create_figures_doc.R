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
    # add header
    figures_doc <- paste0("## Figures {#sec-figures}\n \n")

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

    # will condense/simplify this workflow in another phase
    #
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

    create_fig_chunks <- function(stockplotr_fig = NA,
                                  rda_dir = getwd()){

        if (any(grepl(stockplotr_fig,
                      list.files(file.path(rda_dir, "rda_files"))))) {

          fig_shortname <- stringr::str_remove(stockplotr_fig, "_figure.rda")

        ## import plot, caption, alt text
        figures_doc <- paste0(
          figures_doc,
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

        ## add figure
        figures_doc <- paste0(
          figures_doc,
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
        } else {
          message("stockplotr_fig not found. Figure not created.")
        }
    }

    # Recruitment ts figure
    figures_doc <- create_fig_chunks(stockplotr_fig = stockplotr_fig_list[1],
                                     rda_dir = rda_dir)

    # SB figure
    figures_doc <- create_fig_chunks(stockplotr_fig = stockplotr_fig_list[2],
                                     rda_dir = rda_dir)

    # B figure
    figures_doc <- create_fig_chunks(stockplotr_fig = stockplotr_fig_list[3],
                                     rda_dir = rda_dir)

    # Landings figure
    figures_doc <- create_fig_chunks(stockplotr_fig = stockplotr_fig_list[4],
                                     rda_dir = rda_dir)

    # recruitment deviations figure
    figures_doc <- create_fig_chunks(stockplotr_fig = stockplotr_fig_list[5],
                                     rda_dir = rda_dir)

    # stock recruitment figure
    # TODO: fix workflow so that if a fig isn't found, it doesn't convert figures_doc
    # to NULL. If it is found, then figures_doc <- figures_doc (needs to append)
    create_fig_chunks(stockplotr_fig = stockplotr_fig_list[6],
                                     rda_dir = rda_dir)

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
          label = "fig-indices",
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

    # abundance at age figure
    if (any(grepl("pop.naa_figure.rda", list.files(file.path(rda_dir, "rda_files"))))) {
      ## import plot, caption, alt text
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("# if the abundance at age figure rda exists:
if (file.exists(file.path(rda_dir, 'pop.naa_figure.rda'))){\n
  # load rda
  load(file.path(rda_dir, 'pop.naa_figure.rda'))\n
  # save rda with plot-specific name
  pop.naa_plot_rda <- rda\n
  # remove generic rda object
  rm(rda)\n
  # save figure, caption, and alt text as separate objects; set eval to TRUE
  pop.naa_plot <- pop.naa_plot_rda$figure
  pop.naa_cap <- pop.naa_plot_rda$cap
  pop.naa_alt_text <- pop.naa_plot_rda$alt_text
  eval_pop.naa <- TRUE\n
# if the abundance at age figure rda does not exist, don't evaluate the next chunk
} else {eval_pop.naa <- FALSE}"),
          label = "fig-pop.naa-setup",
          eval = "true"
        ),
        "\n"
      )

      ## add figure
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("pop.naa_plot"),
          label = "fig-pop.naa",
          eval = "!expr eval_pop.naa",
          add_option = TRUE,
          chunk_op = c(
            glue::glue(
              "fig-cap: !expr if(eval_pop.naa) pop.naa_cap"
            ),
            glue::glue(
              "fig-alt: !expr if(eval_pop.naa) pop.naa_alt_text"
            )
          )
        ),
        "\n"
      )
    } else {
      message("Abundance at age figure not created.")
    }

    # biomass at age figure
    if (any(grepl("pop.baa_figure.rda", list.files(file.path(rda_dir, "rda_files"))))) {
      ## import plot, caption, alt text
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("# if the biomass at age figure rda exists:
if (file.exists(file.path(rda_dir, 'pop.baa_figure.rda'))){\n
  # load rda
  load(file.path(rda_dir, 'pop.baa_figure.rda'))\n
  # save rda with plot-specific name
  pop.baa_plot_rda <- rda\n
  # remove generic rda object
  rm(rda)\n
  # save figure, caption, and alt text as separate objects; set eval to TRUE
  pop.baa_plot <- pop.baa_plot_rda$figure
  pop.baa_cap <- pop.baa_plot_rda$cap
  pop.baa_alt_text <- pop.baa_plot_rda$alt_text
  eval_pop.baa <- TRUE\n
# if the biomass at age figure rda does not exist, don't evaluate the next chunk
} else {eval_pop.baa <- FALSE}"),
          label = "fig-pop.baa-setup",
          eval = "true"
        ),
        "\n"
      )

      ## add figure
      figures_doc <- paste0(
        figures_doc,
        add_chunk(
          paste0("pop.baa_plot"),
          label = "fig-pop.baa",
          eval = "!expr eval_pop.baa",
          add_option = TRUE,
          chunk_op = c(
            glue::glue(
              "fig-cap: !expr if(eval_pop.baa) pop.baa_cap"
            ),
            glue::glue(
              "fig-alt: !expr if(eval_pop.baa) pop.baa_alt_text"
            )
          )
        ),
        "\n"
      )
    } else {
      message("Biomass at age figure not created.")
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
