#' Create Quarto Document of Figures
#'
#' @param subdir Location of subdirectory storing the assessment report template
#' @param figures_dir The location of the "figures" folder, which contains
#' figures files.
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
  empty_doc_text <- "Please refer to the `stockplotr` package downloaded from remotes::install_github('nmfs-ost/stockplotr') to add premade figures."

  fig_header <- "# Figures {#sec-figures}\n \n"

  # append figure-producing code to non-empty figures doc, if it exists, vs. overwriting it
  append <- FALSE
  if (length(file.path(subdir, list.files(subdir, pattern = "figures.qmd"))) == 1) {
    existing_figs_doc <- file.path(subdir, list.files(subdir, pattern = "figures.qmd"))
    figure_content <- readLines(existing_figs_doc) |>
      suppressWarnings()
    if ("# Figures {#sec-figures}" %in% figure_content) {
      append <- TRUE
      cli::cli_alert_info("Figures doc will be appended to include figures in `figures_dir`.")

      # remove empty_doc_text
      updated_content <- gsub(empty_doc_text, "", figure_content, fixed = TRUE)
      writeLines(updated_content, existing_figs_doc)
    }
  } else {
    # existing_figs_doc <- NULL
    figure_content <- ""
  }

  figures_doc_header <- ifelse(append,
    "",
    fig_header
  )

  # add chunk that creates object as the directory of all rdas
  # check if the current setup already has the setup chunk
  if (!(any(grepl(
    "#| label: 'set-rda-dir-figs'",
    figure_content,
    fixed = TRUE
  )))) {
    figures_doc_setup <- paste0(
      add_chunk(
        glue::glue("figures_dir <- fs::path('{figures_dir}', 'figures')"),
        label = "set-rda-dir-figs"
      ),
      "\n"
    )
  } else {
    figures_doc_setup <- ""
  }
 
  figures_doc <- ""

  # list all files in figures
  file_list <- list.files(file.path(figures_dir, "figures"))

  # create sublist of only rda figure files
  rda_fig_list <- file_list[grepl("_figure.rda", file_list)]
  # create sublist of only non-rda figure files
  non.rda_fig_list <- file_list[!grepl(".rda", file_list)]
  
  # Check if rda or non-rda already exists and remove from list
  if (length(file.path(subdir, list.files(subdir, pattern = "figures.qmd"))) == 1) {
    existing_figs_doc <- file.path(subdir, list.files(subdir, pattern = "figures.qmd"))
    figure_content <- readLines(existing_figs_doc) |>
      suppressWarnings()
    # find all instances of figures
    existing_rda_figs <- vapply(rda_fig_list, function(x) {
      any(grepl(x, figure_content, fixed = TRUE))
    }, FUN.VALUE = logical(1))
    rda_fig_list <- rda_fig_list[!existing_rda_figs]
    # add condition for message to add "new" into message
    new_rda <- ifelse(
      length(existing_rda_figs) > 0,
      TRUE,
      FALSE
    )
    # find instances of non-rda and remove
    existing_non.rda_figs <- vapply(non.rda_fig_list, function(x) {
      any(grepl(x, figure_content, fixed = TRUE))
    }, FUN.VALUE = logical(1))
    non.rda_fig_list <- non.rda_fig_list[!existing_non.rda_figs]
    # add condition for message to add "new" into message
    new_non.rda <- ifelse(
      length(existing_non.rda_figs) > 0,
      TRUE,
      FALSE
    )
  }

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
", fig_shortname, "_cap <- ", fig_shortname, "_plot_rda$caption
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

    paste0(
      figures_doc_plot_setup1,
      figures_doc_plot_setup2
    )
  }

  if (length(file_list) == 0) {
    cli::cli_alert_warning("Found zero figure files in {fs::path(figures_dir, 'figures')}.",
      wrap = TRUE
    )
    cli::cli_alert_info("For `create_figures_doc` to incorporate figures, there must be:",
      wrap = TRUE
    )
    cli::cli_ol(c(
      "a 'figures' folder in {fs::path(figures_dir)}",
      "files in appropriate formats (e.g., .rda, .png, .jpg) in the 'figures' folder"
    ))
    figures_doc <- paste0(
      figures_doc_header,
      empty_doc_text
    )
  } else {
    # paste rda figure code chunks into one object
    if (length(rda_fig_list) > 0) {
      cli::cli_alert_success("Found {length(rda_fig_list)}{ifelse(new_rda, ' new ', ' ')}figure{?s} in an rda format (i.e., .rda) in {fs::path(figures_dir, 'figures')}.",
        wrap = TRUE
      )
      rda_figures_doc <- ""
      for (i in seq_along(rda_fig_list)) {
        fig_chunk <- create_fig_chunks(
          fig = rda_fig_list[i],
          figures_dir = figures_dir
        )

        rda_figures_doc <- paste0(
          rda_figures_doc, fig_chunk,
          "{{< pagebreak >}} \n\n"
        )
      }
    } else {
      cli::cli_alert_warning("Found zero figures in an rda format (i.e., .rda) in {fs::path(figures_dir, 'figures')}.",
        wrap = TRUE
      )
    }
    if (length(non.rda_fig_list) > 0) {
      cli::cli_alert_success("Found {length(non.rda_fig_list)}{ifelse(new_non.rda, ' new ', ' ')}figure{?s} in a non-rda format (e.g., .jpg, .png) in {fs::path(figures_dir, 'figures')}.",
        wrap = TRUE
      )
      non.rda_figures_doc <- ""
      for (i in seq_along(non.rda_fig_list)) {
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
          "}\n\n",
          "{{< pagebreak >}} \n\n"
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
      # ifelse(!append, figures_doc_setup, ""),
      figures_doc_setup,
      ifelse(
        exists("rda_figures_doc"),
        rda_figures_doc,
        ""
      ),
      ifelse(
        exists("non.rda_figures_doc"),
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
    append = append
  )

  # Read through figures doc and warn about identical labels
  doc_path <- ifelse(
    any(grepl("_figures.qmd$", list.files(subdir))),
    fs::path(subdir, list.files(subdir)[grep("_figures.qmd", list.files(subdir))]),
    fs::path(subdir, "09_figures.qmd")
  )
  
  fix_duplicate_chunks(
    doc_path = doc_path,
    doc_type = "Figures"
  )
}