#' Create Quarto Document of Tables
#'
#' @inheritParams create_template
#' @inheritParams create_figures_doc
#'
#' @return Create a quarto document as part of a stock assessment outline with
#' pre-loaded R chunk adding the stock assessment tables from the nmfs-ost/satf R package
#' @export
#'
#' @examples
#' \dontrun{
#' create_tables_doc(
#'   resdir = "my_save_dir",
#'   model_results = "std_model_output.csv",
#'   model = "SS3",
#'   subdir = "my_subdir",
#'   include_all = TRUE)
#' }
#'
create_tables_doc <- function(resdir = NULL,
                              model_results = NULL,
                              model = c("SS3", "BAM", "ASAP", "AMAK", "WHAM"),
                              subdir = NULL,
                              include_all = TRUE) {
  model <- match.arg(model, several.ok = FALSE)

  if (include_all) {
    # Create tables quarto doc - maybe should add this as separate fxn - same with figs
    tables_doc <- paste0(
      "## Tables \n \n",
      # Indices table
      add_chunk(
        paste0(
          "satf::table_indices(dat = '", resdir, "/", model_results, "', model = '", model, "')"
        ),
        label = "indices",
        eval = "false"
      )
    )

    # Add other tables follow the same above format
    # tables_doc <- paste0("/n",
    #                      tables_doc,
    #                      add_chunk(
    #                        paste0(
    #                          "satf::table_XX()"
    #                        )
    #                       )
    #                      )
  } else {
    # add option for only adding specified tables
  }

  # Save tables doc to template folder
  utils::capture.output(cat(tables_doc),
                        file = paste0(subdir, "/", "08_tables.qmd"),
                        append = FALSE)
}
