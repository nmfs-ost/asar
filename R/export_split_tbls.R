#' Split extra-wide tables into smaller tables and export
#'
#' @inheritParams render_lg_table
#'
#' @return The number of split tables
#' @export
#'
#' @examples
#' \dontrun{
#' export_split_tbls(
#'   tables_dir = here::here(),
#'   plot_name = "bnc_table.rda",
#'   essential_columns = 5
#' )
#'
#' export_split_tbls(
#'   tables_dir = getwd(),
#'   plot_name = "indices.abundance_table.rda",
#'   essential_columns = 1:2
#' )
#' }
export_split_tbls <- function(
  tables_dir = NULL,
  plot_name = NULL,
  essential_columns = NULL
) {
  rda_path <- file.path(paste0(tables_dir, "/tables/", plot_name))

  load(rda_path)
  table_rda <- rda
  rm(rda)

  # split tables and export
  render_lg_table(
    report_flextable = table_rda$table,
    essential_columns = essential_columns,
    tables_dir = tables_dir,
    plot_name = plot_name
  )
}
