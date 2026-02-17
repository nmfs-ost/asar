#' Identify table length class
#'
#' @inheritParams render_lg_table
#'
#' @return The length class of a table: regular or long. The result
#' will determine whether the table can be rendered on a page
#' as-is, or if it needs to be split across multiple pages.
#' @export
#'
#' @examples
#' \dontrun{
#' ID_tbl_length_class(
#'   plot_name = "indices.abundance_table.rda",
#'   tables_dir = here::here()
#' )
#' }
ID_tbl_length_class <- function(
  tables_dir,
  plot_name
) {
  tables_path <- fs::path(
    tables_dir,
    "tables",
    paste0(plot_name, "_table.rda")
  )

  if (file.exists(tables_path)) {
    load(tables_path)
    table_rda <- rda
    rm(rda)
    gt_table <- table_rda$table

    # Get table length in rows
    table_length <- nrow(gt_table[["_data"]])
    
    # determine table length class
    if (table_length <= 30) {
      length_class <- "regular"
    } else {
      length_class <- "long"
      }
  } else {
    cli::cli_abort(message = "Table not found at {tables_path}")
  }

  length_class
}
