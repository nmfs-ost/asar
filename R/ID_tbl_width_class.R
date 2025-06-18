#' Identify table width class
#'
#' @inheritParams render_lg_table
#' @param portrait_pg_width The amount of space between the margins of a
#' portrait-oriented page, in inches. Represents the threshold for the maximum
#' width of a table that can be rendered on a portrait page before it needs to
#' be resized, rotated, and/or split across multiple pages.
#'
#' @return The width class of a table: regular, wide, or extra-wide. The result
#' will determine whether the table that can be rendered on a portrait page
#' as-is, or if it needs to be resized, rotated, and/or split across multiple pages.
#' @export
#'
#' @examples
#' \dontrun{
#' ID_tbl_width_class(
#'   plot_name = "indices.abundance_table.rda",
#'   rda_dir = here::here(),
#'   portrait_pg_width = 5
#' )
#' }
ID_tbl_width_class <- function(
    rda_dir,
    plot_name,
    portrait_pg_width) {
  rda_path <- file.path(paste0(rda_dir, "/rda_files/", plot_name))

  if (file.exists(rda_path)) {
    load(rda_path)
    table_rda <- rda
    rm(rda)
    table_width <- flextable::flextable_dim(table_rda$table)[["widths"]] |>
      as.numeric()

    # determine table width class
    if (table_width > portrait_pg_width) {
      if (table_width > 12) {
        width_class <- "extra-wide"
      } else {
        width_class <- "wide"
      }
    } else {
      width_class <- "regular"
    }
  } else {
    width_class <- "regular"
  }

  return(width_class)
}
