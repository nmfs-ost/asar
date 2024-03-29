#' Write a harvest projection table
#'
#' @param results_last projection results from last assessment
#' @param results_lastp1 projection results + 1 year from last assessment
#' @param results_curr current projections
#' @param results_currp1 current projections - 1 yr?
#' @param tier tier categorization for target spp
#' @param overfishing_last YES or NO overfishing last assessment
#' @param overfishing_curr YES or NO currently overfishing?
#'
#' @return Write harvest projection table according to AFSC/NPFMC guidelines;
#'         Function to autopopulate the table using converted results
#' @export
#'
#' @examples write_harv_proj_table(tier = 3)
write_harv_proj_table <- function(
    results_last = 9999,
    results_lastp1 = 9999,
    results_curr = 9999,
    results_currp1 = 9999,
    tier = NULL,
    overfishing_last = "NO",
    overfishing_curr = "NO") {
  # Pull current projection results
  # Need to update function to pull these results after converter is finished
  # proj_last <- results_last |>
  #   dplyr::select(M)
  #
  # proj_lastp1 <- results_lastp1 |>
  #   dplyr::select(M)
  #
  # proj_curr <- results_lastp1 |>
  #   dplyr::select(M)
  #
  # proj_currp1 <- results_lastp1 |>
  #   dplyr::select(M)
  #
  # proj_res <- list(results_last, results_lastp1, results_curr, results_currp1) |>
  #   purrr::map(dplyr::select(M))

  # Temporary fill
  proj_last <- 9999
  proj_lastp1 <- 9999
  proj_curr <- 9999
  proj_currp1 <- 9999
  proj_res <- 9999

  # Write table contents
  quantity <- c(
    "M (natural mortality rate", "Tier", "Biomass (t)", "F OFL", "maxF ABC", "F ABC", "OFL (t)",
    "maxABC (t)", "ABC (t)"
  )
  status <- c(
    "Status", "As determined last year for: current year - 2", "As determined last year for: current year - 1",
    "As determined this year for: current year - 1", "As determined this year for: current year"
  )
  overfishing <- c("Overfishing", overfishing_last, NA, overfishing_curr, NA)

  # Build table
  proj_tab <- data.frame(quantity, proj_last, proj_lastp1, proj_curr, proj_currp1) |>
    rbind(status, overfishing) |>
    flextable::flextable() |>
    flextable::delete_part(part = "header") |>
    flextable::add_header(
      quantity = "",
      proj_last = "current year",
      proj_lastp1 = "current year +1",
      proj_curr = "current year +1",
      proj_currp1 = "current year +2"
    ) |>
    flextable::add_header(
      quantity = "",
      proj_last = "As estimated or specified last year for:",
      proj_lastp1 = "As estimated or specified last year for:",
      proj_curr = "As estimated or recommended this year for:",
      proj_currp1 = "As estimated or recommended this year for:"
    ) |>
    flextable::merge_h(part = "header") |>
    flextable::vline(j = c(1, 3), part = "body") |>
    flextable::vline(j = c(1), part = "header") |>
    flextable::hline(part = "header") |>
    flextable::align(part = "header", align = "center")

  return(proj_tab)
}
