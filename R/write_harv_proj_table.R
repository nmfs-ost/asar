# Write harvest projection table according to AFSC/NPFMC guidelines

# Function to autopopulate the table using converted results

write_harv_proj_table <- function(
    results_last = NULL,
    results_lastp1 = NULL,
    results_curr = NULL,
    results_currp1 = NULL,
    tier = NULL,
    overfishing_last = "NO",
    overfishing_curr = "NO"
    ){

  # Pull current projection results
  proj_last <- results_last |>
    dplyr::select(M)

  proj_lastp1 <- results_lastp1 |>
    dplyr::select(M)

  proj_curr <- results_lastp1 |>
    dplyr::select(M)

  proj_currp1 <- results_lastp1 |>
    dplyr::select(M)

  proj_res <- list(results_last, results_lastp1, results_curr, results_currp1) |>
    purrr::map(dplyr::select(M)) |>


  # Write table contents
  quantity <- c("M (natural mortality rate", "Tier", "Biomass (t)", "F OFL", "maxF ABC", "F ABC","OFL (t)",
                "maxABC (t)", "ABC (t)")
  status <- c("Status", "As determined last year for: current year - 2", "As determined last year for: current year - 1",
              "As determined this year for: current year - 1", "As determined this year for: current year")
  overfishing <- c("Overfishing", overfishing_last, NA, overfishing_curr, NA)

  # Build table
  proj_tab <- data.frame(quantity, proj_last, proj_lastp1, proj_curr, proj_currp1) |>
    rbind(status, overfishing) |>
    flextable::flextable() |>
    flextable::delete_part(part = "header") %>%
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
    flextable::vline(j = c(1,3,6)) |>
    flextable::hline(part = "header") |>
    flextable::align(part = "header", align = "center")

  return(proj_tab)

}
