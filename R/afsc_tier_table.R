# AFSC Tier Table
afsc_tier_table <- function() {
  level <- c(
    "Level 1: Normal",
    "Level 2: Substantially Increased Concerns",
    "Level 3: Major Concern",
    "Level 4: Extreme COncern"
  )
  ass_considerations <- c(
    "Typical to moderately increased uncertainty/minor unresolved issues in assessment.",
    "Substantially increased assessment uncertainty/ unresolved issues.",
    "Major problems with the stock assessment; very poor fits to data; high level of uncertainty; strong retrospective bias.",
    "Severe problems with the stock assessment; severe retrospective bias. Assessment considered unreliable."
  )
  popdy_considerations <- c(
    "Stock trends are typical for the stock; recent recruitment is within normal range.",
    "Stock trends are unusual; abundance increasing or decreasing faster than has been seen recently, or recruitment pattern is atypical.",
    "Stock trends are highly unusual; very rapid changes in stock abundance, or highly atypical recruitment patterns.",
    "Stock trends are unprecedented; More rapid changes in stock abundance than have ever been seen previously, or a very long stretch o poor recruitment compared to previous patterns."
  )
  eco_considerations <- c(
    "No apparent environmental/ecosystem concerns",
    "Some indicators showing adverse signals relevant to the stock but the pattern is not consistent across all indicators.",
    "Multiple indicators showing consistent adverse signals a) across the same trophic level as the stock, and/or b) up or down trophic levels (i.e., predators and prey of the stock)",
    "Extreme anomalies in multiple ecosystem indicators that are highly likely to impact the stock; Potential for cascading effects on other ecosystem components"
  )
  fish_performance <- c(
    "No apparent fishery/resourceuse performance and/or behavior concerns",
    "Some indicators showing adverse signals but the pattern is not consistent across all indicators",
    "Multiple indicators showing consistent adverse signals a) across different sectors, and/or b) different gear types",
    "Extreme anomaliesin multiple performance indicators that are highly likely to impact the stock"
  )
  tier_df <- data.frame(level, ass_considerations, popdy_considerations, eco_considerations, fish_performance)
  flextable::flextable(tier_df) |>
    flextable::set_header_labels(
      level = "",
      ass_considerations = "Assessment-related considerations",
      popdy_considerations = "Population dynamics considerations",
      eco_considerations = "Environmental/ecosystems considerations",
      fish_performance = "Fishery Performance"
    ) |>
    flextable::italic(part = "header") |>
    flextable::bold(part = "header") |>
    flextable::hline(i = c(1, 2, 3))
}
