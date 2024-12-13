#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
globvar <- c(
  "yr", "value", "estimate", "seas", "subseas", "age_bins", "morph",
  "age", "year", "nsim", "fleet", "uncertainty", "initial", "alt_label",
  "last", "affiliation", "label", "type", "caption", "alt_text", "caption",
  "name", "."
)
if (getRversion() >= "2.15.1") utils::globalVariables(globvar)
