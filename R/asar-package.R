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
  "name", ".", "rda", "cols_to_del", "cols_to_del_seq", "essential_cols_seq",
  "file_dir", "prev_skeleton", "prev_format", "author_list", "bib_name",
  "input", "first", "mi", "first_initial", "bib", "output_order", "keyword",
  "output", "sexes", "like", "era", "init", "parm_stdev", "Component",
  "row_rescale", "table_list", "read.csv", "All", "x", "Acronym", "Meaning",
  "meaning_lower", "Definition", "n", "Label", "figures_doc_header",
  "X", "len_bins"
)
if (getRversion() >= "2.15.1") utils::globalVariables(globvar)
