# Split an extra-wide table into multiple tables

Split an extra-wide table into multiple tables

## Usage

``` r
render_lg_table(report_gt, essential_columns, tables_dir, plot_name)
```

## Arguments

- report_gt:

  The extra-wide gt table.

- essential_columns:

  The columns that will be retained between the split tables, formatted
  as a sequence (e.g., 1:2 for columns 1-2, or 1 for a single column).
  Example: for the indices table, this could be the year column.

- tables_dir:

  The location of the "tables" folder, which contains tables files.

- plot_name:

  Name of the .rda file containing the table

## Value

A list of the split tables.

## Examples

``` r
if (FALSE) { # \dontrun{
render_lg_table(
  report_gt = indices_table,
  essential_columns = 1,
  tables_dir = here::here(),
  plot_name = "indices.abundance_table.rda"
)

render_lg_table(
  report_gt = important_table,
  essential_columns = 1:3,
  tables_dir = "data",
  plot_name = "bnc_table.rda"
)
} # }
```
