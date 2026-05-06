# Split extra-wide tables into smaller tables and export

Split extra-wide tables into smaller tables and export

## Usage

``` r
export_split_tbls(
  tables_dir = NULL,
  plot_name = NULL,
  essential_columns = NULL
)
```

## Arguments

- tables_dir:

  The location of the "tables" folder, which contains tables files.

- plot_name:

  Name of the .rda file containing the table

- essential_columns:

  The columns that will be retained between the split tables, formatted
  as a sequence (e.g., 1:2 for columns 1-2, or 1 for a single column).
  Example: for the indices table, this could be the year column.

## Value

The number of split tables

## Examples

``` r
if (FALSE) { # \dontrun{
export_split_tbls(
  tables_dir = here::here(),
  plot_name = "bnc_table.rda",
  essential_columns = 5
)

export_split_tbls(
  tables_dir = getwd(),
  plot_name = "indices.abundance_table.rda",
  essential_columns = 1:2
)
} # }
```
