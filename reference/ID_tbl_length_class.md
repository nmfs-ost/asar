# Identify table length class

Identify table length class

## Usage

``` r
ID_tbl_length_class(tables_dir, plot_name)
```

## Arguments

- tables_dir:

  The location of the "tables" folder, which contains tables files.

- plot_name:

  Name of the .rda file containing the table

## Value

The length class of a table: regular or long. The result will determine
whether the table can be rendered on a page as-is, or if it needs to be
split across multiple pages.

## Examples

``` r
if (FALSE) { # \dontrun{
ID_tbl_length_class(
  plot_name = "indices.abundance_table.rda",
  tables_dir = here::here()
)
} # }
```
