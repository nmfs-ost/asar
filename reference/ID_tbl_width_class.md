# Identify table width class

Identify table width class

## Usage

``` r
ID_tbl_width_class(tables_dir, plot_name, portrait_pg_width)
```

## Arguments

- tables_dir:

  The location of the "tables" folder, which contains tables files.

- plot_name:

  Name of the .rda file containing the table

- portrait_pg_width:

  The amount of space between the margins of a portrait-oriented page,
  in inches. Represents the threshold for the maximum width of a table
  that can be rendered on a portrait page before it needs to be resized,
  rotated, and/or split across multiple pages.

## Value

The width class of a table: regular, wide, or extra-wide. The result
will determine whether the table can be rendered on a portrait page
as-is, or if it needs to be resized, rotated, and/or split across
multiple pages.

## Examples

``` r
if (FALSE) { # \dontrun{
ID_tbl_width_class(
  plot_name = "indices.abundance_table.rda",
  tables_dir = here::here(),
  portrait_pg_width = 5
)
} # }
```
