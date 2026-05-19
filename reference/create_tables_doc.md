# Create Quarto Document of Tables

Only tables in an rda format (e.g., my_table.rda) will be imported.
Tables in other formats (e.g., .jpg, .png) are not supported; they lack
text recognition. See [the `asar` custom figures and tables
vignette](https://nmfs-ost.github.io/asar/articles/custom-figs-tabs.html#make-rdas)
for more information about making .rda files with custom tables.

## Usage

``` r
create_tables_doc(subdir = getwd(), tables_dir = getwd())
```

## Arguments

- subdir:

  Location of subdirectory storing the assessment report template

- tables_dir:

  The location of the "tables" folder, which contains tables files.

## Value

Create a quarto document as part of a stock assessment outline with
pre-loaded R chunks that add stock assessment tables from the
nmfs-ost/stockplotr R package, or other tables in the same rda format.

## Details

If your table is too wide to print on a portrait-oriented page, the page
will be rotated to landscape view. If if is too wide to print in
landscape view, it will be split into multiple tables. In this case, a
new rda will be created and is identifiable by the phrase "split" in the
filename (e.g., indices.abundance_table.rda will generate a new
indices.abundance_table_split.rda file), and column 1 will be repeated
across split tables. These tables will share the same caption. To
specify a different repeated column(s), use asar::export_split_tbls with
your preferred essential_columns value.

## Examples

``` r
if (FALSE) { # \dontrun{
create_tables_doc(
  subdir = getwd(),
  tables_dir = here::here()
)
} # }
```
