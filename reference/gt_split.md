# Split a table into a group of tables (a `gt_group`)

With a **gt** table, you can split it into multiple tables and get that
collection in a `gt_group` object. This function is useful for those
cases where you want to section up a table in a specific way and print
those smaller tables across multiple pages (in RTF and Word outputs,
primarily via [gtsave](https://gt.rstudio.com/reference/gtsave.html),
or, with breaks between them when the output context is HTML.

## Usage

``` r
gt_split(data, row_every_n = NULL, row_slice_i = NULL, col_slice_at = NULL)
```

## Arguments

- data:

  *The gt table data object*

  `obj:<gt_tbl>` // **required**

  This is the **gt** table object that is commonly created through use
  of the [`gt::gt()`](https://gt.rstudio.com/reference/gt.html)
  function.

- row_every_n:

  *Split at every n rows*

  `scalar<numeric|integer>` // *default:* `NULL` (`optional`)

  A directive to split at every *n* number of rows. This argument
  expects a single numerical value.

- row_slice_i:

  *Row-slicing indices*

  `vector<numeric|integer>` // *default:* `NULL` (`optional`)

  An argument for splitting at specific row indices. Here, we expect
  either a vector of index values or a function that evaluates to a
  numeric vector.

- col_slice_at:

  *Column-slicing locations*

  `<column-targeting expression>` // *default:* `NULL` (`optional`)

  Any columns where vertical splitting across should occur. The splits
  occur to the right of the resolved column names. Can either be a
  series of column names provided in
  [`c()`](https://rdrr.io/r/base/c.html), a vector of column indices, or
  a select helper function (e.g.
  [starts_with](https://tidyselect.r-lib.org/reference/starts_with.html),
  [ends_with](https://tidyselect.r-lib.org/reference/starts_with.html),
  [contains](https://tidyselect.r-lib.org/reference/starts_with.html),
  [matches](https://tidyselect.r-lib.org/reference/starts_with.html),
  [num_range](https://tidyselect.r-lib.org/reference/starts_with.html),
  and
  [everything](https://tidyselect.r-lib.org/reference/everything.html)).

## Value

An object of class `gt_group`.

## Note

This function is a temporary export of asar, but all development and
rights belong to `rstudio/gt`. This function provides a fix to the
function introduced by a bug in gt v1.3.0. Until this is corrected in
the package, we are using the function here. Once this bug is patched,
we will deprecate and remove this function from asar and direct users to
use the gt package version of this function.

## Examples

Use a subset of the
[`gt::gtcars`](https://gt.rstudio.com/reference/gtcars.html) dataset to
create a **gt** table. Format the `msrp` column to display numbers as
currency values, set column widths with
[cols_width](https://gt.rstudio.com/reference/cols_width.html), and
split the table at every five rows with `gt_split()`. This creates a
`gt_group` object containing two tables. Printing this object yields two
tables separated by a line break.

    gtcars |>
      dplyr::slice_head(n = 10) |>
      dplyr::select(mfr, model, year, msrp) |>
      gt() |>
      fmt_currency(columns = msrp) |>
      cols_width(
        year ~ px(80),
        everything() ~ px(150)
      ) |>
      gt_split(row_every_n = 5)

Use a smaller subset of the
[`gt::gtcars`](https://gt.rstudio.com/reference/gtcars.html) dataset to
create a **gt** table. Format the `msrp` column to display numbers as
currency values, set the table width with
[`gt::tab_options()`](https://gt.rstudio.com/reference/tab_options.html)
and split the table at the `model` column This creates a `gt_group`
object again containing two tables but this time we get a vertical
split. Printing this object yields two tables of the same width.

    gtcars |>
      dplyr::slice_head(n = 5) |>
      dplyr::select(mfr, model, year, msrp) |>
      gt() |>
      fmt_currency(columns = msrp) |>
      tab_options(table.width = px(400)) |>
      gt_split(col_slice_at = "model")

## Function ID

14-2

## Function Introduced

`v0.9.0` (Mar 31, 2023)
