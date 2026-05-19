# Adding Custom Tables and Figures

## Introduction

The main way to create tables and figures for your `asar` report is by
using functions within the `stockplotr` package. However, if
`stockplotr` doesn’t contain the functions to make tables or figures you
need, you may wish to add custom tables or figures to your report. Here,
we provide multiple workflow options for doing so.

### Choose your workflow

[TABLE]

## Workflow Guides

- [Direct image insertion workflow](#tabset-1-1)
- [rda-based workflow](#tabset-1-2)
- [Direct coding-in-qmd workflow](#tabset-1-3)

&nbsp;

- 1.  Write captions and alternative text
  2.  Save captions and alternative text
  3.  Add custom tables and figures

  #### Write captions and alternative text

  Once you know what your tables and figures look like, write your own
  captions and alternative text. Custom tables will require captions
  only.

  ##### Writing text

  Check out the `stockplotr` [‘captions_alt_text_template.csv’
  file](https://github.com/nmfs-ost/stockplotr/blob/master/inst/resources/captions_alt_text_template.csv),
  which contains the templates for each table or figure’s
  caption/alternative text, to get a sense of how each description is
  structured. Then, refer to [the accessibility vignette’s
  guide](https://nmfs-ost.github.io/asar/articles/accessibility_guide.html)
  to learn how to write clear and comprehensive alternative text.

  We recommend adding in key quantities, like maximum and minimum
  values, to ensure your captions and alternative text clearly describe
  your plots. For example, if you’re showing how biomass changed over
  time, we recommend providing the maximum and minimum values of biomass
  and years shown, instead of simply stating that a figure shows biomass
  changing over time. Check out the [“How captions and alternative text
  are generated”
  vignette](https://nmfs-ost.github.io/stockplotr/articles/how-caps-alttext-are-made.html)
  to see how we extract certain key quantities from the model results
  files. You are welcome to use and adapt that code to your own results
  to calculate key quantities, or calculate the quantities with your own
  code.

  ##### Saving text

  Do you have an existing captions_alt_text.csv file? If you’ve already
  generated a figure or table rda, this file should exist, most likely
  in your working directory. If you don’t have one, run a `stockplotr`
  plotting function (e.g., `plot_biomass(make_rda = TRUE)`,
  `table_bnc(make_rda = TRUE)`) to generate the file.

  Open captions_alt_text.csv file, then add your text. The file is set
  up like this:

  - Column 1 (“label”) is a shorthand label for your figure or table.
    Labels should be somewhat short and exclude spaces. Examples include
    “kobe”, “relative.biomass”, and “fishing.mortality”.
  - Column 2 (“type”) contains “figure” or “table”.
  - Column 3 (“caption”) contains your caption.
  - Column 4 (“alt_text”) contains alternative text for figures. For
    tables, this column will be blank.

  | label | type | caption | alt_text |
  |----|----|----|----|
  | example_fig | figure | Example caption for figure. | Example alternative text for figure. |
  | example_tab | table | Example caption for table. |  |

  Format for csv containing table and figure captions and alternative
  text. {.table .caption-top style="width:98%;"}

  > **NOTE**: Only the *alternative text* will be used in your final
  > report- not the caption. This happens because of the workaround
  > workflow that allows us to add alt text into PDFs. The caption will
  > be taken from your directly-coded image’s markdown code, so you can
  > leave it empty or put the same caption here; it doesn’t matter.

  #### Add custom tables and figures

  In your ‘report’ folder, open your figures and tables docs (probably
  named something like ‘08_tables.qmd’ and ‘09_figures.qmd’). Add the
  following code there.

  **IMPORTANT**: The label in your captions/alt text csv must match the
  label in your plot’s chunk options, **MINUS the “fig-” or “tab-”**.
  For example, the label “your_image_label_name” in the csv would match
  up properly with the “tbl-your_image_label_name” label in this code:

  ``` rmd
  ![Your caption here](your_image.png){width=4in #fig-your_image_label_name}
  ```

  The same rule applies for tables: “custom_table2” in the csv would
  match up with “tbl-custom_table2” in the markdown’s label.

  ##### Identify page orientation

  If you’re using this workflow, your table/figure should be at most 8”
  wide or can be shrunken to 8” wide and still be legible. Find the
  width of your image file. Would you prefer to show it on a page with a
  portrait orientation (can fit 5” width) or landscape orientation (can
  fit 8” width)?

  Identify the preferred width of your image. Keep it in mind for when
  you add your image in the next section.

  ##### Add image

  Next, add your image in this format ([see this Quarto documentation
  for more
  information](https://quarto.org/docs/authoring/figures.html)):

  ``` markdown
  ![Your caption here](your_image.png){width=___in #fig-your_image_label_name}
  ```

  > **NOTE**: Captions and alternative text in this markdown format will
  > appear in your final report file. However, we **strongly encourage**
  > adding the caption and alt text in the captions_alt_text.csv file as
  > well, where your other figures and tables’ captions and alt text are
  > stored, for reproducibility and clarity when reusing report
  > templates.

  > **NOTE**: It can be a headache to specify the correct image path. We
  > recommend placing your files in either of these places:

  1.  Your report folder (path = the image name, like *image.png*)
  2.  A folder within your report folder (path = the relative filepath,
      like *images/image.png* if there’s an images folder within report)
  3.  A higher-level folder that can be referenced by formatting a
      filepath with two periods and a forward slash (../). This
      indicates “the parent folder of the current directory”.

  Here are some examples, using the following example folder structure
  setup:

  ``` markdown
  stock_assessments_2025/
  ├── captions_alt_text.csv
  ├── rockfish.jpg
  ├── report/
  │   ├── 01_executive_summary.qmd
  │   ├── 02_introduction.qmd
  │   ├── hake.png
  │   ├── images/
  │   │   ├── sardine.png
  ```

  ``` markdown
  # Example 1
  ![Here is my caption to a hake figure.](hake.png){width=3in #fig-hake}
  ```

  ``` markdown
  # Example 2
  ![Here is my caption to a sardine table.](images/sardine.png){width=5in #tab-sardine}
  ```

  ``` markdown
  # Example 3
  ![Here is my caption to a rockfish figure.](../rockfish.jpg){width=5in #fig-rockfish}
  ```

  ##### Add landscape braces (optional)

  If you’d like to place your image on a landscape-oriented page, you’ll
  add [landscape
  braces](https://quarto.org/docs/authoring/article-layout.html#landscape-mode)
  before and after your table/figure to ensure it will be displayed
  properly, like this:

  ``` default
  ::: {.landscape}
  ```

  ``` rmd
  ![Your caption here](your_image.png){width=___in #fig-your_image_label_name}
  ```

  Then, add three colons to close the braces:

  ``` default
  :::
  ```

  > **NOTE**: Landscape braces only work in [Quarto
  > v1.6+](https://quarto.org/docs/blog/posts/2024-11-25-1.6-release/).
  > Please read [this
  > Discussion](https://github.com/nmfs-ost/asar/discussions/186) to
  > learn how to check your Quarto version and update it if necessary.

  ##### Page break

  If you are including more than one custom table or figure in a file,
  we suggest adding a page break to separate the tables/figures by
  adding this on a new line: “{{\< pagebreak \>}}”

1.  Code your custom tables and figures
2.  Write captions and alternative text
3.  Save captions and alternative text
4.  Export your custom tables and figures
5.  Identify table width, orientation, and if splitting is necessary
6.  Add custom tables and figures

#### Code your custom tables and figures

First, write the code to produce (but not export; that’s a future step!)
your custom tables and figures. Save each table and figure as an object
(e.g., in the examples below, `your_table` and `your_figure`). Save the
script somewhere safe.

``` r

# Example dataframe
your_df <- data.frame(
  "x" = c(1, 2, 3),
  "y" = c(4, 5, 6)
)

# Example table
your_table <- gt::gt(your_df)

# Example figure
your_figure <- ggplot2::ggplot(
  your_df,
  aes(
    x = x,
    y = y
  )
) +
  ggplot2::geom_point()
```

#### Write captions and alternative text

Once you know what your tables and figures look like, write your own
captions and alternative text. Custom tables will require captions only.

##### Writing text

Check out the `stockplotr` [‘captions_alt_text_template.csv’
file](https://github.com/nmfs-ost/stockplotr/blob/master/inst/resources/captions_alt_text_template.csv),
which contains the templates for each table or figure’s
caption/alternative text, to get a sense of how each description is
structured. Then, refer to [the accessibility vignette’s
guide](https://nmfs-ost.github.io/asar/articles/accessibility_guide.html)
to learn how to write clear and comprehensive alternative text.

We recommend adding in key quantities, like maximum and minimum values,
to ensure your captions and alternative text clearly describe your
plots. For example, if you’re showing how biomass changed over time, we
recommend providing the maximum and minimum values of biomass and years
shown, instead of simply stating that a figure shows biomass changing
over time. Check out the [“How captions and alternative text are
generated”
vignette](https://nmfs-ost.github.io/stockplotr/articles/how-caps-alttext-are-made.html)
to see how we extract certain key quantities from the model results
files. You are welcome to use and adapt that code to your own results to
calculate key quantities, or calculate the quantities with your own
code.

##### Saving text

Do you have an existing captions_alt_text.csv file? If you’ve already
generated a figure or table rda, this file should exist, most likely in
your working directory. If you don’t have one, run a `stockplotr`
plotting function (e.g., `plot_biomass(make_rda = TRUE)`,
`table_bnc(make_rda = TRUE)`) to generate the file.

Open captions_alt_text.csv file, then add your text. The file is set up
like this:

- Column 1 (“label”) is a shorthand label for your figure or table.
  Labels should be somewhat short and exclude spaces. Examples include
  “kobe”, “relative.biomass”, and “fishing.mortality”.
- Column 2 (“type”) contains “figure” or “table”.
- Column 3 (“caption”) contains your caption.
- Column 4 (“alt_text”) contains alternative text for figures. For
  tables, this column will be blank.

| label | type | caption | alt_text |
|----|----|----|----|
| example_fig | figure | Example caption for figure. | Example alternative text for figure. |
| example_tab | table | Example caption for table. |  |

Format for csv containing table and figure captions and alternative
text. {.table .caption-top style="width:98%;"}

#### Export your custom tables and figures

Next, export your tables and figures to rda files. This code was adapted
from functions like
[`stockplotr::plot_biomass`](https://noaa-fisheries-integrated-toolbox.r-universe.dev/stockplotr/reference/plot_biomass.html).

##### Import pre-written captions and alternative text

Import your csv containing your captions and alternative text and save
it as an object. Then, save the captions and alternative text as
separate objects. For example:

``` r

# Import captions and alternative text
my_text <- read.csv(file = "captions_alt_text.csv")

# Extract table caption
my_table_cap <- my_text |>
  dplyr::filter(
    label == "example_table",
    type == "table"
  ) |>
  dplyr::select(caption) |>
  as.character()

# Extract figure caption
my_figure_cap <- my_text |>
  dplyr::filter(
    label == "example_fig",
    type == "figure"
  ) |>
  dplyr::select(caption) |>
  as.character()

# Extract figure alternative text
my_figure_alt_text <- my_text |>
  dplyr::filter(
    label == "example_fig",
    type == "figure"
  ) |>
  dplyr::select(alt_text) |>
  as.character()
```

> **NOTE**: Your label must match the label associated with your table
> or figure in your csv containing the captions and alternative text. If
> it does not match, the label will not be extracted properly and added
> to your rdas.

##### Make rdas

Your rdas will contain your table or figure, caption, and alternative
text (if it’s a figure).

###### Tables

``` r

rda <- list(
  "table" = your_table, # your table object
  "caption" = my_table_cap # your table caption
)
```

###### Figures

``` r

rda <- list(
  "figure" = your_figure, # your figure object
  "caption" = my_figure_cap, # your figure caption
  "alt_text" = my_figure_alt_text # your figure alternative text
)
```

##### Make ‘figures’ and ‘tables’ folders (if needed)

Do you already have ‘figures’ and ‘tables’ folders? *Hint: both should
be in your working directory.* If yes: Great! You can skip this step. If
not, simply enter the code below to make the folders.

``` r

dir.create(fs::path(getwd(), "figures"))
dir.create(fs::path(getwd(), "tables"))
```

##### Export your rdas

###### Tables

This will produce an rda called “example_table.rda” and save it in your
‘tables’ folder.

**IMPORTANT**: Your table must have “\_table” before the .rda file
extension. For example, “custom_table1.rda” wouldn’t work, but
“custom1_table.rda” would work.

``` r

save(rda,
  file = fs::path(
    getwd(), # this is your tables directory
    "tables",
    paste0(
      "example_table.rda" # your table's label
    )
  )
)
```

###### Figures

This will produce an rda called “example_figure.rda” and save it in your
‘figures’ folder.

**IMPORTANT**: Your figure must have “\_figure” before the .rda file
extension. For example, “custom_figure1.rda” wouldn’t work, but
“custom1_figure.rda” would work.

``` r

save(rda,
  file = fs::path(
    getwd(), # this is your figures directory
    "figures",
    paste0(
      "example_figure.rda" # your figure's label
    )
  )
)
```

#### Identify table width, length, orientation, and if splitting is necessary

*Skip this section if you are not including custom tables.*

Your next step is to get a sense of your table dimensions. You’ll ask:

- Will each fit neatly into a portrait orientation, or should the page
  be rotated 90 degrees into landscape view?
- Even in landscape view, is the table too wide to be shown properly,
  and must be split into smaller tables by column?
- Is the table too long for one page, and must be split into smaller
  tables by row?

This section contains the steps to answer these questions.

> **NOTE**: This workflow is reflected in the create_tables_doc.R file
> and its associated functions (such as
> [`render_lg_table()`](nmfs-ost.github.io/asar/reference/render_lg_table.md),
> [`ID_tbl_length_class()`](nmfs-ost.github.io/asar/reference/ID_tbl_length_class.md),
> [`ID_tbl_width_class()`](nmfs-ost.github.io/asar/reference/ID_tbl_width_class.md),
> and
> [`export_split_tbls()`](nmfs-ost.github.io/asar/reference/export_split_tbls.md)).

Run this code:

``` r

my_plot_name <- "example_table.rda" # replace this with the filename of your table rda
tables_dir <- getwd() # replace this with the location of your tables folder

# Identify table width class
tbl_orient <- ID_tbl_width_class(
  plot_name = gsub("_table.rda", "", my_plot_name),
  tables_dir = tables_dir,
  portrait_pg_width = 5 # 5 inches = the threshold for maximum table width before it needs to be resized, rotated, and/or split
)

# Identify table length class
tbl_length <- ID_tbl_length_class(
  plot_name = gsub("_table.rda", "", my_plot_name),
  tables_dir = tables_dir
)

table_specs <- list(tbl_orient, tbl_length)

# Identify table dimension class (length x width)
tbl_class <- dplyr::case_when(
  table_specs[[1]] == "regular" & table_specs[[2]] == "regular" ~ "reg_reg",
  table_specs[[1]] == "regular" & table_specs[[2]] == "long" ~ "reg_long",
  table_specs[[1]] == "wide" & table_specs[[2]] == "regular" ~ "wide_reg",
  table_specs[[1]] == "wide" & table_specs[[2]] == "long" ~ "wide_long",
  table_specs[[1]] == "extra-wide" & table_specs[[2]] == "regular" ~ "ewide_reg",
  table_specs[[1]] == "extra-wide" & table_specs[[2]] == "long" ~ "ewide_long",
  TRUE ~ "unknown"
)

max_rows <- ifelse(tbl_orient == "regular", 38, 28)

print(paste0("Your table dimension is ", tbl_class, ". The maximum number of table rows per page is ", max_rows, "."))

tables_path <- fs::path(
  tables_dir,
  "tables",
  my_plot_name
)

load(tables_path)

# Get table length in rows
split_table_rows <- length(rda[[1]]$`_data`[[1]])
split_tables_rowwise <- ceiling(split_table_rows / max_rows)

if (tbl_length == "long") {
  print(paste0("Your table must be split row-wise. Each table can have, at most, ", max_rows, "rows."))
}
if (tbl_orient == "extra-wide") {
  print(paste0("Your table must be split column-wise. Proceed to the section 'Table splitting' (regarding extra-wide tables) to identify the number of smaller tables your original table must be split into."))
}
```

You should see one of six outputs: reg_reg, reg_long, wide_reg,
wide_long, ewide_reg, or ewide_long.

|  |  |  |  |  |  |  |
|----|:--:|:--:|:--:|----|----|----|
| **Classification** | reg_reg | reg_long | wide_reg | wide_long | ewide_reg | ewide_long |
| **Dimensions** | Regular width & length | Regular width, long | Wide, regular length | Wide, long | Extra-wide, regular length | Extra-wide, long |
| **Table width** | \< 5” wide | \< 5” wide | at least 5” and up to 12” wide | at least 5” and up to 12” wide | \>12” wide | \>12” wide |
| **Table length**[^1] | \<= 38 rows | \> 38 rows | \<= 28 rows | \> 28 rows | \<= 28 rows | \> 28 rows |

Table width and length classification system coded in the
`create_tables_doc` function. {.table .caption-top style="width:100%;"}

**Regular-length tables**

*If your output is reg_reg*, you can display your table in a .qmd chunk
without any additional steps. Jump to section [Add custom tables and
figures](#sec-add-plots) and proceed.

*If your output is wide_reg*, you will place your table’s .qmd chunk in
special braces that change the page orientation to landscape. Jump to
section [Add custom tables and figures](#sec-add-plots) and proceed.

*If your output is ewide_reg*, you will place your table’s .qmd chunk in
special braces that change the page orientation to landscape *and* split
it into narrower tables, each of which are displayed on separate pages.
Proceed to the section [Table splitting](#sec-table-splitting).

**Long tables**

*If your output is reg_long*, you will split your table into shorter
tables, each of which are displayed on separate pages. Proceed to the
section [Table splitting](#sec-table-splitting).

*If your output is wide_long*, you will place your table’s .qmd chunk in
special braces that change the page orientation to landscape *and* split
your table into shorter tables, each of which are displayed on separate
pages. Proceed to the section [Table splitting](#sec-table-splitting).

*If your output is ewide_long*, you will place your table’s .qmd chunk
in special braces that change the page orientation to landscape; split
it into narrower tables, each of which are displayed on separate pages;
*and* split your table into shorter tables, each of which are displayed
on separate pages. Proceed to the section [Table
splitting](#sec-table-splitting).

##### Table splitting

###### Extra-wide tables

Extra-wide tables will need to be split among multiple pages. Run this
code to learn the number of tables into which your original table will
be split (by column) *and* export the split tables (as a list) into a
new rda:

``` r

my_plot_name <- "example_table.rda" # replace this with the filename of your table rda
tables_dir <- getwd() # replace this with the location of your tables folder
essential_cols <- 1:2 # replace this with the columns that will be retained between the split tables, formatted as a sequence (e.g., 1:2 for columns 1-2, or 1 for a single column)

export_split_tbls(
  tables_dir = tables_dir,
  plot_name = my_plot_name,
  essential_columns = essential_cols
)
```

If the number returned was, say, 5, then since each split table must be
in its own chunk, you will need 5 chunks to show your split tables by
column (if your table is *ewide-long*, then you must also split tables
by row). You will also need an additional chunk to load in the data.
Each table needs its own chunk, and therefore label, so that it can be
cross-referenced in text.

You can check out your split tables in the new rda saved in your
tables_dir folder. It will have the same filename as your original rda
except that it will contain “\_split” in the filename. For instance,
split tables created from “example_table.rda” will be saved in
“example_table_table_split.rda”.

###### Long tables

Long tables will need to be split among multiple pages. Refer to the
code chunk above to learn the number of tables into which your original
table will be split by row.

If the number returned was, say, 3, then since each split table must be
in its own chunk, you will need 3 chunks to show your split tables by
row. You will also need an additional chunk to load in the data. Each
table needs its own chunk, and therefore label, so that it can be
cross-referenced in text.

###### Extra-wide, long tables

Extra-wide, long tables will need to be split by column *and* row. Read
both “Extra-wide tables” and “Long tables” sections above, then proceed
to the next section for guidance.

#### Add custom tables and figures

This step will alter your figures and tables docs (probably named
something like ‘08_tables.qmd’ and ‘09_figures.qmd’), located in your
‘report’ folder.

As a reminder, this workflow assumes that you have tables and/or figures
stored in rda files. If you don’t have those rda files, please see the
[Export your custom tables and figures](#sec-export-custom-figs-tables)
section to create them.

**IMPORTANT**: The label in your captions/alt text csv must match the
label in your plot’s chunk options, **MINUS the “fig-” or “tab-”**. For
example, the label “custom_table3” in the csv would match up properly
with the “tbl-custom_table3” label in a code chunk. The same rule
applies for figures: “custom_figure1” in the csv would match up with
“fig-custom_figure1” in the chunk options’ label.

##### Figures

To add figures, first ensure that your folder containing the figures is
called “figures”.

Then, there are only two arguments to fill out:

1.  `subdir`: The location where the *new* figures doc should be saved
    (we recommend your ‘report’ folder, so it overwrites the old, empty
    version)
2.  `figures_dir`: The location of your “figures” folder

``` r

create_figures_doc(
  subdir = fs::path(getwd(), "report"), # indicates the new figures doc will be saved in your "report" folder, located in the working directory
  figures_dir = getwd() # indicates your "figures" folder is located in the working directory
)
```

In your figures doc, you’ll notice the following structure:

**Chunk 1** will always create an object saving the location of your
`figures_dir`.

Then, there will be at least two chunks for each figure. For each
figure,

**Chunk 2** will:

- load an rda containing a figure
- give the rda a specific name
- save the figure, caption, and alternative text as separate objects

**Chunk 3** will:

- display the figure

##### Tables

Adding tables entails nearly the same process as described for figures.
The only differences are:

1.  Tables will involve captions only, whereas figure require captions
    and alternative text
2.  Tables can be rotated or split across pages. See the [direct
    coding-in-qmd workflow (Option 3)](#sec-add-plots_coding) to learn
    about the logic within
    [`create_tables_doc()`](nmfs-ost.github.io/asar/reference/create_tables_doc.md)
    and why some tables are split into several code chunks.

To add tables to the {asar} tables doc:

``` r

create_tables_doc(
  subdir = fs::path(getwd(), "report"),
  tables_dir = getwd()
)
```

1.  Code your custom tables and figures
2.  Write captions and alternative text
3.  Identify table width, orientation, and if splitting is necessary
4.  Add custom tables and figures

#### Code your custom tables and figures

First, write the code to produce (but not export; that’s a future step!)
your custom tables and figures. Save each table and figure as an object
(e.g., in the examples below, `your_table` and `your_figure`). Save the
script somewhere safe.

``` r

# Example dataframe
your_df <- data.frame(
  "x" = c(1, 2, 3),
  "y" = c(4, 5, 6)
)

# Example table
your_table <- gt::gt(your_df)

# Example figure
your_figure <- ggplot2::ggplot(
  your_df,
  aes(
    x = x,
    y = y
  )
) +
  ggplot2::geom_point()
```

#### Write captions and alternative text

Once you know what your tables and figures look like, write your own
captions and alternative text. Custom tables will require captions only.

##### Writing text

Check out the `stockplotr` [‘captions_alt_text_template.csv’
file](https://github.com/nmfs-ost/stockplotr/blob/master/inst/resources/captions_alt_text_template.csv),
which contains the templates for each table or figure’s
caption/alternative text, to get a sense of how each description is
structured. Then, refer to [the accessibility vignette’s
guide](https://nmfs-ost.github.io/asar/articles/accessibility_guide.html)
to learn how to write clear and comprehensive alternative text.

We recommend adding in key quantities, like maximum and minimum values,
to ensure your captions and alternative text clearly describe your
plots. For example, if you’re showing how biomass changed over time, we
recommend providing the maximum and minimum values of biomass and years
shown, instead of simply stating that a figure shows biomass changing
over time. Check out the [“How captions and alternative text are
generated”
vignette](https://nmfs-ost.github.io/stockplotr/articles/how-caps-alttext-are-made.html)
to see how we extract certain key quantities from the model results
files. You are welcome to use and adapt that code to your own results to
calculate key quantities, or calculate the quantities with your own
code.

#### Identify table width, length, orientation, and if splitting is necessary

*Skip this section if you are not including custom tables.*

Your next step is to get a sense of your table dimensions. You’ll ask:

- Will each fit neatly into a portrait orientation, or should the page
  be rotated 90 degrees into landscape view?
- Even in landscape view, is the table too wide to be shown properly,
  and must be split into smaller tables by column?
- Is the table too long for one page, and must be split into smaller
  tables by row?

This section contains the steps to answer these questions.

> **NOTE**: This workflow is reflected in the create_tables_doc.R file
> and its associated functions (such as
> [`render_lg_table()`](nmfs-ost.github.io/asar/reference/render_lg_table.md),
> [`ID_tbl_length_class()`](nmfs-ost.github.io/asar/reference/ID_tbl_length_class.md),
> [`ID_tbl_width_class()`](nmfs-ost.github.io/asar/reference/ID_tbl_width_class.md),
> and
> [`export_split_tbls()`](nmfs-ost.github.io/asar/reference/export_split_tbls.md)).

Run this code:

``` r

my_plot_name <- "example_table.rda" # replace this with the filename of your table rda
tables_dir <- getwd() # replace this with the location of your tables folder

# Identify table width class
tbl_orient <- ID_tbl_width_class(
  plot_name = gsub("_table.rda", "", my_plot_name),
  tables_dir = tables_dir,
  portrait_pg_width = 5 # 5 inches = the threshold for maximum table width before it needs to be resized, rotated, and/or split
)

# Identify table length class
tbl_length <- ID_tbl_length_class(
  plot_name = gsub("_table.rda", "", my_plot_name),
  tables_dir = tables_dir
)

table_specs <- list(tbl_orient, tbl_length)

# Identify table dimension class (length x width)
tbl_class <- dplyr::case_when(
  table_specs[[1]] == "regular" & table_specs[[2]] == "regular" ~ "reg_reg",
  table_specs[[1]] == "regular" & table_specs[[2]] == "long" ~ "reg_long",
  table_specs[[1]] == "wide" & table_specs[[2]] == "regular" ~ "wide_reg",
  table_specs[[1]] == "wide" & table_specs[[2]] == "long" ~ "wide_long",
  table_specs[[1]] == "extra-wide" & table_specs[[2]] == "regular" ~ "ewide_reg",
  table_specs[[1]] == "extra-wide" & table_specs[[2]] == "long" ~ "ewide_long",
  TRUE ~ "unknown"
)

max_rows <- ifelse(tbl_orient == "regular", 38, 28)

print(paste0("Your table dimension is ", tbl_class, ". The maximum number of table rows per page is ", max_rows, "."))

tables_path <- fs::path(
  tables_dir,
  "tables",
  my_plot_name
)

load(tables_path)

# Get table length in rows
split_table_rows <- length(rda[[1]]$`_data`[[1]])
split_tables_rowwise <- ceiling(split_table_rows / max_rows)

if (tbl_length == "long") {
  print(paste0("Your table must be split row-wise. Each table can have, at most, ", max_rows, "rows."))
}
if (tbl_orient == "extra-wide") {
  print(paste0("Your table must be split column-wise. Proceed to the section 'Table splitting' (regarding extra-wide tables) to identify the number of smaller tables your original table must be split into."))
}
```

You should see one of six outputs: reg_reg, reg_long, wide_reg,
wide_long, ewide_reg, or ewide_long.

|  |  |  |  |  |  |  |
|----|:--:|:--:|:--:|----|----|----|
| **Classification** | reg_reg | reg_long | wide_reg | wide_long | ewide_reg | ewide_long |
| **Dimensions** | Regular width & length | Regular width, long | Wide, regular length | Wide, long | Extra-wide, regular length | Extra-wide, long |
| **Table width** | \< 5” wide | \< 5” wide | at least 5” and up to 12” wide | at least 5” and up to 12” wide | \>12” wide | \>12” wide |
| **Table length**[^2] | \<= 38 rows | \> 38 rows | \<= 28 rows | \> 28 rows | \<= 28 rows | \> 28 rows |

Table width and length classification system coded in the
`create_tables_doc` function. {.table .caption-top style="width:100%;"}

**Regular-length tables**

*If your output is reg_reg*, you can display your table in a .qmd chunk
without any additional steps. Jump to section [Add custom tables and
figures](#sec-add-plots) and proceed.

*If your output is wide_reg*, you will place your table’s .qmd chunk in
special braces that change the page orientation to landscape. Jump to
section [Add custom tables and figures](#sec-add-plots) and proceed.

*If your output is ewide_reg*, you will place your table’s .qmd chunk in
special braces that change the page orientation to landscape *and* split
it into narrower tables, each of which are displayed on separate pages.
Proceed to the section [Table splitting](#sec-table-splitting).

**Long tables**

*If your output is reg_long*, you will split your table into shorter
tables, each of which are displayed on separate pages. Proceed to the
section [Table splitting](#sec-table-splitting).

*If your output is wide_long*, you will place your table’s .qmd chunk in
special braces that change the page orientation to landscape *and* split
your table into shorter tables, each of which are displayed on separate
pages. Proceed to the section [Table splitting](#sec-table-splitting).

*If your output is ewide_long*, you will place your table’s .qmd chunk
in special braces that change the page orientation to landscape; split
it into narrower tables, each of which are displayed on separate pages;
*and* split your table into shorter tables, each of which are displayed
on separate pages. Proceed to the section [Table
splitting](#sec-table-splitting).

##### Table splitting

###### Extra-wide tables

Extra-wide tables will need to be split among multiple pages. Run this
code to learn the number of tables into which your original table will
be split (by column) *and* export the split tables (as a list) into a
new rda:

``` r

my_plot_name <- "example_table.rda" # replace this with the filename of your table rda
tables_dir <- getwd() # replace this with the location of your tables folder
essential_cols <- 1:2 # replace this with the columns that will be retained between the split tables, formatted as a sequence (e.g., 1:2 for columns 1-2, or 1 for a single column)

export_split_tbls(
  tables_dir = tables_dir,
  plot_name = my_plot_name,
  essential_columns = essential_cols
)
```

If the number returned was, say, 5, then since each split table must be
in its own chunk, you will need 5 chunks to show your split tables by
column (if your table is *ewide-long*, then you must also split tables
by row). You will also need an additional chunk to load in the data.
Each table needs its own chunk, and therefore label, so that it can be
cross-referenced in text.

You can check out your split tables in the new rda saved in your
tables_dir folder. It will have the same filename as your original rda
except that it will contain “\_split” in the filename. For instance,
split tables created from “example_table.rda” will be saved in
“example_table_table_split.rda”.

###### Long tables

Long tables will need to be split among multiple pages. Refer to the
code chunk above to learn the number of tables into which your original
table will be split by row.

If the number returned was, say, 3, then since each split table must be
in its own chunk, you will need 3 chunks to show your split tables by
row. You will also need an additional chunk to load in the data. Each
table needs its own chunk, and therefore label, so that it can be
cross-referenced in text.

###### Extra-wide, long tables

Extra-wide, long tables will need to be split by column *and* row. Read
both “Extra-wide tables” and “Long tables” sections above, then proceed
to the next section for guidance.

#### Add custom tables and figures

In your ‘report’ folder, open your figures and tables docs (probably
named something like ‘08_tables.qmd’ and ‘09_figures.qmd’). Add the
following code there.

**IMPORTANT**: The label in your captions/alt text csv must match the
label in your plot’s chunk options, **MINUS the “fig-” or “tab-”**. For
example, the label “custom_table1” in the csv would match up properly
with the “tbl-custom_table1” label in a code chunk. The same rule
applies for figures: “custom_figure1” in the csv would match up with
“fig-custom_figure1” in the chunk options’ label.

##### Tables

Follow the instructions in the appropriate section below based on your
table classification: [reg_reg](#sec-rr), [reg_long](#sec-long),
[wide_reg](#sec-wr), [wide_long](#sec-long), [ewide_reg](#sec-er), or
[ewide_long](#sec-el).

For all table types, add a setup chunk to define your table and its
caption:

```` default
```{r}
#| label: 'tab-setup-custom'
#| include: false

# Example dataframe
your_df <- data.frame(
  "x" = c(1, 2, 3),
  "y" = c(4, 5, 6)
)

# Example table
your_table <- gt::gt(your_df)

# Define your caption
your_cap <- "This is the caption for my custom table."
```
````

###### Regular tables (`reg_reg`)

*Portrait orientation, fits on one page.*

Add this chunk to display the table:

```` default
```{r}
#| label: 'tbl-custom1'
#| echo: false
#| warning: false
#| tbl-cap: !expr your_cap

your_table
```
````

###### Wide tables (`wide_reg`)

*Landscape orientation, fits on one page.*

Wrap the chunk in landscape braces:

```` default
::: {.landscape}

```{r}
#| label: 'tbl-custom-wide'
#| echo: false
#| warning: false
#| tbl-cap: !expr your_cap

your_table |>
  gt::tab_options(table.width = pct(100), table.layout = "auto") |>
  gt::cols_width(everything() ~ pct(20))
```

:::
````

###### Long tables (`reg_long` or `wide_long`)

*Splits by row across multiple pages. `wide_long` uses landscape;
`reg_long` uses portrait.*

You will need a separate chunk for each page of the split. For example,
if your table requires 2 pages, repeat this chunk twice, changing the
index in `grp_pull(1)` to `grp_pull(2)`.

Also, notice how
[`gt_split()`](nmfs-ost.github.io/asar/reference/gt_split.md) extracts
subsets of rows from the table to ensure it fits on one page. This
function is from the excellent `gt` package. There is a bug preventing
the `gt` function from working as expected, so we filed an issue and
submitted [a pull request that fixes the
issue](https://github.com/rstudio/gt/pull/2132). Until it’s resolved,
we’ve added the fixed function to {asar} so users can access it via
[`asar::gt_split()`](nmfs-ost.github.io/asar/reference/gt_split.md).

```` default
```{r}
#| label: 'tbl-custom-long-p1'
#| echo: false
#| tbl-cap: !expr paste0(your_cap, ' (Page 1 of 2)')

your_table |>
  gt::tab_options(table.width = pct(100), table.layout = "auto") |>
  gt::cols_width(everything() ~ pct(20)) |>
  asar::gt_split(row_every_n = 38) |> # Use 28 for wide_long
  gt::grp_pull(1) # Change this to 2 for the next page
```
````

> **NOTE**: For `wide_long`, wrap the chunk in `::: {.landscape}`
> braces. For example:

``` default
::: {.landscape}
# code chunk shown above
:::
```

###### Extra-wide tables (`ewide_reg`)

*Landscape orientation, split by column across multiple pages.*

Because extra-wide tables are split by column, you must have already run
[`export_split_tbls()`](https://github.com/nmfs-ost/asar/blob/main/R/export_split_tbls.R)
to create the `_split.rda` file. Load that split file to access the list
of tables:

```` default
```{r}
#| label: 'tbl-setup-ewide-split'
#| include: false
# Load the list of split tables generated by export_split_tbls()
load(file.path(tables_dir, "example_tab_table_split.rda"))
table_list_split <- table_list
```
````

Then, for each split table in the list, create a landscape chunk:

```` default
::: {.landscape}

```{r}
#| label: 'tbl-custom-ewide-c1'
#| echo: false
#| tbl-cap: !expr paste0(your_cap, ' (Column split 1 of 3)')

table_list_split[[1]] |>
  gt::tab_options(table.width = pct(100), table.layout = "auto") |>
  gt::cols_width(everything() ~ pct(20))
```

:::
````

###### Extra-wide, long tables (`ewide_long`)

*Landscape orientation, split by both column AND row.*

This requires a nested approach using the `table_list_split` loaded
above. You must create chunks for every combination of column-split and
row-split.

```` default
::: {.landscape}

```{r}
#| label: 'tbl-custom-ewide-long-c1-r1'
#| echo: false
#| tbl-cap: !expr paste0(your_cap, ' (Column split 1 of 2, Row split 1 of 3)')

table_list_split[[1]] |>
  gt::tab_options(table.width = pct(100), table.layout = "auto") |>
  gt::cols_width(everything() ~ pct(20)) |>
  asar::gt_split(row_every_n = 28) |>
  gt::grp_pull(1)
```

:::
````

The next chunk would be identical except for a different label, caption,
and would have `gt::grp_pull(2)` instead of `gt::grp_pull(1)`, for
instance. When adding the second split table, add
`table_list_split[[2]]` instead of `table_list_split[[1]]`, then change
the label, caption, and
[`gt::grp_pull()`](https://gt.rstudio.com/reference/grp_pull.html) as
needed.

###### Final step: page break

If you are adding another table, add a page break by adding this on a
new line: “{{\< pagebreak \>}}”

##### Figures

The workflow for figures is very similar to the workflow for [regular
tables](#sec-rr): 1) add a chunk to define your figure, its caption, and
its alternative text, and then 2) add a chunk to display the figure.

In your figures doc, add the following chunk to display the figure in
your .qmd when it’s rendered:

```` default
```{r} 
#| label: 'fig-custom1' # choose a label
#| echo: false
#| warning: false
#| fig-cap: !expr This is your figure caption.
#| fig-alt: !expr This is your figure alternative text.
# Import your data
your_df <- data.frame(
  "x" = c(1, 2, 3),
  "y" = c(4, 5, 6)
)

# Make your figure
your_figure <- ggplot2::ggplot(
  your_df,
  aes(
    x = x,
    y = y
  )
) +
  ggplot2::geom_point()

# Show your figure
your_figure
``` 
````

If you are adding another figure, add a page break by adding this on a
new line: “{{\< pagebreak \>}}”

[^1]: The maximum number of rows drops from 38 (for regular-width
    tables) to 28 (for wide or extra-wide tables) because the latter two
    tables are rotated on a landscape page, which offers less space for
    table rows.

[^2]: The maximum number of rows drops from 38 (for regular-width
    tables) to 28 (for wide or extra-wide tables) because the latter two
    tables are rotated on a landscape page, which offers less space for
    table rows.
