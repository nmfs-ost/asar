#' Create Quarto Document of Tables
#'
#' @inheritParams create_template
#' @param subdir subdirectory where the assessment report template is being stored
#' @param include_all include all default tables for a stock assessment report
#'
#' @return Create a quarto document as part of a stock assessment outline with
#' pre-loaded R chunk adding the stock assessment tables from the nmfs-ost/stockplotr
#' R package. NOTE: If your table is too wide to print on a portrait-oriented page,
#' the page will be rotated to landscape view. If if is too wide to print in
#' landscape view, it will be split into multiple tables. In this case, a new rda
#' will be created and is identifiable by the phrase "split" in the filename (e.g.,
#' indices.abundance_table.rda will generate a new indices.abundance_table_split.rda
#' file). These tables will share the same caption.
#' @export
#'
create_tables_doc <- function(subdir = NULL,
                              include_all = TRUE,
                              rda_dir = NULL) {

  # set portrait page width (in)
  portrait_pg_width <- 5

  # set landscape page width (in)
  landscape_pg_width <- 8

  if (include_all) {
    # add header
    tables_doc <- paste0("## Tables {#sec-tables}\n \n")

    # add chunk that creates object as the directory of all rdas
    tables_doc <- paste0(
      tables_doc,
      add_chunk(
        paste0("library(flextable)\nrda_dir <- '", rda_dir, "/rda_files'"),
        label = "set-rda-dir-tbls",
        eval = "true",
        add_option = TRUE,
        chunk_op = "include: false"
      ),
      "\n"
    )


    # Bnc table-----
    plot_name.bnc <- "bnc_table.rda"
    if (any(grepl(plot_name.bnc, list.files(file.path(rda_dir, "rda_files"))))) {
      ## import table, caption
      tables_doc <- paste0(
        tables_doc,
        add_chunk(
          paste0("# if the bnc table rda exists:
if (file.exists(file.path(rda_dir, 'bnc_table.rda'))){\n
  # load rda
  load(file.path(rda_dir, 'bnc_table.rda'))\n
  # save rda with plot-specific name
  bnc_table_rda <- rda\n
  # remove generic rda object
  rm(rda)\n
  # save table, caption as separate objects; set eval to TRUE
  bnc_table <- bnc_table_rda$table
  bnc_cap <- bnc_table_rda$cap
  eval_bnc <- TRUE\n
# if the bnc table rda does not exist, don't evaluate the next chunk
} else {eval_bnc <- FALSE}"),
          label = "tbl-bnc-setup",
          eval = "true",
          add_option = TRUE,
          chunk_op = c(
            glue::glue(
              "include: false"
            )
          )
        ),
        "\n"
      )


      # identify table orientation
      bnc_orient <- ID_tbl_width_class(plot_name = plot_name.bnc,
                                       rda_dir = rda_dir,
                                       portrait_pg_width = portrait_pg_width)

      # add landscape braces before R chunk depending on table width
      if(bnc_orient != "regular"){
        tables_doc <- paste0(
          tables_doc,
          "::: {.landscape}\n\n"
        )
      }

      if(bnc_orient == "extra-wide"){
        # split extra-wide tables into smaller tables and export AND
        # identify number of split tables
        split_tables <- export_split_tbls(rda_dir = rda_dir,
                                          plot_name = plot_name.bnc,
                                          essential_columns = 1)

        # add a chunk to import new captions
        tables_doc <- paste0(
          tables_doc,
          add_chunk(
            paste0(
              "load(file.path(rda_dir, 'bnc_table_split.rda'))\n
# save rda with plot-specific name
bnc_table_split_rda <- table_list\n
# remove generic rda object
rm(table_list)\n
# extract table caption specifiers
bnc_cap_split <- names(bnc_table_split_rda)"
            )
            ,
            label = "tbl-bnc-labels",
            eval = "!expr eval_bnc",
            add_option = TRUE,
            chunk_op = c(
              glue::glue(
                "include: false"
              )
            )
          ),
          "\n"
        )

        # prepare text for chunk that will display split tables
        for (i in 1:as.numeric(split_tables)){

          # add a chunk for each table
          tables_doc <- paste0(
            tables_doc,
            add_chunk(
              paste0(
                "# plot split table ", i, "
bnc_table_split_rda[[", i, "]] |> flextable::fit_to_width(max_width = 8)\n"
              )
              ,
              label = paste0("tbl-bnc", i),
              eval = "!expr eval_bnc",
              add_option = TRUE,
              chunk_op = c(
                glue::glue(
                  "tbl-cap: !expr if(eval_bnc) paste0(bnc_cap, '(', bnc_cap_split[[", i, "]], ')')"
                ),
                glue::glue(
                  "include: !expr eval_bnc"
                )
              )
            ),
            "\n"
          )
        }
      } else {

        ## add table if it only requires one chunk
        tables_doc <- paste0(
          tables_doc,
          add_chunk(
            if (bnc_orient == "wide"){
              paste0(
                "bnc_table |>
                flextable::fit_to_width(max_width = 8)"
              )
            } else if (bnc_orient == "regular"){
              paste0("bnc_table")
            }
            ,
            label = "tbl-bnc",
            eval = "!expr eval_bnc",
            add_option = TRUE,
            chunk_op = c(
              glue::glue(
                "tbl-cap: !expr if(eval_bnc) bnc_cap"
              ),
              glue::glue(
                "include: !expr eval_bnc"
              )
            )
          ),
          "\n"
        )
      }

      # add landscape braces after R chunk depending on table width
      if(bnc_orient != "regular"){
        tables_doc <- paste0(
          tables_doc,
          ":::\n"
        )
      }

      # add page break after table plotted
      tables_doc <- paste0(
        tables_doc,
        "\n{{< pagebreak >}}\n"
      )
    }

    # Indices table-----
    plot_name.indices <- "indices.abundance_table.rda"
    if (any(grepl(plot_name.indices, list.files(file.path(rda_dir, "rda_files"))))) {
      ## import table, caption
      tables_doc <- paste0(
        tables_doc,
        add_chunk(
          paste0("# if the indices table rda exists:
if (file.exists(file.path(rda_dir, 'indices.abundance_table.rda'))){\n
  # load rda
  load(file.path(rda_dir, 'indices.abundance_table.rda'))\n
  # save rda with plot-specific name
  indices_table_rda <- rda\n
  # remove generic rda object
  rm(rda)\n
  # save table, caption as separate objects; set eval to TRUE
  indices_table <- indices_table_rda$table
  indices_cap <- indices_table_rda$cap
  eval_indices <- TRUE\n
# if the indices table rda does not exist, don't evaluate the next chunk
} else {eval_indices <- FALSE}"),
          label = "tbl-indices-setup",
          eval = "true",
          add_option = TRUE,
          chunk_op = c(
            glue::glue(
              "include: false"
            )
          )
        ),
        "\n"
      )

      # identify table orientation
      indices_orient <- ID_tbl_width_class(plot_name = plot_name.indices,
                                           rda_dir = rda_dir,
                                           portrait_pg_width = portrait_pg_width)

      # add landscape braces before R chunk depending on table width
      if(indices_orient != "regular"){
        tables_doc <- paste0(
          tables_doc,
          "::: {.landscape}\n\n"
        )
      }

      if(indices_orient == "extra-wide"){
        # split extra-wide tables into smaller tables and export AND
        # identify number of split tables
        split_tables <- export_split_tbls(rda_dir = rda_dir,
                                          plot_name = plot_name.indices,
                                          essential_columns = 1)

        # add a chunk to import new captions
        tables_doc <- paste0(
          tables_doc,
          add_chunk(
            paste0(
              "load(file.path(rda_dir, 'indices.abundance_table_split.rda'))\n
# save rda with plot-specific name
indices_table_split_rda <- table_list\n
# remove generic rda object
rm(table_list)\n
# extract table caption specifiers
indices_cap_split <- names(indices_table_split_rda)"
            )
            ,
            label = "tbl-indices-labels",
            eval = "!expr eval_indices",
            add_option = TRUE,
            chunk_op = c(
              glue::glue(
                "include: false"
              )
            )
          ),
          "\n"
        )

        # prepare text for chunk that will display split tables
        for (i in 1:as.numeric(split_tables)){

          # add a chunk for each table
          tables_doc <- paste0(
            tables_doc,
            add_chunk(
              paste0(
                "# plot split table ", i, "
indices_table_split_rda[[", i, "]] |> flextable::fit_to_width(max_width = 8)\n"
              )
              ,
              label = paste0("tbl-indices", i),
              eval = "!expr eval_indices",
              add_option = TRUE,
              chunk_op = c(
                glue::glue(
                  "tbl-cap: !expr if(eval_indices) paste0(indices_cap, '(', indices_cap_split[[", i, "]], ')')"
                ),
                glue::glue(
                  "include: !expr eval_indices"
                )
              )
            ),
            "\n"
          )
        }
      } else {

        ## add table if it only requires one chunk
        tables_doc <- paste0(
          tables_doc,
          add_chunk(
            if (indices_orient == "wide"){
              paste0(
                "indices_table |>
                flextable::fit_to_width(max_width = 8)"
              )
            } else if (indices_orient == "regular"){
              paste0("indices_table")
            }
            ,
            label = "tbl-indices",
            eval = "!expr eval_indices",
            add_option = TRUE,
            chunk_op = c(
              glue::glue(
                "tbl-cap: !expr if(eval_indices) indices_cap"
              ),
              glue::glue(
                "include: !expr eval_indices"
              )
            )
          ),
          "\n"
        )
      }

      # add landscape braces after R chunk depending on table width
      if(indices_orient != "regular"){
        tables_doc <- paste0(
          tables_doc,
          ":::\n"
        )
      }

      # add page break after table plotted
      tables_doc <- paste0(
        tables_doc,
        "\n{{< pagebreak >}}\n"
      )
    }

    # landings table----- # ADD WHEN READY

    # Add other tables follow the same above format
  } else {
    # add option for only adding specified tables
    warning("Functionality for adding specific tables is still in development. Please set 'include_all' to true and edit the 08_tables.qmd file to remove specific tables from the report.")
  }

  # Save tables doc to template folder
  utils::capture.output(cat(tables_doc),
                        file = paste0(subdir, "/", "08_tables.qmd"),
                        append = FALSE
  )
}
