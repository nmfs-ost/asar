#' Create Quarto Document of Tables
#'
#' @inheritParams create_template
#' @param subdir subdirectory where the assessment report template is being stored
#' @param include_all include all default tables for a stock assessment report
#'
#' @return Create a quarto document as part of a stock assessment outline with
#' pre-loaded R chunk adding the stock assessment tables from the nmfs-ost/satf R package
#' @export
#'
create_tables_doc <- function(subdir = NULL,
                              include_all = TRUE,
                              rda_dir = NULL) {
  if (include_all) {
    # add header
    tables_doc <- paste0("## Tables {#sec-tables}\n \n")

    # add chunk that creates object as the directory of all rdas
    tables_doc <- paste0(
      tables_doc,
      add_chunk(
        paste0("library(flextable)\n
rda_dir <- '", rda_dir, "/rda_files'"),
        label = "set-rda-dir-tbls",
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


    # Bnc table
    if (any(grepl("bnc_table.rda", list.files(file.path(rda_dir, "rda_files"))))) {
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

      ## add table
      tables_doc <- paste0(
        tables_doc,
        add_chunk(
          paste0("bnc_table"),
          label = "tbl-bnc-plot",
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

    # Indices table
    if (any(grepl("indices.abundance_table.rda", list.files(file.path(rda_dir, "rda_files"))))) {
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

      ## add table
      tables_doc <- paste0(
        tables_doc,
        add_chunk(
          paste0("indices_table"),
          label = "tbl-indices-plot",
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

    #     # landings table
    #     if (any(grepl("landings_table.rda", list.files(file.path(rda_dir, "rda_files"))))) {
    #     ## import table, caption
    #     tables_doc <- paste0(
    #       tables_doc,
    #       add_chunk(
    #         paste0("# if the landings table rda exists:
    # if (file.exists(file.path(rda_dir, 'landings_table.rda'))){\n
    #   # load rda
    #   load(file.path(rda_dir, 'landings_table.rda'))\n
    #   # save rda with plot-specific name
    #   landings_table_rda <- rda\n
    #   # remove generic rda object
    #   rm(rda)\n
    #   # save table, caption as separate objects; set eval to TRUE
    #   landings_table <- landings_table_rda$table
    #   landings_cap <- landings_table_rda$cap
    #   eval_landings <- TRUE\n
    # # if the landings table rda does not exist, don't evaluate the next chunk
    # } else {eval_landings <- FALSE}"
    #         ),
    #         label = "tbl-landings-setup",
    #         eval = "true",
    #         add_option = TRUE,
    #         chunk_op = c(
    #           glue::glue(
    #             "include: false"
    #           )
    #         )
    #       ),
    #       "\n"
    #     )
    #
    #     ## add table
    #     tables_doc <- paste0(
    #       tables_doc,
    #       add_chunk(
    #         paste0("landings_table"),
    #         label = "tbl-landings-plot",
    #         eval = "!expr eval_landings",
    #         add_option = TRUE,
    #         chunk_op = c(
    #           glue::glue(
    #             "tbl-cap: !expr if(eval_landings) landings_cap"
    #           ),
    #           glue::glue(
    #             "include: !expr eval_landings"
    #           ))
    #       ),
    #       "\n"
    #     )
    #     }

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
