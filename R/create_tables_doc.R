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
#' file), and column 1 will be repeated across split tables. These tables will
#' share the same caption. To specify a different repeated column(s), use
#' asar::export_split_tbls with your preferred essential_columns value.
#' @export
#'
#' @examples
#' \dontrun{
#' create_tables_doc(
#' subdir = getwd(),
#' rda_dir = here::here())
#' }
create_tables_doc <- function(subdir = getwd(),
                              include_all = TRUE,
                              rda_dir = getwd()) {

  # NOTE: essential_columns = 1 for all tables split using export_split_tbls() in
  # the code below.
  # To customize essential_columns, the user must run export_split_tbls() manually
  # and specify essential_columns. Then, the split table will be imported into
  # the tables doc as is.
  # Upon adding more tables to stockplotr, the code may need to be altered to
  # specify essential_columns for stockplotr-created tables.

  # set portrait page width (in)
  portrait_pg_width <- 5

  # set landscape page width (in)
  landscape_pg_width <- 8

  if (include_all == FALSE) {
    # add option for only adding specified tables
    warning("Functionality for adding specific tables is still in development. Please set 'include_all' to true and edit the 08_tables.qmd file to remove specific tables from the report.")
  } else {

    # add header
    tables_doc_header <- paste0("## Tables {#sec-tables}\n \n")

    # add chunk that creates object as the directory of all rdas
    tables_doc_setup <- paste0(
      add_chunk(
        paste0("library(flextable)\nrda_dir <- '", rda_dir, "/rda_files'"),
        label = "set-rda-dir-tbls",
        eval = "true",
        add_option = TRUE,
        chunk_op = "include: false"
      ),
      "\n"
    )

    tables_doc <- ""

    # list all files in rda_files
    file_list <- list.files(file.path(rda_dir, "rda_files"))

    # create sublist of only table files
    file_tab_list <- file_list[grepl("_table", file_list)]
    # create sublist of only rda table files
    rda_tab_list <- file_tab_list[grepl(".rda", file_tab_list)]

    # remove rda table files that have an associated "split" version
    # remove "_split" from filenames
    remove_split_names <- gsub("_split", "", rda_tab_list)
    # identify duplicates in remove_split_names
    dup_tab <- remove_split_names[duplicated(remove_split_names) | duplicated(remove_split_names, fromLast = TRUE)]
    # remove duplicates in remove_split_names to create final list
    final_rda_tab_list <- rda_tab_list[!(remove_split_names %in% dup_tab & !grepl("_split", rda_tab_list))]

    # create sublist of only non-rda table files
    non.rda_tab_list <- file_tab_list[!grepl(".rda", file_tab_list)]

    # create two-chunk system to plot each rda figure
    create_tab_chunks <- function(tab = NA,
                                  rda_dir = getwd()){

      # test whether table has been split
      split <- grepl("split", tab)

      tab_shortname <- ifelse(split,
                              stringr::str_remove(tab, "_table_split.rda"),
                              stringr::str_remove(tab, "_table.rda"))

      # identify table orientation
      # split tables will always be extra_wide
      tbl_orient <- ifelse(split,
                           "extra_wide",
                           ID_tbl_width_class(plot_name = tab_shortname,
                                              rda_dir = rda_dir,
                                              portrait_pg_width = portrait_pg_width))

      ## import table, caption, alt text
      ## do this for all tables
      tables_doc_plot_setup1 <- paste0(
        add_chunk(
          paste0(
"# if the table rda exists:
if (file.exists(file.path(rda_dir, '", stringr::str_remove(tab, "_split"), "'))){\n
# load rda
load(file.path(rda_dir, '", stringr::str_remove(tab, "_split"), "'))\n
# save rda with table-specific name\n",
tab_shortname, "_table_rda <- rda\n
# save table and caption as separate objects; set eval to TRUE\n",
tab_shortname, "_table <- ", tab_shortname, "_table_rda$table\n",
tab_shortname, "_cap <- ", tab_shortname, "_table_rda$cap
eval_", tab_shortname, " <- TRUE\n
# if the table rda does not exist, don't evaluate the next chunk
} else {eval_",  tab_shortname, " <- FALSE}"
          ),
          label = paste0("tab-", tab_shortname, "-setup"),
          eval = "true"
        ),
"\n"
)

      ## add table if it only requires one chunk
      if(tbl_orient == "regular"){
          tables_doc_plot_setup2 <- paste0(
            add_chunk(
              paste0(tab_shortname, "_table"),
              label = paste0("tbl-", tab_shortname),
              eval = paste0("!expr eval_", tab_shortname),
              add_option = TRUE,
              chunk_op = c(
                glue::glue(
                  "tbl-cap: !expr if(eval_", tab_shortname, ") ", tab_shortname, "_cap"
                ),
                glue::glue(
                  "include: !expr eval_", tab_shortname
                )
              )
            ),
            "\n"
          )
      }

      if(tbl_orient == "wide"){
        tables_doc_plot_setup2 <- paste0(
          # add landscape braces before R chunk
          "::: {.landscape}\n\n",
          add_chunk(
              paste0(
                tab_shortname, "_table |>
                flextable::fit_to_width(max_width = 8)"
              ),
            label = paste0("tbl-", tab_shortname),
            eval = paste0("!expr eval_", tab_shortname),
            add_option = TRUE,
            chunk_op = c(
              glue::glue(
                "tbl-cap: !expr if(eval_", tab_shortname, ") ", tab_shortname, "_cap"
              ),
              glue::glue(
                "include: !expr eval_", tab_shortname
              )
            )
          ),
          "\n",
          # add landscape braces after R chunk
          ":::\n"
        )
      }

       if(tbl_orient == "extra_wide"){
        if (split) {
          # identify number of split tables
          load(fs::path(rda_dir, "rda_files", tab))
          split_tables <- length(table_list)
        } else {
          # split extra_wide tables into smaller tables and export AND
          # identify number of split tables IF not already split
          split_tables <- export_split_tbls(rda_dir = rda_dir,
                                          plot_name = tab,
                                          essential_columns = 1)
        }

         # add a chunk to import split tables
         tables_doc_plot_setup2_import <- paste0(
           add_chunk(
             paste0(
               "load(file.path(rda_dir, '", tab, "'))\n
# save rda with plot-specific name\n",
               tab_shortname, "_table_split_rda <- table_list\n
# extract table caption specifiers\n",
               tab_shortname, "_cap_split <- names(", tab_shortname, "_table_split_rda)"
             )
             ,
             label = paste0("tbl-", tab_shortname, "-labels"),
             eval = paste0("!expr eval_", tab_shortname),
             add_option = TRUE,
             chunk_op = c(glue::glue("include: false"))
           ),
           "\n"
         )
        # prepare text for chunk that will display split tables
        tables_doc_plot_setup2_display <- ""
        for (i in 1:as.numeric(split_tables)){
          # add a chunk for each table
          tables_doc_plot_setup2_display <- paste0(
           tables_doc_plot_setup2_display,
            # add landscape braces before R chunk
            "::: {.landscape}\n\n",
            add_chunk(
              paste0(
                "# plot split table ", i, "\n",
                tab_shortname, "_table_split_rda[[", i, "]] |> flextable::fit_to_width(max_width = 8)\n"
              )
              ,
              label = paste0("tbl-", tab_shortname, i),
              eval = paste0("!expr eval_", tab_shortname),
              add_option = TRUE,
              chunk_op = c(
                glue::glue(
                  "tbl-cap: !expr if(eval_", tab_shortname, ") paste0(", tab_shortname, "_cap, '(', ", tab_shortname, "_cap_split[[", i, "]], ')')"
                ),
                glue::glue(
                  "include: !expr eval_", tab_shortname
                )
              )
            ),
            "\n",
            # add landscape braces after R chunk
            ":::\n"
          )
        }

        tables_doc_plot_setup2 <- paste0(
          tables_doc_plot_setup2_import,
          tables_doc_plot_setup2_display
        )
        }

      return(paste0(tables_doc_plot_setup1,
                    tables_doc_plot_setup2))

    }

    if (length(file_tab_list) == 0){
      message(paste0("Note: No table files were present in '", fs::path(rda_dir, "rda_files"), "'."))
      tables_doc <- "## Tables {#sec-tables}"
    } else {
      # paste rda table code chunks into one object
      if (length(final_rda_tab_list) > 0) {
        rda_tables_doc <- ""
        for (i in 1:length(final_rda_tab_list)){
          tab_chunk <- create_tab_chunks(tab = final_rda_tab_list[i],
                                         rda_dir = rda_dir)

          rda_tables_doc <- paste0(rda_tables_doc, tab_chunk)
        }
      } else {
        message(paste0("Note: No tables in an rda format (i.e., .rda) were present in '", fs::path(rda_dir, "rda_files"), "'."))
      }
      if (length(non.rda_tab_list) > 0){
        non.rda_tables_doc <- ""
        for (i in 1:length(non.rda_tab_list)){
          # remove file extension
          tab_name <- stringr::str_extract(non.rda_tab_list[i],
                                           "^[^.]+")
          # remove "_table", if present
          tab_name <- sub("_table", "", tab_name)
          tab_chunk <- paste0(
            "![Your caption here](", fs::path("rda_files",
                                              non.rda_tab_list[i]),
            "){#tab-",
            tab_name,
            "}\n\n"
          )

          non.rda_tables_doc <- paste0(non.rda_tables_doc, tab_chunk)
        }
      } else {
        message(paste0("Note: No table files in a non-rda format (e.g., .jpg, .png) were present in '",  fs::path(rda_dir, "rda_files") , "'."))
      }

      # combine figures_doc setup with figure chunks
      tables_doc <- paste0(tables_doc_header,
                           tables_doc_setup,
                           ifelse(exists("rda_tables_doc"),
                                  rda_tables_doc,
                                  ""),
                           ifelse(exists("non.rda_tables_doc"),
                                  non.rda_tables_doc,
                                  "")
      )
    }
  }

  # Save tables doc to template folder
  utils::capture.output(cat(tables_doc),
                        file = paste0(subdir, "/", "08_tables.qmd"),
                        append = FALSE
  )
}
