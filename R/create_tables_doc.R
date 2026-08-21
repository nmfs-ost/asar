#' Create Quarto Document of Tables
#'
#' Only tables in an rda format (e.g., my_table.rda) will be imported. Tables in
#' other formats (e.g., .jpg, .png) are not supported; they lack text recognition.
#' See [the `asar` custom figures and tables vignette](https://nmfs-ost.github.io/asar/articles/custom-figs-tabs.html#make-rdas)
#' for more information about making .rda files with custom tables.
#'
#' If your table is too wide to print on a portrait-oriented page,
#' the page will be rotated to landscape view. If if is too wide to print in
#' landscape view, it will be split into multiple tables. In this case, a new rda
#' will be created and is identifiable by the phrase "split" in the filename (e.g.,
#' indices.abundance_table.rda will generate a new indices.abundance_table_split.rda
#' file), and column 1 will be repeated across split tables. These tables will
#' share the same caption. To specify a different repeated column(s), use
#' asar::export_split_tbls with your preferred essential_columns value.
#'
#' @inheritParams create_figures_doc
#' @param tables_dir The location of the "tables" folder, which contains tables
#' files.
#'
#' @param tables_doc_name (Optional) Filename for the figures doc written in
#' `subdir` (e.g., `"06_tables.qmd"`). If NULL, the function auto-detects an
#' existing `*_tables.qmd` file, or defaults to `"09_tables.qmd"`.
#' 
#' Default: NULL
#' 
#' @return Create a quarto document as part of a stock assessment outline with
#' pre-loaded R chunks that add stock assessment tables from the nmfs-ost/stockplotr
#' R package, or other tables in the same rda format.
#' @export
#'
#' @examples
#' \dontrun{
#' create_tables_doc(
#'   subdir = getwd(),
#'   tables_dir = here::here()
#' )
#' }
create_tables_doc <- function(subdir = getwd(),
                              tables_dir = getwd()) {
  portrait_pg_width <- 5
  landscape_pg_width <- 8
  
  empty_doc_text <- "Please refer to the `stockplotr` package downloaded from remotes::install_github('nmfs-ost/stockplotr') to add premade tables."
  
  tab_header <- "# Tables {#sec-tables}\n \n"
  
  append <- FALSE
  if (file.exists(target_table_doc)) {
    existing_tables_doc <- target_table_doc
    table_content <- readLines(existing_tables_doc) |>
      suppressWarnings()
    
    if ("# Tables {#sec-tables}" %in% table_content) {
      append <- TRUE
      cli::cli_alert_info("Tables doc will be appended to include tables in `tables_dir`.")
      
      updated_content <- gsub(empty_doc_text, "", table_content, fixed = TRUE)
      writeLines(updated_content, existing_tables_doc)
    }
  } else {
    table_content <- ""
  }
  
  tables_doc_header <- ifelse(append,
                              "",
                              tab_header
  )
  
  if (!(any(grepl(
    "#| label: 'set-rda-dir-tbls'",
    table_content,
    fixed = TRUE
  )))) {
    tables_doc_setup <- paste0(
      add_chunk(
        glue::glue(
          "library(gt)
          tables_dir <- fs::path('{tables_dir}', 'tables')"
        ),
        label = "set-rda-dir-tbls",
        chunk_option = c(
          "echo: false",
          "warning: false",
          "include: false"
        )
      ),
      "\n"
    )
  } else {
    tables_doc_setup <- ""
  }
  
  tables_doc <- ""
  
  file_list <- list.files(file.path(tables_dir, "tables"))
  
  rda_tab_list <- file_list[grepl(".rda", file_list)]
  
  new_rda <- FALSE
  if (file.exists(target_table_doc)) {
    existing_tbls_doc <- target_table_doc
    table_content <- readLines(existing_tbls_doc) |>
      suppressWarnings()
    existing_rda_tabs <- vapply(rda_tab_list, function(x) {
      any(grepl(x, table_content, fixed = TRUE))
    }, FUN.VALUE = logical(1))
    rda_tab_list <- rda_tab_list[!existing_rda_tabs]
    new_rda <- ifelse(
      length(existing_rda_tabs) > 0,
      TRUE,
      FALSE
    )
  }
  
  remove_split_names <- gsub("_split", "", rda_tab_list)
  dup_tab <- remove_split_names[duplicated(remove_split_names) | duplicated(remove_split_names, fromLast = TRUE)]
  final_rda_tab_list <- rda_tab_list[!(remove_split_names %in% dup_tab & !grepl("_split", rda_tab_list))]
  
  create_tab_chunks <- function(tab = NA,
                                tables_dir = getwd()) {
    split <- grepl("split", tab)
    
    tab_shortname <- ifelse(split,
                            stringr::str_remove(tab, "_table_split.rda"),
                            stringr::str_remove(tab, "_table.rda")
    )
    
    tbl_orient <- ifelse(split,
                         "extra-wide",
                         ID_tbl_width_class(
                           plot_name = tab_shortname,
                           tables_dir = tables_dir,
                           portrait_pg_width = portrait_pg_width
                         )
    )
    
    tbl_length <- ID_tbl_length_class(
      plot_name = tab_shortname,
      tables_dir = tables_dir
    )
    
    table_specs <- list(tbl_orient, tbl_length)
    
    tbl_class <- dplyr::case_when(
      table_specs[[1]] == "regular" & table_specs[[2]] == "regular" ~ "reg_reg",
      table_specs[[1]] == "regular" & table_specs[[2]] == "long" ~ "reg_long",
      table_specs[[1]] == "wide" & table_specs[[2]] == "regular" ~ "wide_reg",
      table_specs[[1]] == "wide" & table_specs[[2]] == "long" ~ "wide_long",
      table_specs[[1]] == "extra-wide" & table_specs[[2]] == "regular" ~ "ewide_reg",
      table_specs[[1]] == "extra-wide" & table_specs[[2]] == "long" ~ "ewide_long",
      TRUE ~ "unknown"
    )
    
    if (tbl_class == "unknown") {
      cli::cli_abort("Unknown table class. Check table is an acceptable `gt` table.")
    }
    
    max_rows <- ifelse(tbl_orient == "regular", 38, 28)
    
    tables_doc_plot_setup1 <- paste0(
      add_chunk(
        paste0(
          "# load rda
load(file.path(tables_dir, '", stringr::str_remove(tab, "_split"), "'))\n
# save rda with table-specific name\n",
          tab_shortname, "_table_rda <- rda\n
# save table and caption as separate objects\n",
          tab_shortname, "_table <- ", tab_shortname, "_table_rda$table\n",
          tab_shortname, "_cap <- ", tab_shortname, "_table_rda$caption"
        ),
        label = glue::glue("tab-{tab_shortname}-setup")
      ),
      "\n"
    )
    
    if (tbl_class == "reg_reg") {
      tables_doc_plot_setup2 <- paste0(
        add_chunk(
          glue::glue(
            "{tab_shortname}_table |>\n",
            "    gt::cols_width(\n",
            "      everything() ~ pct(20)\n",
            "    ) \n"
          ),
          label = glue::glue("tbl-{tab_shortname}"),
          chunk_option = c(
            "echo: false",
            "warnings: false",
            glue::glue(
              "tbl-cap: !expr {tab_shortname}_cap"
            ),
            "tbl-pos: 't'"
          )
        ),
        "\n"
      )
    }
    
    if (tbl_class == "wide_reg") {
      tables_doc_plot_setup2 <- paste0(
        "::: {.landscape}\n\n",
        add_chunk(
          glue::glue(
            "{tab_shortname}_table |>\n",
            "  gt::tab_options(\n",
            "    table.width = pct(100),\n",
            "    table.layout = 'auto'\n",
            "  ) |>\n",
            "  gt::cols_width(\n",
            "    everything() ~ pct(20)\n",
            "  ) \n"
          ),
          label = glue::glue("tbl-{tab_shortname}"),
          chunk_option = c(
            "echo: false",
            "warnings: false",
            glue::glue(
              "tbl-cap: !expr {tab_shortname}_cap"
            ),
            "tbl-pos: 't'"
          )
        ),
        "\n",
        ":::\n"
      )
    }
    
    if (tbl_class == "reg_long" | tbl_class == "wide_long") {
      load(fs::path(tables_dir, "tables", tab))
      split_table_rows <- length(rda[[1]]$`_data`[[1]])
      split_tables_rowwise <- ceiling(split_table_rows / max_rows)
      
      tables_doc_plot_setup2 <- ""
      for (i in 1:as.numeric(split_tables_rowwise)) {
        tables_doc_plot_setup2 <- paste0(
          tables_doc_plot_setup2,
          ifelse(tbl_class == "wide_long",
                 "::: {.landscape}\n\n",
                 ""
          ),
          add_chunk(
            paste0(
              "# plot table ", i, "\n",
              tab_shortname, "_table |>\n",
              "  gt::tab_options(\n",
              "    table.width = pct(100),\n",
              "    table.layout = 'auto'\n",
              "  ) |>\n",
              "  gt::cols_width(\n",
              "    everything() ~ pct(20)\n",
              "  ) |> \n",
              " asar::gt_split(row_every_n = ", max_rows, ") |>\n",
              " gt::grp_pull(", i, ")\n"
            ),
            label = glue::glue("tbl-{tab_shortname}", i),
            add_option = TRUE,
            chunk_option = c(
              "echo: false",
              glue::glue(
                "tbl-cap: !expr paste0({tab_shortname}_cap, ' ({i} of {split_tables_rowwise})')"
              ),
              "tbl-pos: 't'"
            )
          ),
          ifelse(tbl_class == "wide_long",
                 ":::\n",
                 "\n"
          )
        )
      }
    }
    
    if (tbl_class == "ewide_reg") {
      if (split) {
        load(fs::path(tables_dir, "tables", tab))
        split_tables <- length(table_list)
      } else {
        split_tables <- export_split_tbls(
          tables_dir = tables_dir,
          plot_name = tab,
          essential_columns = 1
        )
        
        tab <- gsub("table", "table_split", tab)
        load(fs::path(tables_dir, "tables", tab))
        split_tables <- length(table_list)
      }
      
      tables_doc_plot_setup2_import <- paste0(
        add_chunk(
          paste0(
            "load(file.path(tables_dir, '", tab, "'))\n
# save rda with plot-specific name\n",
            tab_shortname, "_table_split_rda <- table_list\n
# extract table caption specifiers\n",
            tab_shortname, "_cap_split <- names(", tab_shortname, "_table_split_rda)"
          ),
          label = glue::glue("tbl-{tab_shortname}-labels"),
          chunk_option = c(
            "echo: false",
            "warnings: false",
            glue::glue("include: false")
          )
        ),
        "\n"
      )
      
      tables_doc_plot_setup2_display <- ""
      for (i in 1:as.numeric(split_tables)) {
        tables_doc_plot_setup2_display <- paste0(
          tables_doc_plot_setup2_display,
          "::: {.landscape}\n\n",
          add_chunk(
            paste0(
              "# plot split table ", i, "\n",
              tab_shortname, "_table_split_rda[[", i, "]] |>\n",
              "  gt::tab_options(\n",
              "    table.width = pct(100),\n",
              "    table.layout = 'auto'\n",
              "  ) |>\n",
              "  gt::cols_width(\n",
              "    everything() ~ pct(20)\n",
              "  ) \n"
            ),
            label = glue::glue("tbl-{tab_shortname}", i),
            add_option = TRUE,
            chunk_option = c(
              "echo: false",
              glue::glue(
                "tbl-cap: !expr paste0({tab_shortname}_cap, ' ({i} of {split_tables})')"
              ),
              "tbl-pos: 't'"
            )
          ),
          "\n",
          ":::\n"
        )
      }
      
      tables_doc_plot_setup2 <- paste0(
        tables_doc_plot_setup2_import,
        tables_doc_plot_setup2_display,
        "{{< pagebreak >}} \n\n"
      )
    }
    
    if (tbl_class == "ewide_long") {
      if (split) {
        load(fs::path(tables_dir, "tables", tab))
        split_tables <- length(table_list)
      } else {
        split_tables <- export_split_tbls(
          tables_dir = tables_dir,
          plot_name = tab,
          essential_columns = 1
        )
        
        tab <- gsub("table", "table_split", tab)
        load(fs::path(tables_dir, "tables", tab))
        split_tables <- length(table_list)
      }
      split_table_rows <- length(table_list[[1]]$`_data`[[1]])
      split_tables_rowwise <- ceiling(split_table_rows / max_rows)
      
      tables_doc_plot_setup2_import <- paste0(
        add_chunk(
          paste0(
            "load(file.path(tables_dir, '", tab, "'))\n
# save rda with plot-specific name\n",
            tab_shortname, "_table_split_rda <- table_list\n
# extract table caption specifiers\n",
            tab_shortname, "_cap_split <- names(", tab_shortname, "_table_split_rda)"
          ),
          label = glue::glue("tbl-{tab_shortname}-labels"),
          chunk_option = c(
            "echo: false",
            "warnings: false",
            glue::glue("include: false")
          )
        ),
        "\n"
      )
      
      tables_doc_plot_setup2_display <- ""
      for (i in 1:as.numeric(split_tables)) {
        for (j in 1:as.numeric(split_tables_rowwise)) {
          tables_doc_plot_setup2_display <- paste0(
            tables_doc_plot_setup2_display,
            "::: {.landscape}\n\n",
            add_chunk(
              paste0(
                "# plot split table ", i, "\n",
                tab_shortname, "_table_split_rda[[", i, "]] |>\n",
                "  gt::tab_options(\n",
                "    table.width = pct(100),\n",
                "    table.layout = 'auto'\n",
                "  ) |>\n",
                "  gt::cols_width(\n",
                "    everything() ~ pct(20)\n",
                "  ) |> \n",
                " asar::gt_split(row_every_n = ", max_rows, ") |>\n",
                " gt::grp_pull(", j, ")\n"
              ),
              label = glue::glue("tbl-{tab_shortname}", i, "-", j),
              add_option = TRUE,
              chunk_option = c(
                "echo: false",
                glue::glue(
                  "tbl-cap: !expr paste0({tab_shortname}_cap, ' ({i} of {split_tables} tables split by column, {j} of {split_tables_rowwise} tables split by rows)')"
                ),
                "tbl-pos: 't'"
              )
            ),
            "\n",
            ":::\n"
          )
        }
      }
      
      tables_doc_plot_setup2 <- paste0(
        tables_doc_plot_setup2_import,
        tables_doc_plot_setup2_display
      )
    }
    
    paste0(
      tables_doc_plot_setup1,
      tables_doc_plot_setup2,
      "{{< pagebreak >}} \n\n"
    )
  }
  
  if (length(rda_tab_list) == 0) {
    if (!file.exists(target_table_doc)) {
      cli::cli_alert_warning("Found zero tables in an rda format (i.e., .rda) in {fs::path(tables_dir, 'tables')}.",
                             wrap = TRUE
      )
      cli::cli_alert_info("For `create_tables_doc` to incorporate tables, there must be:",
                          wrap = TRUE
      )
      cli::cli_ol(c(
        "a 'tables' folder in {fs::path(tables_dir)}",
        ".rda files in the 'tables' folder"
      ))
      tables_doc <- paste0(
        tables_doc_header,
        empty_doc_text
      )
    } else {
      cli::cli_alert("No new tables detected.")
    }
  } else {
    cli::cli_alert_success("Found {length(final_rda_tab_list)}{ifelse(new_rda, ' new ', ' ')}table{?s} in an rda format (i.e., .rda) in {fs::path(tables_dir, 'tables')}.",
                           wrap = TRUE
    )
    if (length(final_rda_tab_list) > 0) {
      rda_tables_doc <- ""
      for (i in seq_along(final_rda_tab_list)) {
        tab_chunk <- create_tab_chunks(
          tab = final_rda_tab_list[i],
          tables_dir = tables_dir
        )
        
        rda_tables_doc <- paste0(rda_tables_doc, tab_chunk)
      }
    }
    
    tables_doc <- paste0(
      tables_doc_header,
      tables_doc_setup,
      ifelse(exists("rda_tables_doc"),
             rda_tables_doc,
             ""
      )
    )
  }
  
  doc_info <- migrate_legacy_docs(subdir, doc_type = "tables")
  
  if (doc_info$using_legacy) {
    cli::cli_alert_info("Detected legacy figure/table document order ({.file {doc_info$legacy_name}}). asar now uses {.file {doc_info$current_name}} to maintain an accurate Table of Contents.")
    cli::cli_alert_info("{.file {doc_info$legacy_name}} will be renamed to {.file {doc_info$current_name}}.")
  }
  
  tables_doc_name <- if (doc_info$using_legacy) {
    doc_info$legacy_name
  } else {
    doc_info$resolved_name
  }
  
  utils::capture.output(cat(tables_doc),
                        file = fs::path(subdir, tables_doc_name),
                        append = append
  )
  
  if (doc_info$using_legacy) {
    file.rename(
      from = fs::path(subdir, doc_info$legacy_name),
      to   = fs::path(subdir, doc_info$current_name)
    )
    target_table_doc <- fs::path(subdir, tab_doc_data$current_doc_name)
  }
  
  current_tables_doc <- fs::path(subdir, doc_info$resolved_name)
  
  fix_duplicate_chunks(
    doc_path = current_tables_doc,
    doc_type = "Tables"
  )
}