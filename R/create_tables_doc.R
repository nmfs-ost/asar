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

  empty_doc_text <- "Please refer to the `stockplotr` package downloaded from remotes::install_github('nmfs-ost/stockplotr') to add premade tables."

  tab_header <- "# Tables {#sec-tables}\n \n"

  # append table-producing code to non-empty tables doc, if it exists, vs. overwriting it
  append <- FALSE
  if (length(file.path(subdir, list.files(subdir, pattern = "tables.qmd"))) == 1) {
    existing_tables_doc <- file.path(subdir, list.files(subdir, pattern = "tables.qmd"))
    table_content <- readLines(existing_tables_doc) |>
      suppressWarnings()

    if ("# Tables {#sec-tables}" %in% table_content) {
      append <- TRUE
      cli::cli_alert_info("Tables doc will be appended to include tables in `tables_dir`.")

      # remove empty_doc_text
      updated_content <- gsub(empty_doc_text, "", table_content, fixed = TRUE)
      writeLines(updated_content, existing_tables_doc)
    }
  } else {
    # existing_figs_doc <- NULL
    table_content <- ""
  }

  # add header
  tables_doc_header <- ifelse(append,
    "",
    tab_header
  )

  # add chunk that creates object as the directory of all rdas
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
        # add_option = TRUE,
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

  # list all files in tables
  file_list <- list.files(file.path(tables_dir, "tables"))

  # create sublist of only rda table files
  rda_tab_list <- file_list[grepl(".rda", file_list)]
  
  # Check if rda already exists and remove from list
  # Check if rda or non-rda already exists and remove from list
  new_rda <- FALSE
  if (length(file.path(subdir, list.files(subdir, pattern = "tables.qmd"))) == 1) {
    existing_tbls_doc <- file.path(subdir, list.files(subdir, pattern = "tables.qmd"))
    table_content <- readLines(existing_tbls_doc) |>
      suppressWarnings()
    # find all instances of figures
    existing_rda_tabs <- vapply(rda_tab_list, function(x) {
      any(grepl(x, table_content, fixed = TRUE))
    }, FUN.VALUE = logical(1))
    rda_tab_list <- rda_tab_list[!existing_rda_tabs]
    # add condition for message to add "new" into message
    new_rda <- ifelse(
      length(existing_rda_tabs) > 0,
      TRUE,
      FALSE
    )
  }

  # remove rda table files that have an associated "split" version
  # remove "_split" from filenames
  remove_split_names <- gsub("_split", "", rda_tab_list)
  # identify duplicates in remove_split_names
  dup_tab <- remove_split_names[duplicated(remove_split_names) | duplicated(remove_split_names, fromLast = TRUE)]
  # remove duplicates in remove_split_names to create final list
  final_rda_tab_list <- rda_tab_list[!(remove_split_names %in% dup_tab & !grepl("_split", rda_tab_list))]

  # create sublist of only non-rda table files
  # non.rda_tab_list <- file_list[!grepl(".rda", file_list)]

  if (length(rda_tab_list) == 0) {
    if (length(file.path(subdir, list.files(subdir, pattern = "tables.qmd"))) != 1) {
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
    # paste rda table code chunks into one object
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
    # if (length(non.rda_tab_list) > 0){
    #   non.rda_tables_doc <- ""
    #   for (i in seq_along(non.rda_tab_list)){
    #     # remove file extension
    #     tab_name <- stringr::str_extract(non.rda_tab_list[i],
    #                                      "^[^.]+")
    #     # remove "_table", if present
    #     tab_name <- sub("_table", "", tab_name)
    #     tab_chunk <- paste0(
    #       "![Your caption here](", fs::path("tables",
    #                                         non.rda_tab_list[i]),
    #       "){#tab-",
    #       tab_name,
    #       "}\n\n"
    #     )
    #
    #     non.rda_tables_doc <- paste0(non.rda_tables_doc, tab_chunk)
    #   }
    # } else {
    #   message(paste0("Note: No table files in a non-rda format (e.g., .jpg, .png) were found in '",  fs::path(tables_dir, "tables") , "'."))
    # }

    # combine tables_doc setup with table chunks
    tables_doc <- paste0(
      tables_doc_header,
      tables_doc_setup,
      ifelse(exists("rda_tables_doc"),
        rda_tables_doc,
        ""
      ) # ,
      # ifelse(exists("non.rda_tables_doc"),
      #        non.rda_tables_doc,
      #        "")
    )
  }

  # Save tables doc to template folder
  utils::capture.output(cat(tables_doc),
    file = paste0(
      subdir, "/",
      ifelse(
        any(grepl("_tables.qmd$", list.files(subdir))),
        list.files(subdir)[grep("_tables.qmd", list.files(subdir))],
        "08_tables.qmd"
      )
    ),
    append = append
  )

  # Read through tables doc and warn about identical labels
  doc_path <- ifelse(
    any(grepl("_tables.qmd$", list.files(subdir))),
    fs::path(subdir, list.files(subdir)[grep("_tables.qmd", list.files(subdir))]),
    fs::path(subdir, "08_tables.qmd")
  )
  
  fix_duplicate_chunks(
    doc_path = doc_path,
    doc_type = "Tables"
  )

}