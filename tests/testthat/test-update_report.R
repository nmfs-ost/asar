test_that("multiplication works", {
  # create folder for initial report
  dir.create("species_year1")
  # create folder for "next" report
  dir.create("species_year2")
  
  # make initial report
  create_template(
    file_dir = "species_year1",
    year = 2023,
    bib_file = FALSE
  )
  
  # add writing to intro doc
  line <- "this is some additional text to add into the intro file."
  write(line, file = "species_year1/report/02_introduction.qmd", append = TRUE)
  
  # update the report to the other folder
  update_report(
    file_dir = "species_year2",
    previous_file_dir = "species_year1",
    year = 2027
  )
  
  # test number of files is the same
  num_files_init <- length(list.files("species_year1/report"))
  num_files_next <- length(list.files("species_year2/report"))
  expect_all_equal(num_files_init, num_files_next)
  # test one of the child docs is the same -- intro
  yr1_intro <- readLines("species_year1/report/02_introduction.qmd")
  yr2_intro <- readLines("species_year2/report/02_introduction.qmd")
  expect_equal(yr1_intro, yr2_intro)
  
  unlink("species_year1", recursive = TRUE)
  unlink("species_year2", recursive = TRUE)
})

test_that("tables and figures are reset when prompted.", {
  # create folder for initial report
  dir.create("species_year1")
  # create folder for "next" report
  dir.create("species_year2")
  
  year1_dir <- fs::path(getwd(), "species_year1")
  year2_dir <- fs::path(getwd(), "species_year2")
  
  # make example table and figure
  withr::with_dir(
    year1_dir, {
      stockplotr::plot_spawning_biomass(
        stockplotr::example_data,
        make_rda = TRUE,
        # figures_dir = x,
        interactive = FALSE
      )
      
      # commenting out table until withdir issue fixed
      # stockplotr::table_index(
      #   stockplotr::example_data,
      #   make_rda = TRUE,
      #   # tables_dir = x,
      #   interactive = FALSE
      # )
    }
  )
  
  # make initial report
  withr::with_dir(
    year1_dir,
    create_template(
      # file_dir = "species_year1",
      year = 2023,
      bib_file = FALSE
    )
  )
  
  # update the report to the other folder
  update_report(
    file_dir = "species_year2",
    previous_file_dir = "species_year1",
    year = 2027,
    reset_tables_and_figures = TRUE
  )
  
  init_figs_doc <- readLines(file.path(year1_dir, "report", "08_figures.qmd"))
  update_figs_doc <- readLines(file.path(year2_dir, "report", "08_figures.qmd"))
  
  # init_tabs_doc <- readLines(file.path(year1_dir, "report", "09_tables.qmd"))
  # update_tabs_doc <- readLines(file.path(year2_dir, "report", "09_tables.qmd"))
  
  expect_false(init_figs_doc == update_figs_doc)
  # expect_false(init_tabs_doc == update_tabs_doc)
  
  unlink("species_year1", recursive = TRUE)
  unlink("species_year2", recursive = TRUE)
})