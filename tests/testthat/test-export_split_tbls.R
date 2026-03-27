# test_that("Number of split tables is calculated correctly for 1 table", {
#   library(gt)
#   load(file.path(
#     "fixtures", "ss3_models_converted", "Hake_2018",
#     "std_output.rda"
#   ))
#   
#   stockplotr::table_landings(out_new,
#                              module = "CATCH",
#                              make_rda = TRUE
#   )
#   
# 
#   # indices table
#   num_tabs <- export_split_tbls(
#     tables_dir = getwd(),
#     plot_name = "landings_table.rda",
#     essential_columns = 1
#   )
# 
#   # expect 1 table
#   expected_output <- 1
#   expect_equal(num_tabs, expected_output)
# 
#   # erase temporary testing files
#   file.remove(fs::path(getwd(), "captions_alt_text.csv"))
#   unlink(fs::path(getwd(), "tables"), recursive = T)
# })
# 
# 
# test_that("Number of split tables is calculated correctly for 3 tables", {
#   library(gt)
#   load(file.path(
#     "fixtures", "ss3_models_converted", "Hake_2018",
#     "std_output.rda"
#   ))
#   
#   stockplotr::table_landings(out_new,
#                              module = "CATCH",
#                              make_rda = TRUE
#   )
#   
#   load(file.path("tables", "landings_table.rda"))
#   
#   wider_df <- cbind(
#     rda$table$`_data`,
#     data.frame(
#       Col1 = 1:52,
#       Col2 = 52:103,
#       Col3 = 104:155,
#       Col4 = 156:207,
#       Col5 = 208:259,
#       Col6 = 260:311,
#       Col7 = 312:363,
#       Col8 = 364:415
#     )
#   ) |>
#     gt()
#   
#   rda$table <- wider_df
#   save(rda, file = file.path(getwd(), "tables", "landings_table.rda"))
#   
#   # indices table
#   num_tabs <- export_split_tbls(
#     tables_dir = getwd(),
#     plot_name = "landings_table.rda",
#     essential_columns = 1
#   )
#   
#   # expect 3 tables
#   expected_output <- 3
#   expect_equal(num_tabs, expected_output)
#   
#   # expect to see an "landings_table_split.rda" file
#   expect_no_error("landings_table_split.rda" %in% list.files(file.path("tables")))
#   
#   # expect that an object "table_list" imported into environment
#   expect_no_error(load(file.path("tables", "landings_table_split.rda")))
#   
#   expect_equal(length(table_list), 3)
#   
#   # erase temporary testing files
#   file.remove(fs::path(getwd(), "captions_alt_text.csv"))
#   unlink(fs::path(getwd(), "tables"), recursive = T)
#   
# })
