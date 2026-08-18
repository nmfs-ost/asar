update_report <- function(
    dir = "report",
    new_dir = getwd()
) {
  # Identify location of previous folder and copy over to new directory
  # Don't copy old figures and tables
  # Run create_template(rerender_skeleton = TRUE) with new arguments
  # Take out rerender from own function?
  
  # Create "report" folder in new dir
  dir.create(file.path(new_dir, "report"))
  
  previous_report_files <- list.files(dir, recursive = TRUE)
  from_path <- file.path(dir, previous_report_files)
  to_path <- file.path(new_dir, previous_report_files)
  # copy files to new folder
  # currently not working - showing warning:
  # Warning message:
  # In FUN(X[[i]], ...) :
  #   'C:\Users\samantha.schiano.NMFS\Documents\test_folder' already exists
  vapply(
    unique(dirname(to_path)),
    dir.create,
    logical(1),
    recursive = TRUE,
    showWarnings = TRUE
  )
  
  file.copy(from_path,to_path, overwrite = FALSE)
}
