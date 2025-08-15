# Download SS3 models and place them in the specified directory
ss3_models <- r4ss::download_models(
  dir = test_path("fixtures", "ss3_models")
)

# Get the SS3 executable for running the models
ss3_exe <- r4ss::get_ss3_exe(
  dir = test_path("fixtures", "ss3_models")
)

# Function to run a specific SS3 model
run_ss3_models <- function(model_id, model_dir, exe_dir) {
  # Define the working directory for the model
  working_dir <- model_dir[model_id]

  # Copy the SS3 executable to the model's working directory
  file.copy(Sys.which(file.path(exe_dir, "ss3")), working_dir)

  # Run SS3 model
  r4ss::run(dir = working_dir)
}

# List all the model directories to be processed
all_models <- list.dirs(
  file.path(test_path("fixtures", "ss3_models"), "models"),
  full.names = TRUE,
  recursive = FALSE
)

# Detect the number of available CPU cores, leaving one free
core_num <- parallel::detectCores() - 1

# Initialize snowfall for parallel processing, using the detected core count
snowfall::sfInit(parallel = TRUE, cpus = core_num)

# Run the SS3 models in parallel using the snowfall library
snowfall::sfLapply(
  seq_along(all_models),
  run_ss3_models,
  all_models,
  test_path("fixtures", "ss3_models")
)

# Stop the snowfall parallel session
snowfall::sfStop()

# Remove the SS3 files except Report.sso from the working directory after the run is complete
all_files <- list.files(
  test_path("fixtures", "ss3_models", "models"),
  full.names = TRUE,
  recursive = TRUE
)
report_files <- file.path(all_models, "Report.sso")

report_id <- match(report_files, all_files)
files_to_delete <- all_files[-report_id]
file.remove(files_to_delete)

# Clean up by removing the SS3 executable from the main directory after running all models
file.remove(Sys.which(test_path("fixtures", "ss3_models", "ss3")))
