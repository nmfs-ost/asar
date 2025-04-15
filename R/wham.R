# Development script for WHAM converter
# Example
# Haddock example from dan ----
had.dir <- "~/Stock Assessment Workflow/Scoping/Assessment Models/WHAM/Gulf of Maine Haddock"
had.obj <- "GOMHADwham2024MTA.rds"
output_file <- file.path(had.dir, had.obj)

# read in WHAM model object
haddock <- readRDS(output_file)

# rep = report contain quantities and other stuff
names(haddock$rep)

# Inputs
inp <- haddock$input
# ages
# ages <- sub("+", "", inp$ages.lab)
ages <- as.numeric((stringr::str_replace_all(inp$ages.lab, "[\\+]*", "")))
# years
years <- inp$years
start_year <- min(years)
end_year <- max(years)

# Output
rep <- haddock$rep

# Manipulating data set
# conditional statement for age or year
# data named estimate
# label is the name for that pulled set in list
# if contains list then matrix of num years by num ages - then those are fleets?

# what does each indicate when a vector in the list is 1 x 4 x 47 x 9 ?

# Export all output variable names
# Break a part input names
in_var_names <- c()
for (i in 1:length(names(input))) {
  # check if list
  if (is.list(input[[i]]) & !is.null(names(input[[i]]))) {
    # extract var name and add to vector
    sub_var <- names(input[i])
    sub_var_df <- input[[i]]
    in_var_names <- c(in_var_names, sub_var)
    # check throughout list if another nested list
    for (j in 1:length(names(sub_var_df))) {
      var_next <- names(sub_var_df[j])
      var_next_df <- sub_var_df[[j]]
      # if is a list and has names within the names, add to the in_var_names
      if (is.list(var_next_df) & !is.null(names(var_next_df))) {
        # add to vector
        in_var_names <- c(in_var_names, var_next, names(var_next_df))
      } else {
        # add name for var_next to list
        in_var_names <- c(in_var_names, var_next)
      }
    }
  } else {
    # extract name
    sub_var <- names(input[i])
    # add to vector
    in_var_names <- c(in_var_names, sub_var)
  }
}
var_names <- data.frame(
  input_names = in_var_names,
  output_names = c(names(output), rep(NA, length(in_var_names) - length(names(output))))
)

# write.csv(var_names, file = "~/Stock Assessment Workflow/Scoping/Assessment Models/WHAM/WHAM_var_names.csv", row.names = FALSE)

#### Converter start ----

#### out_new ----
out_new <- data.frame(
  label = NA,
  time = NA,
  year = NA,
  fleet = NA,
  area = NA,
  season = NA,
  subseason = NA,
  age = NA,
  sex = NA,
  growth_pattern = NA,
  len_bins = NA,
  # initial = NA,
  estimate = NA,
  uncertainty = NA,
  uncertainty_label = NA,
  # likelihood = NA,
  # gradient = NA,
  # estimated = NA, # TRUE/FALSE
  module_name = NA,
  # Additional factors from SS3
  bio_pattern = NA,
  birthseas = NA,
  settlement = NA,
  morph = NA,
  # beg/mid = NA, # need to identify df where this is applicable
  type = NA,
  factor = NA,
  platoon = NA,
  month = NA,
  sexes = NA,
  part = NA,
  bin = NA,
  kind = NA,
  nsim = NA,
  age_a = NA
)
out_new <- out_new[-1, ]

## read file ----
dat <- readRDS(output_file)
# extract input
input <- dat$input
# extract output
output <- dat$rep

# Pull out potential factors and their lengths due to set up of file
# extracted from input
# identify fleet names
fleet_names <- input$fleet_names
nfleets <- input$data$n_fleets
# identify age structure
# plus group identifier removed - *always assumed*
ages <- as.numeric((stringr::str_replace_all(input$ages.lab, "[\\+]*", "")))
nages <- input$data$n_ages
# identify years
years <- input$years
nyears <- input$data$n_years_model

# Other factors ?
# area
# season
nseas <- input$data$n_seasons
# subseason
# sex
# growth pattern
# len bins
# bio_pattern
# birthseas
# settlement
# morph
# platoon
# month

# n selblocks? - not sure what this is


# Create list for morphed dfs to go into (for rbind later)
out_list <- list()

