# Pre-calculated quantities and output for use in a stock assessment report
start_year <- output |> 
  dplyr::filter(era == 'time') |> 
  dplyr::summarise(min_year = min(year)) |> 
  dplyr::pull(min_year) |> 
  as.numeric() 

end_year <- output |> 
  dplyr::filter(era == 'time') |> 
  dplyr::summarise(max_year = max(year)) |> 
  dplyr::pull(max_year) |> 
  as.numeric() 

# subset output to remove quantities that are split by factor 
output2 <- output |> 
  dplyr::filter(is.na(season), 
                is.na(fleet), 
                is.na(sex), 
                is.na(area), 
                is.na(growth_pattern), 
                is.na(subseason), 
                is.na(age))

# terminal fishing mortality 
Fend <- output2 |> 
  dplyr::filter((label == 'fishing_mortality' & year == end_year) | (label == 'terminal_fishing_mortality' & is.na(year))) |>
  dplyr::pull(estimate) |>
  # unique() |>
  dplyr::first()

# fishing mortality at msy 
# please change target if desired 
Ftarg <- output2 |>
  dplyr::filter(grepl('f_target', label) | grepl('f_msy', label) | (grepl('fishing_mortality_msy', label) & is.na(year))) |>
  dplyr::pull(estimate)

# Terminal year F respective to F target 
F_Ftarg <- Fend / Ftarg

# terminal year biomass 
Bend <- output2 |>
  dplyr::filter(grepl('^biomass$', label),
                year == end_year) |>
  dplyr::pull(estimate)

# target biomass (msy) 
# please change target if desired 
Btarg <- output2 |>
  dplyr::filter(
    !grepl("spawning|catch", label),
    (grepl('biomass', label) & grepl('target', label) & estimate >1) | label == 'biomass_msy') |>
  dplyr::pull(estimate)

# total catch in the last year 
total_catch <- output |> 
  dplyr::filter(grepl('^catch$', label), 
                year == end_year) |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(total_catch  = sum(estimate)) |> 
  dplyr::ungroup() |> 
  dplyr::pull(total_catch) 

# total landings in the last year 
total_landings <- output |>
  dplyr::filter(grepl('landings_observed', label), year == end_year) |>
  dplyr::group_by(year) |>
  dplyr::summarise(total_land  = sum(estimate)) |>
  dplyr::ungroup() |>
  dplyr::pull(total_land)

# spawning biomass in the last year
SBend <- output2 |>
  dplyr::filter(
    grepl('spawning_biomass$', label), year == end_year,
    !is.na(estimate)
  ) |>
  dplyr::pull(estimate) |>
  unique()

# overall natural mortality or at age 
M <- output |>
  dplyr::filter(grepl('natural_mortality', label)) |>
  dplyr::pull(estimate) |>
  unique()

# Biomass at msy 
# to change to another reference point, replace msy in the following lines with other label 
Bmsy <- output2 |>
  dplyr::filter((grepl('^biomass', label) & grepl('msy', label) & estimate >1) | grepl('^biomass_msy$', label)) |>
  dplyr::pull(estimate)

# target spawning biomass(msy) 
# please change target if desired 
SBmsy <- output2 |>
  dplyr::filter((grepl('spawning_biomass', label) & grepl('msy$', label) & estimate > 1) | label == 'spawning_biomass_msy$') |>
  dplyr::pull(estimate)

# steepness 
h <- output |> 
  dplyr::filter(grepl('steep', label)) |> 
  dplyr::pull(estimate)

# recruitment 
R0 <- output |> 
  dplyr::filter(grepl('recruitment_unfished$', label)) |> 
  dplyr::pull(estimate)
