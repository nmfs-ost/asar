# Testing variations of templates

# Last update and testing 3/20/24

### PLEASE DO NOT PUSH ANY TEMPLATES THAT YOU MAKE TESTING OUT THIS FUNCTION###
# More guidance for saving scripts will be provided in the future

# reference functions during testing
source(here::here('R', 'create_template.R'))
source(here::here('R', 'write_title.R'))
source(here::here('R', 'paste_child.R'))
source(here::here('R', 'chunkr.R'))
source(here::here('R', 'generate_citation.R'))

# Basic parameters
new_template = TRUE
format = "pdf"
office = "NEFSC"
region = "GB"
species = "Bluefish"
spp_latin = "bluishfihesi"
year = 2024
author = c("John Snow", "Danny Phantom", "Patrick Star")
include_affiliation = TRUE
parameters = TRUE
param_names = c("fleet1", "fleet2", "model")
param_values = c("Commercial", "Recreational", "Woods Hole Assessment Model")
type = "RT"

################################################################################

# Basic, new default template
# Last tested 3/20/24
create_template(
  new_template = TRUE,
  format = "pdf",
  office = "NEFSC",
  region = "GB",
  species = "Atlantic Bluefish",
  spp_latin = "Pomatomus saltatrix",
  year = 2024,
  author = c("John Snow", "Danny Phantom", "Patrick Star"),
  include_affiliation = TRUE,
  parameters = TRUE,
  param_names = c("fleet1", "fleet2", "model"),
  param_values = c("Commercial", "Recreational", "Woods Hole Assessment Model"),
  type = "RT",
  model_results = "report.sso",
  model = "SS"
)

################################################################################

# Test pulling template from older assessment
# Last tested 3/20/24
# "Old" assessment template
create_template(
  new_template = TRUE,
  format = "pdf",
  office = "SEFSC",
  region = "Gulf of Mexico",
  species = "Red Snapper",
  spp_latin = "Lutjanus campechanus",
  year = 2021,
  author = c("John Snow", "Danny Phantom", "Patrick Star"),
  include_affiliation = TRUE,
  parameters = FALSE,
  type = "RT"
)
# New assessment from old template
create_template(
  new_template = FALSE,
  prev_year = 2021,
  year = 2024,
  office = "SEFSC",
  region = "Gulf of Mexico",
  species = "Red Snapper",
  latin = "Lutjanus campechanus",
  type = "RT"
)

################################################################################


