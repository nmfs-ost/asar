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
source(here::here('R', 'create_style_css.R'))

# Option and parameters for tempalte generation
new_template = TRUE
tempdir = here::here()
format = c("pdf", "docx", "html", NULL)
office = c("AFSC", "PIFSC", "NEFSC", "NWFSC", "SEFSC", "SWFSC")
region = NULL
complex = "NO"
species = NULL
spp_latin = NULL
year = NULL
author = NULL
include_affiliation = FALSE
simple_affiliation = TRUE
alt_title = FALSE
title = NULL
parameters = TRUE
param_names = NULL
param_values = NULL
resdir = NULL
model_results = NULL
model = NULL
add_section = FALSE
secdir = NULL
new_section = NULL
section_location = NULL
type = NULL
prev_year = NULL
custom = FALSE
custom_sections = NULL

################################################################################
# Basic parameters
# Last tested 04-05-24
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
model_results = "report.sso"
model = "SS"

################################################################################

# Basic, new default template
# Last tested0 3-20-24
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
# Last tested 3-20-24
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

# Create assessment with custom sections using those in the repo already
# Last tested 04-05-24

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
custom = TRUE
add_section = FALSE
custom_sections = c("executive summary", "data", "model", "results", "references", "tables", "figures")
tempdir = here::here()
complex = "NO"
simple_affiliation = TRUE
alt_title = FALSE
resdir = "C:/Users/samantha.schiano/Documents/Stock Assessment Workflow/ex_mod_res"
model_results = "output.Rdata"
model = "WHAM"
