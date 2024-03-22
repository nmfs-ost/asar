# testing functions

# Example script that allows the user to try out creating stock assessment report templates
# In development
# Last edited: 03//24

### PLEASE DO NOT PUSH ANY TEMPLATES THAT YOU MAKE TESTING OUT THIS FUNCTION###
# More guidance for saving scripts will be provided in the future

# Functions
source(here::here('R', 'create_template.R'))
source(here::here('R', 'write_title.R'))
source(here::here('R', 'paste_child.R'))
source(here::here('R', 'chunkr.R'))
source(here::here('R', 'generate_citation.R'))


title1 <- write_title(
  office = "NEFSC",
  species = "Red Snapper",
  spp_latin = "Lutjanus campechanus",
  region = "Gulf of Mexico",
  type = "RT")

cat(generate_citation(
  title = title1,
  author = c("John Snow", "Danny Phantom", "Patrick Star"),
  year = 2021,
  office = "NEFSC"
))

# test paste_child
cat(
  paste_child(
    c("00_abstract.qmd", "01_executive_summary.qmd", "11_appendix.qmd"),
    label = c("abstract", "executive summary","appendix")
  )
)

x=c("00_abstract.qmd", "01_executive_summary.qmd", "11_appendix.qmd")

write_harv_proj_table(results_last = NA,
                      results_lastp1 = NA,
                      results_curr = NA,
                      results_currp1 = NA,
                      tier = 3,
                      overfishing_last = "NO",
                      overfishing_curr = "NO")

create_styles_css(species = "Atlantic Bluefish",
                  savedir = "C:/Users/samantha.schiano/Documents/GitHub-Repos/ASAR/inst/templates/archive/NEFSC/Atlantic Bluefish/GB/2024")
