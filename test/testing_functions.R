# testing functions

# Example script that allows the user to try out creating stock assessment report templates
# In development
# Last edited: 03/14/24

### PLEASE DO NOT PUSH ANY TEMPLATES THAT YOU MAKE TESTING OUT THIS FUNCTION###
# More guidance for saving scripts will be provided in the future

# test generating new template
create_template(
    new_template = TRUE,
    format = "pdf",
    office = "NEFSC",
    region = "GB",
    species = "Bluefish",
    year = 2024,
    author = c("Kelli Johnson", "Ben Williams", "Dan Hennen"),
    include_affiliation = TRUE,
    parameters = TRUE,
    param_names = c("fleet1", "fleet2", "model"),
    param_values = c("Commercial", "Recreational", "Woods Hole Assessment Model"),
    type = "RT"
)


# new_template = TRUE
# format = "pdf"
# office = "NEFSC"
# region = "GB"
# species = "Bluefish"
# year = 2024
# new_author = FALSE
# include_affiliation = FALSE
# parameters = TRUE
# param_names = c("office","region","species")
# param_values = c("NEFSC","Georges Bank", "Bluefish")
# add_section = FALSE
# type = "RT"

# Author affiliation reference
  # cat(paste("format: \n",
  #           "  " , "pdf", ": \n",
  #           "  ", "  ", "keep-tex: ", "true \n",
  #           "  " , "  ", "template-partials: \n",
  #           "  ", "  ", "  ", " - title.tex \n",
  #           "  ", "  ", "  ", " - sadraft.tex \n",
  #           "  ", "  ", "include-in-header: \n",
  #           "  ", "  ", "  ", "text: | \n",
  #           "  ", "  ", "  ", "  ", "\\usepackage[noblocks]{authblk} \n",
  #           "  ", "  ", "  ", "  ", "\\renewcommand*{\\Authsep}{, } \n",
  #           "  ", "  ", "  ", "  ", "\\renewcommand*{\\Authand}{, } \n",
  #           "  ", "  ", "  ", "  ", "\\renewcommand*{\\Authands}{, } \n",
  #           "  ", "  ", "  ", "  ", "\\renewcommand\\Affilfont{\\small} \n",
  #           sep = ""))

# Test template pull from previous assessment
# build "old template"
create_template(
  new_template = TRUE,
  format = "pdf",
  office = "SEFSC",
  region = "Gulf of Mexico",
  species = "Red Snapper",
  year = 2021,
  author = c("Lisa Ailloud", "Kyle Shertzer", "Katie Siegfried"),
  include_affiliation = TRUE,
  parameters = FALSE,
  type = "RT"
)

# Pull new one
create_template(new_template = FALSE,
                prev_year = 2021,
                year = 2024,
                office = "SEFSC",
                region = "Gulf of Mexico",
                species = "Red Snapper",
                type = "RT"
                )

# new_template = FALSE
# prev_year = 2021
# year = 2024
# office = "SEFSC"
# region = "Gulf of Mexico"
# species = "Red Snapper"
# type = "RT"
