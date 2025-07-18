% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_tagging.r
\name{add_tagging}
\alias{add_tagging}
\title{Add tagging structure to latex documents produced from quarto}
\usage{
add_tagging(
  x = list.files(getwd())[grep("skeleton.tex", list.files(getwd()))],
  dir = getwd(),
  compile = TRUE,
  rename = NULL
)
}
\arguments{
\item{x}{.tex file containing report. Typically produced after initially
rendering the skeleton made from create_template.}

\item{dir}{directory where the tex file is located that will be edited}

\item{compile}{Indicate whether the document (X) should be
rendered after these files are changed. Default TRUE.}

\item{rename}{Indicate a name for the new tex file produced from this
function. There is no need to include ".tex" in the name. Defaults to current
name and overwrites the current tex file.}
}
\value{
This function was made to help add in
latex packages and content associated with PDF
tagging. Quarto does not allow the user to edit anything
before documentclass, so this function alters the rendered .tex file.
}
\description{
Add tagging structure to latex documents produced from quarto
}
\examples{
\dontrun{
create_template(
  new_template = TRUE,
  format = "pdf",
  office = "NWFSC",
  region = "U.S. West Coast",
  species = "Dover sole",
  spp_latin = "Microstomus pacificus",
  year = 2010,
  author = c("John Snow", "Danny Phantom", "Patrick Star"),
  include_affiliation = TRUE,
  new_section = "an_additional_section",
  section_location = "after-introduction"
)

path <- getwd()

quarto::quarto_render(file.path(path, "report", "SAR_USWC_Dover_sole_skeleton.qmd"))

withr::with_dir(
  file.path(path, "report"),
  add_tagging(
    x = "SAR_USWC_Dover_sole_skeleton.tex",
    dir = getwd(),
    compile = TRUE
  )
)
}

}
