% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_title.R
\name{create_title}
\alias{create_title}
\title{Write Stock Assessment Report Title}
\usage{
create_title(
  type = "skeleton",
  office = "",
  species = "species",
  spp_latin = NULL,
  region = NULL,
  year = format(Sys.Date(), "\%Y"),
  complex = NULL
)
}
\arguments{
\item{type}{Type of report to build. Default is SAR (NOAA Fisheries
Stock Assessment Report).}

\item{office}{Regional Fisheries Science Center producing the
report (i.e., AFSC, NEFSC, NWFSC, PIFSC, SEFSC, SWFSC).}

\item{species}{Full common name for target species. Split
naming with a space and capitalize first letter(s). Example:
"Dover sole".}

\item{spp_latin}{Latin name for the target species. Example:
"Pomatomus saltatrix".}

\item{region}{Full name of region in which the species is
evaluated (if applicable). If the region is not specified for
your center or species, do not use this variable.}

\item{year}{Year the assessment is being conducted. Default
is the year in which the report is rendered.}

\item{complex}{TRUE/FALSE; Is this a species complex? Default
is false.}
}
\value{
Return a string containing a title for a NOAA Fisheries stock
assessment report.
}
\description{
Write Stock Assessment Report Title
}
\examples{
create_title(
  type = "SAR", office = "SEFSC", species = "Red Snapper",
  spp_latin = "Lutjanus campechanus", region = "South Atlantic",
  year = 2024
)
}
