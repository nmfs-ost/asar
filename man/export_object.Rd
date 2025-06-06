% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_object.R
\name{export_object}
\alias{export_object}
\title{Export report object}
\usage{
export_object(object = NULL, file_format = "docx", subdir = NULL)
}
\arguments{
\item{object}{Table, plot, or other object in the R environment
to export for reference outside of the report document.}

\item{file_format}{The format of the exported file. Options include
pdf, docx, xlsx, csv, and rdata. Default is docx.}

\item{subdir}{Directory where object will be exported.}
}
\value{
Objects put through this function will be put into the
folder "exported" for better organization; DO NOT PUSH THESE TO THE REPO
}
\description{
Function to export specified object from R environment found in the
stock assessment report.
}
\examples{
source <- c("NMFS Groundfish Survey", "", "U.S. Trawl Fisheries", "", "")
data <- c("Survey biomass", "Age Composition", "Catch", "Age Composition", "Length Composition")
years <- c(
  "1984-1999 (triennial), 2001-2013 (biennial)",
  "1984, 1987, 1990, 1993, 1996, 1999, 2003, 2005, 2007, 2009, 2011", "1961-2013",
  "1990,1998-2002, 2004, 2005, 2006, 2008, 2010", "1963-1977, 1991-1997"
)
test_obj <- data.frame(source, data, years)

export_object(object = test_obj, file_format = "csv", subdir = "~")

}
\author{
Samantha Schiano
}
