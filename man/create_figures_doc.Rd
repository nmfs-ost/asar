% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_figures_doc.R
\name{create_figures_doc}
\alias{create_figures_doc}
\title{Create Quarto Document of Figures}
\usage{
create_figures_doc(subdir = NULL, include_all = TRUE, rda_dir = NULL)
}
\arguments{
\item{subdir}{Location of subdirectory storing the assessment report template}

\item{include_all}{TRUE/FALSE; Option to include all default
figures for a stock assessment report. Default is true.}

\item{rda_dir}{If the user has already created .rda files containing
figures, tables, alt text, and captions with \code{stockplotr}, rda_dir represents
the location of the folder containing these .rda files ("rda_files").
Otherwise, if the user has not used \code{stockplotr} to make those .rda files already,
those files will be generated automatically and placed within an "rda_files"
folder within rda_dir. The "rda_files" folder would have been
made with \code{stockplotr::exp_all_figs_tables()}, or by exporting files by running individual
\code{stockplotr} figure- and table-generating functions. If you have used \code{stockplotr} to
generate these .rda files, you can leave the arguments below blank. NOTE:
If an "rda_files" folder is detected within rda_dir, .rda files will not be
regenerated.}
}
\value{
A quarto document with pre-loaded R chunk that adds the
stock assessment tables from the nmfs-ost/stockplotr R package. The
quarto document will become part of the stock assessment outline.
}
\description{
Create Quarto Document of Figures
}
\examples{
\dontrun{
create_figures_doc(
subdir = getwd(),
rda_dir = here::here())
}
}
