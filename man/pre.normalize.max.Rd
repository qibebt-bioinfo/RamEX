% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pre-normalize.R
\name{pre.normalize.max}
\alias{pre.normalize.max}
\title{Pre-process Raman data by max normalization}
\usage{
pre.normalize.max(object)
}
\arguments{
\item{object}{A Ramanome object.}

\item{wave}{The specific wavenumber to use for the "max" normalization method.}
}
\value{
The updated Ramanome object with the max normalized Raman data.
}
\description{
This function applies normalization to the Raman data based on the max method and updates the Raman object.
}
