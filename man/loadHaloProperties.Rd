% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadHaloProperties.R
\name{loadHaloProperties}
\alias{loadHaloProperties}
\title{Load extra halo properties}
\usage{
loadHaloProperties(subtree, filename = NULL, verbose = TRUE)
}
\arguments{
\item{subtree}{A structured list specifying the datasets to read from the HDF5 file.}

\item{filename}{Full path of the HDF5 halo file. If \code{NULL}, the value stored in `swift$paths$halos` will be used, which can be set with \code{\link{setPath}}.}

\item{verbose}{Logical flag to control whether progress and timing information should be printed in console.}
}
\value{
None. Modifies `swift$halos` in place.
}
\description{
Reads additional halo properties from a SOAP HDF5 file and appends them to the existing halo table.
}
\details{
This function updates `swift$halos` by appending new columns based on datasets specified in the `subtree`. The variable `halos` is bound via an active binding to refer directly to `swift$halos` within the function scope.
}
