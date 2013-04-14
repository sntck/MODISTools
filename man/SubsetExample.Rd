\name{SubsetExample}
\alias{SubsetExample}
\docType{data}
\title{Example dataset for MODISSubsets()}
\description{A dataset consisting of a set of arbitrary locations with a corresponding set of arbitrary start and end time-series dates and IDs.
This file can be used in the example for the MODIS subset function.}
\usage{data(SubsetExample)}
\format{
  A data frame with 1 observation on the following 5 variables.
  \describe{
    \item{lat}{A numeric vector; decimal degrees latitudes in WGS-1984 coordinate system.}
    \item{long}{A numeric vector; decimal degrees longitudes in WGS-1984 coordinate system.}
    \item{start.date}{A numeric vector; listing the date to begin the time-series for each corresponding location.}
    \item{end.date}{A numeric vector; listing the date to end the time-series for each corresponding location.}
    \item{ID}{A numeric vector; a unique ID code for each unique time-series (either unique in time or in space).}
  }
}
\details{Each time-series that has a unique combination of location/time-series dates should ideally have a unique ID. If IDs are not unique, then an arbitrary set of IDs will be assigned
and the optional summary file should be written to maintain transparency in the data collection process.}
\source{Locations were arbitrarily drawn from the grounds of Silwood Park Campus, Imperial College London.}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\keyword{datasets}
