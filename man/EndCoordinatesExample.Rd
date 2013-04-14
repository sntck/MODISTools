\name{EndCoordinatesExample}
\alias{EndCoordinatesExample}
\docType{data}
\title{Example dataset for EndCoordinates()}
\description{A dataset consisting of a set of arbitrary locations with a corresponding set of arbitrary start and end time-series dates and IDs.
This file can be used in the example for the MODIS EndCoordinates function.}
\usage{data(EndCoordinatesExample)}
\format{
  A data frame with 4 observation on the following 5 variables.
  \describe{
    \item{start.lat}{A numeric vector; decimal degrees latitudes in WGS-1984 coordinate system.}
    \item{start.long}{A numeric vector; decimal degrees longitudes in WGS-1984 coordinate system.}
    \item{start.date}{A numeric vector; listing the date to begin the time-series for each corresponding locations. Used during MODISTransects.}
    \item{end.date}{A numeric vector; listing the date to end the time-series for each corresponding locations. Used during MODISTransects.}
    \item{ID}{A numeric vector; a unique ID code for each unique location.}
  }
}
\source{Locations were arbitrarily drawn from the grounds of Silwood Park Campus, Imperial College London.}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\keyword{datasets}
