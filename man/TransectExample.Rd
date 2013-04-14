\name{TransectExample}
\alias{TransectExample}
\docType{data}
\title{Example dataset for MODISTransects()}
\description{A dataset consisting of a set of arbitrary transect start/end locations with a corresponding set of arbitrary start and end time-series dates and transect IDs.
This file can be used in the example for the MODIS subset function.}
\usage{data(TransectExample)}
\format{
  A data frame with 1 observation on the following 7 variables.
  \describe{
    \item{transect}{A numeric vector; a unique ID code for each transect.}
    \item{start.lat}{A numeric vector; decimal degrees latitudes in WGS-1984 coordinate system for the transect start point.}
    \item{start.long}{A numeric vector; decimal degrees latitudes in WGS-1984 coordinate system for the transect start point.}
    \item{end.lat}{A numeric vector; decimal degrees latitudes in WGS-1984 coordinate system for the transect end point.}
    \item{end.long}{A numeric vector; decimal degrees latitudes in WGS-1984 coordinate system for the transect end point.}
    \item{start.date}{A numeric vector; listing the year to begin the time-series for each corresponding location.}
    \item{end.date}{A numeric vector; listing the year to end the time-series for each corresponding location.}
  }
}
\source{Locations were arbitrarily drawn from the grounds of Silwood Park Campus, Imperial College London.}
\seealso{\code{\link[MODISTools:MODISTransects]{MODISTransects}}}
\keyword{datasets}