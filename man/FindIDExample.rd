\name{FindIDExample}
\alias{FindIDExample}
\docType{data}
\title{Example dataset for FindIDSubsets()}
\description{A dataset consisting of a set of arbitrary locations with a corresponding set of arbitrary start and end time-series dates and IDs.
This file can be used in the example for the function that takes a set of time-series and finds all IDs for records in a larger data frame that match the time-series definition.}
\usage{data(FindIDExample)}
\format{
  A data frame with 4 observations on the following 5 variables.
  \describe{
    \item{lat}{A numeric vector; decimal degrees latitudes in WGS-1984 coordinate system.}
    \item{long}{A numeric vector; decimal degrees longitudes in WGS-1984 coordinate system.}
    \item{start.date}{A numeric vector; listing the date to begin the time-series for each corresponding location.}
    \item{end.date}{A numeric vector; listing the date to end the time-series for each corresponding location.}
    \item{ID}{A numeric vector; a unique ID code for each unique time-series (either unique in time or in space).}
  }
}
\details{Rows in ID do not need to be the same order as in Data, but the variables in Data to be looked in must have the same names as those in ID. Date information can include one date, or optionally a start and end date. Any information in ID must be included in Data.}
\source{Locations were arbitrarily drawn from the grounds of Silwood Park Campus, Imperial College London.}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\keyword{datasets}