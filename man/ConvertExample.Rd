\name{ConvertExample}
\alias{ConvertExample}
\docType{data}
\title{Example dataset for ConvertToDD}
\description{A dataset consisting of a set of arbitrary locations in degrees minutes seconds (DMS) format that ConvertToDD can convert to decimal degrees format.}
\usage{data(ConvertExample)}
\format{
  A data frame with 4 observations on the following 2 variables.
  \describe{
    \item{\code{lat}}{A dataframe column containing latitude degrees minutes seconds (DDdMMmSS.SSSsN. ConvertToDD can take other formats and layouts in general.}
    \item{\code{long}}{A dataframe column containing longitude degrees minutes seconds (DDdMMmSS.SSSsW. ConvertToDD can take other formats and layouts in general.}
  }
}
\details{The various compatible layouts for each format are described at ?ConvertToDD (see also). If hemispheres are denoted using letters (N/S; E/W) then the same transformation (+ive -ive coordinates) will
be carried out on all points. Therefore, in this case, if points within a dataset are in different hemispheres, they will be all be placed into one.}
\source{Locations were arbitrarily drawn from the grounds of Silwood Park Campus, Imperial College London.}
\seealso{\code{\link[MODISTools:ConvertToDD]{ConvertToDD}}}
\keyword{datasets}
