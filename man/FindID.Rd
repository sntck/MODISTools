\name{FindID}
\alias{FindID}
\title{Time-series matching to IDs}
\description{A function that takes a unique time-series definition and looks inside a large dataset to find all the IDs that correspond to records with matching time-series definitions. A useful
function for maintaining transparency in the data collection and processing chain.}
\usage{FindID(ID, Data)}
\arguments{
  \item{ID}{A data frame with one or a set of rows containing a time-series definition, in terms of lat, long, and dates information.}
  \item{Data}{A larger data frame consisting of at least the same information as ID, which is to be looked in to find matches.}
}
\details{Rows in ID do not need to be the same order as in Data, but the variables in dat to be looked in must have the same names as those in ID. Date information can include one date, or optionally a start and end date. Any information in ID must be included in Data.}
\value{A set of character strings, listing all the IDs that correspond to the given unique time-series.}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSummaries]{MODISSummaries}}}
\examples{
data(SubsetExample, FindIDExample)
FindID(ID=SubsetExample, Data=FindIDExample)
}
