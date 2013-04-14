\name{FindID}
\alias{FindID}
\title{Time-series matching to IDs}
\description{A function that takes a unique time-series definition and looks inside a large dataset to find all the IDs that correspond to records with matching time-series definitions. A useful
function for maintaining transparency in the data collection and processing chain.}
\usage{FindID(x, dat)}
\arguments{
  \item{x}{A data frame with one or a set of rows containing a time-series definition, in term of lat, long, and dates information.}
  \item{dat}{A larger data frame consisting of at least the same information as x, which is to be looked in to find matches.}
}
\details{Rows in x do not need to be the same order as in dat, but the variables in dat to be looked in must have the same names as those in x. Date information can include one date, or optionally a start and end date. Any information in x must be included in dat.}
\value{A set of character strings, listing all the IDs that correspond to the given unique time-series.}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSummaries]{MODISSummaries}}}
\examples{
data(SubsetExample)
data(FindIDExample)
FindID(x=SubsetExample,dat=FindIDExample)
}
