\name{UpdateSubsets}
\alias{UpdateSubsets}
\title{Updating Subset List}
\description{A function that identifies, from a given dataset, what subsets have already been downloaded into a directory and returns a dataframe of those yet to be downloaded.  A useful function if the list of locations for subsets is continually increasing, or if download was halted previously.
}
\usage{UpdateSubsets(LoadDat, Dir)}
\arguments{
  \item{LoadDat}{Input dataset: either the name of an object already in the workspace, or a file to be read in by specifying its file path as a character string, that has location data, end date (end.date) and study ID for each location.  	
  	}
  \item{Dir}{Directory where previously downloaded subsets are saved. Default is current working directory.}
}
\details{The input dataset should be organised accordingly: "lat" and "long" columns showing WGS-1984 decimal degrees latitudes and longitudes respectively; "end.date" for study end date.}
\value{A dataframe containing any data for which a subset has not been downloaded.  This can then be used with MODISSubsets.}
\author{Helen Phillips and Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{blah blah blah}
