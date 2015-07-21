\name{UpdateSubsets}
\alias{UpdateSubsets}
\title{Updating Subset List}
\description{A function that identifies, from a given dataset, which subsets have already been downloaded and returns a dataframe of those yet to be downloaded.  A useful function if the list of locations for subsets is continually increasing or if download was halted previously.
}
\usage{UpdateSubsets(LoadDat, StartDate=FALSE, Dir=".")}
\arguments{
  \item{LoadDat}{Input dataset: either the name of an object already in the workspace, or a file to be read in by specifying its file path as a character string, that has location data, end date (end.date) and study ID for each location.  	
  	}
  \item{StartDate}{Logical: Specifying whether StartDate was specified in the original MODISSubset download.}	
  \item{Dir}{String: Directory where previously downloaded subsets are saved. Default is current working directory.}
}
\details{The input dataset should be organised accordingly: "lat" and "long" columns showing WGS-1984 decimal degrees latitudes and longitudes respectively; "end.date" for study end date.}
\value{A dataframe containing any data for which a subset has not been downloaded.  This can then be used with MODISSubsets.}
\author{Helen Phillips and Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{
\dontrun{
# dontrun() used because running the example requires internet access.

data(SubsetExample, ConvertExample)

modis.subset <- ConvertToDD(XY = ConvertExample, LatColName = "lat", LongColName = "long")
modis.subset <- data.frame(lat = c(SubsetExample$lat, modis.subset[ ,1]),
                           long = c(SubsetExample$long, modis.subset[ ,2]),
                           start.date = rep(SubsetExample$start.date, 9),
                           end.date = rep(SubsetExample$end.date, 9))

MODISSubsets(LoadDat = SubsetExample, Product = "MOD13Q1", Bands = c("250m_16_days_EVI", 
             "250m_16_days_pixel_reliability"), Size = c(0,0), StartDate = TRUE)

Updated.modis.subset <- UpdateSubsets(LoadDat = modis.subset, StartDate = TRUE)

MODISSubsets(LoadDat = Updated.modis.subset, Product = "MOD13Q1", Bands = c("250m_16_days_EVI", 
             "250m_16_days_pixel_reliability"), Size = c(0,0), StartDate = TRUE)
  }
}
