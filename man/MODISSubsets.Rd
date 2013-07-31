\name{MODISSubsets}
\alias{MODISSubsets}
\title{MODIS Subset Tool}
\description{Uses the MODIS SOAP Web Service (see references) as a batch process to remotely extract and locally download time-series subsets of a MODIS product for a given area surrounding many locations.
}
\usage{MODISSubsets(LoadDat, FileSep = NULL, Product, Bands, Size, 
SaveDir = ".", StartDate = FALSE, TimeSeriesLength = 2, Transect=FALSE)
}
\arguments{
  \item{LoadDat}{Input dataset: either the name of an object already in the workspace, or a file to be read in by specifying its file path as a character string, that has location data, dates (end date, and optionally start date) and study ID for each location. For required data organisation, see Details.
  }
  \item{FileSep}{If LoadDat is a character string that corresponds to a file path, choose the delimiter character for that file (e.g. "," for comma separated).
  }
  \item{Product}{Character string; a product code to request subsets from. The MODIS product table shows all available products and their respective product titles (see references).
  }
  \item{Bands}{A character vector; each string a code to retrieve the desired data bands within the specified product to be requested. To get a list of the available bands in a product of interest, use Get.Bands().
  }
  \item{Size}{Numeric vector of two non-negative integers defining the dimensions of tile requested at each location. The first element identifies the distance from the centre to the bottom/top (in both directions), and the second element to the left/right (in both directions) in km. For example, Size=c(0,0) identifies the centre pixel only, whereas Size=c(1,1) identifies a tile of 2kmsq.
  }
  \item{SaveDir}{Character string; an optional argument to specify the subdirectory where downloaded ascii files should be saved: default, SaveDir=".", saves the files to the working directory.
  }
  \item{StartDate}{Logical; indicate whether the input dataset contains information on the time-series start date. If StartDate=TRUE, start dates will be taken from the input data and will expect the data frame to have a column named start.date. Default is StartDate=FALSE, whereby the input data is assumed to have only time-series end date and the length of time-series recorded prior to that date is determined by another optional argument, TimeSeriesLength.
  }
  \item{TimeSeriesLength}{Non-negative integer; if StartDate=FALSE specified the length of time-series. Default TimeSeriesLength=2, giving a time-series length of 3 years, taken from the beginning of the first year, to the end of the last year.
  }
  \item{Transect}{Logical; argument to be used when MODISSubsets() is called in MODISTransects() to merge all subset points for each transect into one file. Should not be optionally used - only useful for MODISTransects().
  }
}
\details{The data should be organised accordingly: "lat" and "long" columns showing WGS-1984 decimal degrees latitudes and longitudes respectively; "end.date" for study end date (and optionally "start.date"), that can be in years or in POSIXlt date format; "ID" containing a unique ID for each unique time-series (in time or space). If IDs do not contain unique time-series, then they will be ignored and unique subset IDs will be assigned. Column names should be exactly as shown.
}
\value{An ascii file written for each unique time-series (unique location, or unique time-series at a duplicated location), containing all data requested, for each band, for every pixel in the specified tile size. A subset download csv file will also be written, that contains all the unique time-series information, and a status report for each time-series telling the user if the respective time-series was downloaded successfully or identifying any problems with that particular download.
}
\references{
  \url{http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html}
  
  \url{https://lpdaac.usgs.gov/products/modis_products_table}
  
  \url{http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl}  
}
\author{Sean Tuck}
\seealso{\code{\link{help}}}
\examples{
\dontrun{ 
# dontrun() used because running the example requires internet access, 
# and takes over a minute to run.
data(SubsetExample)

MODISSubsets(LoadDat=SubsetExample, Product="MOD13Q1", Bands=c("250m_16_days_EVI", 
      "250m_16_days_pixel_reliability"), Size=c(0,0), StartDate=TRUE)

MODISSummaries(LoadDat=SubsetExample, Product="MOD13Q1", Band="250m_16_days_EVI", 
      ValidRange=c(-2000,10000), NoDataFill=-3000, ScaleFactor=0.0001, StartDate=TRUE,
      QualityScreen=TRUE, QualityBand="250m_16_days_pixel_reliability", QualityThreshold=0)
        }    
}