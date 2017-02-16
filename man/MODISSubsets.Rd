\name{MODISSubsets}
\alias{MODISSubsets}
\title{MODIS Subset Tool}
\description{Uses the MODIS SOAP Web Service (see references) as a batch process to remotely extract and locally download time-series subsets of a MODIS product for a given area surrounding many locations.
}
\usage{MODISSubsets(LoadDat, FileSep = NULL, Products, Bands, Size,
SaveDir = ".", StartDate = FALSE, TimeSeriesLength = 0, Transect = FALSE)
}
\arguments{
  \item{LoadDat}{Input dataset: either the name of an object already in the workspace, or a file to be read in by specifying its file path as a character string, that has location data, dates (end date, and optionally start date) and study ID for each location. For required data organisation, see Details.
  }
  \item{FileSep}{If LoadDat is a character string that corresponds to a file path, choose the delimiter character for that file (e.g. "," for comma separated).
  }
  \item{Products}{Character string; a product code to request subsets from. The MODIS product table shows all available products and their respective product titles (see references). Data from multiple Products can be downloaded.
  }
  \item{Bands}{A character vector; each string a code to retrieve the desired data bands within the specified product to be requested. To get a list of the available bands in a product of interest, use GetBands(). Multiple data band across multiple products can be specified and downloaded at the same time.
  }
  \item{Size}{Numeric vector of two non-negative integers defining the dimensions of tile requested at each location. The first element identifies the distance from the centre to the bottom/top (in both directions), and the second element to the left/right (in both directions) in km. For example, Size = c(0,0) identifies the centre pixel only, whereas Size = c(1,1) identifies a tile of 2kmsq.
  }
  \item{SaveDir}{Character string; an optional argument to specify a file path to the subdirectory where downloaded ASCII files should be saved: default, SaveDir = ".", saves the files to the working directory. The directory where the downloaded data files will be located is printed at the beginning of the function call. These downloaded files can then be re-loaded into the R environment in a manageable way by using the function MODISTimeSeries.
  }
  \item{StartDate}{Logical; indicate whether the input dataset contains information on the time-series start date. If StartDate = TRUE, start dates will be taken from the input data and will expect the data frame to have a column named start.date. Default is StartDate = FALSE, whereby the input data is assumed to have only time-series end date and the length of time-series recorded prior to that date is determined by another optional argument, TimeSeriesLength.
  }
  \item{TimeSeriesLength}{Non-negative integer; if StartDate = FALSE specified the length of time-series. Default TimeSeriesLength = 0, giving a time-series length of 1 year, taken from the beginning to the end of the year.
  }
  \item{Transect}{Logical; argument to be used when MODISSubsets() is called in MODISTransects() to merge all subset points for each transect into one file. Should not be optionally used - only useful for MODISTransects().
  }
}
\details{The data should be organised accordingly: "lat" and "long" columns showing WGS-1984 decimal degrees latitudes and longitudes respectively; "end.date" for study end date (and optionally "start.date"), that can be in years or in POSIXlt date format; "ID" containing a unique ID for each unique time-series (in time or space). Column names should be exactly as shown.
}
\value{An ASCII file written for each unique time-series (unique location, or unique time-series at a duplicated location), containing all data requested, for each band, for every pixel in the specified tile size. A subset download CSV file will also be written, that contains all the unique time-series information, and a status report for each time-series telling the user if the respective time-series was downloaded successfully or identifying any problems with that particular download.
}
\references{
  \url{https://daacmodis.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl}
}
\author{Sean Tuck}
\seealso{\code{\link{MODISSummaries}}
         \code{\link{MODISTransects}}
         \code{\link{MODISGrid}}
         \code{\link{MODISTimeSeries}}
}
\examples{
\dontrun{
# dontrun() used because running the example requires internet access.

data(SubsetExample)

MODISSubsets(LoadDat = SubsetExample, Products = "MOD13Q1", Bands = c("250m_16_days_EVI",
             "250m_16_days_pixel_reliability"), Size = c(0,0), StartDate = TRUE)

MODISSummaries(LoadDat = SubsetExample, Product = "MOD13Q1", Bands = "250m_16_days_EVI",
               ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
               StartDate = TRUE, QualityScreen = TRUE, QualityThreshold = 0,
               QualityBand = "250m_16_days_pixel_reliability")

    }
}
