\name{MODISSubsets}
\alias{MODISSubsets}
\title{MODIS Subset Tool}
\description{Uses the MODIS SOAP Web Service (see references) as a batch process to remotely extract and locally download time-series subsets of a MODIS product for a given area surrounding many locations.
}
\usage{MODISSubsets(LoadDat, Products, Bands, Size, StartDate = FALSE,
TimeSeriesLength = 0, ...)
}
\arguments{
  \item{LoadDat}{Input dataset: a data.frame that contains the locations and dates for which MODIS data will be requested. For required data organisation, see Details.
  }
  \item{Products}{Character string; a product code to request subsets from. The MODIS product table shows all available products and their respective product titles (see references). Data from multiple products can be downloaded.
  }
  \item{Bands}{A character vector; the band name codes to retrieve the desired data bands within the specified \code{Products}. To get a list of the available bands in a product of interest, use \code{GetBands()}. Multiple data band across multiple products can be specified and downloaded at the same time.
  }
  \item{Size}{Numeric vector of two non-negative integers defining the size of area of interest surrounding each location. The first element identifies the distance from the centre to the bottom/top (in both directions), and the second element to the left/right (in both directions) in km. For example, \code{Size = c(0,0)} identifies the centre pixel only, whereas \code{Size = c(1,1)} identifies a tile of 2kmsq. The maximum size in either direction is 100.
  }
  \item{StartDate}{Logical; indicate whether the input dataset contains information on the time-series start date. If StartDate = TRUE, start dates will be taken from the input data and will expect the data frame to have a column named start.date. Default is StartDate = FALSE, whereby the input data is assumed to have only time-series end date and the length of time-series recorded prior to that date is determined by another optional argument, TimeSeriesLength.
  }
  \item{TimeSeriesLength}{Non-negative integer; if StartDate = FALSE specified the length of time-series. Default TimeSeriesLength = 0, giving a time-series length of 1 year, taken from the beginning to the end of the year.
  }
  \item{...}{Optional arguments, see Details.
  }
}
\details{The data should be organised accordingly: \code{lat} and \code{long} columns showing WGS-1984 decimal degrees latitudes and longitudes respectively, plus \code{start.date} and \code{end.date} columns to specify the beginning and end of the time period within to search for MODIS data. Dates can be given in years or in POSIXlt date format. Column names should be exactly as shown.

The variable names within LoadDat should have a maximum of one variable containing "ID" in the string, to avoid confusion when looking for unique subset IDs. If no unique subset IDs are found they will be generated.

Optional arguments:
\code{SaveDir} is an optional argument of character type to specify the directory where downloaded ASCII files should be saved: by default files are saved to the working directory. The directory where the downloaded data files will be located is printed at the beginning of the function call. These downloaded files can then be re-loaded into the R environment in a manageable way by using the function \code{MODISTimeSeries}.

}
\value{An ASCII file written for each unique time-series (unique location, or unique time series at a duplicated location), containing all data requested, for each band, for every pixel in the specified tile size. A subset download CSV file will also be written, that contains all the unique time-series information, and a status report for each time series telling the user if the respective time series was downloaded successfully or identifying any problems with that particular download.
}
\references{
  \url{http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html}

  \url{http://daacweb-dev.ornl.gov/MODIS/MODIS-menu/products.html}

  \url{http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl}
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
             "250m_16_days_pixel_reliability"), Size = c(0,0))

MODISSummaries(LoadDat = SubsetExample, Product = "MOD13Q1", Bands = "250m_16_days_EVI",
               ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
               StartDate = TRUE, QualityScreen = TRUE, QualityThreshold = 0,
               QualityBand = "250m_16_days_pixel_reliability")

    }
}