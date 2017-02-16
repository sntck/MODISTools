\name{MODISSummaries}
\alias{MODISSummaries}
\title{MODIS subset processing & organisation tool
}
\description{A function to run time-series analysis and compute summary statistics for a downloaded MODIS subset, writing a summary file and another file with processed MODIS data tagged onto the original file inputted to MODISSubsets. This function allows the user to easily explore the characteristics of the downloaded data, and then process them into a form that is ready for use in modelling.
}
\usage{MODISSummaries(LoadDat, FileSep = NULL, Dir = ".", Product, Bands, ValidRange, NoDataFill,
ScaleFactor, StartDate = FALSE, QualityScreen = FALSE, QualityBand = NULL,
QualityThreshold = NULL, Mean = TRUE, SD = TRUE, Min = TRUE, Max = TRUE, Yield = FALSE,
Interpolate = FALSE, InterpolateN = NULL, DiagnosticPlot = FALSE)
}
\arguments{
  \item{LoadDat}{Input dataset: either the name of an object already in the workspace, or a file to be read in by specifying its file path as a character string, that has location data, dates (end date, and optionally start date) and study ID for each location. If IDs are found in LoadDat that provide a primary key for unique time series then these IDs will be used. Otherwise a set of unique IDs will be generated and used to identify, and file name, each time series.
  }
  \item{FileSep}{If LoadDat is a character string that corresponds to a file path, choose the delimiter character for that file (e.g. "," for comma separated).
  }
  \item{Dir}{Character string; an optional argument to specify a file path to the subdirectory where downloaded ASCII files to be processed are located and the output is written: default Dir = "." extracts files from the working directory.
  }
  \item{Product}{Character; The product shortname code, that the data band input belongs to. The MODIS product table shows all available products and their respective product shortname codes (see references).
  }
  \item{Bands}{Character; the code that identifies from which band types are the data to be processed. Multiple bands can be specified as a character vector, including the quality control data bands, providing they all come from the same product. With the exception of BRDF Reflectance data products (MCD43A4) that have quality information stored as a separate product (MCD43A2).
  }
  \item{ValidRange}{Numeric vector of two elements; states the lower (ValidRange[1]) and upper (ValidRange[2]) bounds within which the data to be processed should be found.
  }
  \item{NoDataFill}{Numeric; the missing data fill value that is used for Bands.
  }
  \item{ScaleFactor}{Numeric; The specified scaling for the given band type, which the data is to be multiplied by. If a scale factor does not exist for the data band, ScaleFactor should be set to 1.
  }
  \item{StartDate}{Logical; indicate whether the input dataset contains information on the time-series start date. If StartDate = TRUE, start dates will be taken from the input data and will expect the data frame to have a column named start.date. Default is StartDate = FALSE, whereby the input data is assumed to have only time-series end date. This should be the same as that used in the relevant call to MODISSubsets.
  }
  \item{QualityScreen}{Logical; optional argument for screening the band data for unreliable pixels. If QualityScreen = TRUE, band data must be downloaded from MODISSubsets with the quality control data corresponding to the same product included. Therefore, both band data and reliability data will be in the same ASCII files for each time-series downloaded. Quality screening is completed by the QualityCheck function, and the arguments for this function need to be included in a MODISSummaries call, if QualityScreen = TRUE. The default is QualityScreen = FALSE, meaning the function will omit data equal to NoDataFill, but will not omit poor quality data.
  }
  \item{QualityBand}{Character; if QualityScreen = TRUE, the shortname code for the quality data band that you are using to screen Band for poor quality data.
  }
  \item{QualityThreshold}{Numeric integer; if QualityScreen = TRUE, set the threshold between acceptable and unacceptable quality. Any pixels of lower quality than the class set by QualityThreshold will be removed, and those equal to or of higher quality will be kept. QualityThreshold should be a number within the range of possible QualityScores for the given Product QA data.
  }
  \item{Mean,
        SD,
        Min,
        Max,
        Yield}{Logical; optional arguments that allow selecting which summaries will be included in the summary file that gets written - see value. Selecting Yield requires Interpolate to also be set as TRUE.
  }
  \item{Interpolate}{Logical; determines whether, after poor quality data is removed, to linearly interpolate between high quality data before calculating the summary statistics. Must be TRUE if Yield = TRUE. The interpolation function used is stats::approx. See ?stats::approx for more details.
  }
  \item{InterpolateN}{Numeric; if Interpolate = TRUE, optionally set the number interpolated data points to be requested from the time-series interpolation. The default is set to a daily interpolation of the data.
  }
  \item{DiagnosticPlot}{Logical; if TRUE will produce an additional folder in the specified directory to which plots of the time series data for each site will be saved. Will add the interpolation line, mean, min and max values if specified in the function call.
  }
}
\details{If QualityScreen = TRUE, subsets to be processed should include a pixel reliability layer, so the data can be screened for poor quality data, removing them and using linear interpolation to refill data between high quality values.
}
\value{Two CSV files:
One file (MODIS_Summary...) contains summary statistics and computed values for each data. The information this file contains is partly defined by the optional arguments settings: Mean is arithmetic mean; SD is standard deviation; Min and Max are minimum and maximum band values; Yield is the average annual yield (designed for vegetation indices, may not be sensible for all band types); NoFill and PoorQuality show the percentage of values in each time-series that were NoDataFill and omitted by QualityCheck (if QualityScreen = TRUE) respectively. All summary statistics, except yield, are included by default.

The second file (MODIS_Data...) that has the information from the original file inputted (which should have been used in MODISSubsets too) with computed means of the MODIS data tagged on, coupling the input with the output in one form ready for use, such as modelling. In the second file, each nth column of MODIS data, if more than one, will be for each pixel within the whole tile of n pixels collected for the time-series on that row.
}
\references{
  \url{https://daacmodis.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl}
}
\author{Sean Tuck}
\seealso{ \code{\link[MODISTools:MODISSubsets]{MODISSubsets}}
          \code{\link[MODISTools:QualityCheck]{QualityCheck}}
}
\examples{
\dontrun{
# dontrun() used because running the example requires internet access,
# and takes over a minute to run.

data(SubsetExample)

MODISSubsets(LoadDat = SubsetExample, Products = "MOD13Q1",
             Bands = c("250m_16_days_EVI", "250m_16_days_NDVI", "250m_16_days_pixel_reliability"),
             Size = c(0,0), StartDate = TRUE)

# Without quality checking
MODISSummaries(LoadDat = SubsetExample, Product = "MOD13Q1", Bands = "250m_16_days_EVI",
               ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
               StartDate = TRUE)

# With quality checking
MODISSummaries(LoadDat = SubsetExample, Product = "MOD13Q1", Bands = "250m_16_days_EVI",
               ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
               StartDate = TRUE, QualityScreen = TRUE, QualityThreshold = 0,
               QualityBand = "250m_16_days_pixel_reliability")

# For both EVI and NDVI
MODISSummaries(LoadDat = SubsetExample, Product = "MOD13Q1",
               Bands = c("250m_16_days_EVI","250m_16_days_NDVI"),
               ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
               StartDate = TRUE, QualityScreen = TRUE, QualityThreshold = 0,
               QualityBand = "250m_16_days_pixel_reliability")
    }
}
