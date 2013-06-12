\name{MODISSummaries}
\alias{MODISSummaries}
\title{MODIS Subset Processing & Organisation Tool}
\description{A function to run time-series analysis and compute summary statistics for a downloaded MODIS subset, writing a summary file and another file with processed MODIS data tagged onto the original file inputted to MODISSubsets(). This function allows the user to easily explore the characteristics of the downloaded data, and then process them into a form that is ready for use in modelling.
}
\usage{MODISSummaries(LoadDat, FileSep=NULL, Dir=".", Band, ValidRange, 
NoDataValue, ScaleFactor, StartDate=FALSE, QualityScreen=FALSE, QualityControl=NULL, 
Mean=TRUE, SD=TRUE, Min=TRUE, Max=TRUE, Yield=FALSE, NoFill=TRUE, PoorQuality=TRUE)
}
\arguments{
  \item{LoadDat}{Input dataset: either the name of an object already in the workspace, or a file to be read in by specifying its file path as a character string, that has location data, dates (end date, and optionally start date) and study ID for each location.
  }
  \item{FileSep}{If LoadDat is a character string that corresponds to a file path, choose the delimiter character for that file (e.g. "," for comma separated).
  }
  \item{Dir}{Character string; an optional argument to specify the subdirectory where downloaded ascii files to be processed are located and written: default Dir="." extracts files from the working directory.
  }
  \item{Band}{Character string; the code that identifies from which band type is the data to be processed. In the input file (LoadDat), alongside the data for this band type, there should be a corresponding set of pixel reliability values included for quality control.
  }
  \item{ValidRange}{Numeric vector of two elements; states the lower (ValidRange[1]) and upper (ValidRange[2]) bounds within which the data to be processed should be found.
  }
  \item{NoDataValue}{States what is used to identify missing values in the subset data to be processed.
  }
  \item{ScaleFactor}{Numeric; The specified scaling for the given band type, which the data is to be multiplied by.
  }
  \item{StartDate}{Logical; indicate whether the input dataset contains information on the time-series start date. If StartDate=TRUE, start dates will be taken from the input data and will expect the data frame to have a column named start.date. Default is StartDate=FALSE, whereby the input data is assumed to have only time-series end date. This should be the same as that used in the relevant call to MODISSubsets.
  }
  \item{QualityScreen}{Logical; optional argument for screening the band data for unreliable pixels. If QualityScreen=TRUE, band data must be downloaded from MODISSubsets with the pixel reliability data corresponding to the same product included. Therefore, both band data and reliability data will be in the same ascii files for each time-series downloaded. QualityControl must also be set, 0-3 (integers), 0 being highest quality, 3 being lowest. The default for QualityScreen is FALSE, meaning the function will omit data equal to NoDataValue, but will not omit poor quality data.
  }
  \item{QualityControl}{The pixel reliability value that marks the threshold for quality control: any band values with a corresponding reliability value > QualityControl will be omitted. Therefore, if QualityControl=3, all band values will be kept. Default is NULL - the user must specify what threshold to use, if QualityScreen=TRUE.
  }
  \item{Mean,
  SD,
  Min,
  Max,
  Yield,
  NoFill,
  PoorQuality}{Logical; optional arguments that allow selecting which summaries will be included in the summary file that gets written. Mean is arithmetic mean. SD is standard deviation. Min and Max are minimum and maximum band values. Yield is the average annual yield (may not be sensible for all band types). NoFill and PoorQuality show the percentage of values in each time-series that were NoDataValue and omitted by QualityControl respectively. All summary statistics, except yield, are included by default.
  }
}
\details{Subsets to be processed should include a pixel reliability layer, so the data can be screened for high quality data only, using linear interpolation between high quality values.
}
\value{Two csv files; One file containing summary statistics and computed values for each data, and one file that has the information from the original file inputted (which should have been used in MODISSubsets() too) with computed MODIS data tagged on, coupling the input with the output in one form ready for use, such as modelling.
}
\references{
  \url{http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html}
  
  \url{https://lpdaac.usgs.gov/products/modis_products_table}
  
  \url{http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl}
}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{
\dontrun{ # dontrun() used because running the example requires internet access, and takes a minute or two to run.
data(SubsetExample)

MODISSubsets(LoadDat=SubsetExample, Product="MOD13Q1",     
    Bands=c("250m_16_days_EVI","250m_16_days_pixel_reliability"), 
    Size=c(0,0), StartDate=TRUE, DateFormat="year")

MODISSummaries(LoadDat=SubsetExample, Band="250m_16_days_EVI", 
    ValidRange=c(-2000,10000), NoDataValue=-3000, ScaleFactor=0.0001, 
    StartDate=TRUE)}  
}