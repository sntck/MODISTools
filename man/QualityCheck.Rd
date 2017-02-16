\name{QualityCheck}
\alias{QualityCheck}
\title{Remove unreliable data and no fill values from MODIS data}
\description{Designed as an internal function for MODISSummaries, but can also be used independently. MODIS data of a given product band are screened for poor quality pixels, using the reliability data available for that product. The missing data values are also filtered out. The output is the same MODIS data, with screened poor quality and missing data converted to NAs.
}
\usage{
QualityCheck(Data, Product, Band, NoDataFill, QualityBand, QualityScores,
              QualityThreshold)
}
\arguments{
  \item{Data}{Numeric vector or matrix; the input MODIS data, for QualityCheck to screen for poor quality and missing data.
  }
  \item{Product}{Character; the product code for Data (e.g. "MOD15A2" if you want to screen the quality of a downloaded data band from the leaf area index product).
  }
  \item{Band}{Character; the shortname code for the data band that you are quality screening.
  }
  \item{NoDataFill}{Numeric; the missing data fill value for the data band (Data) that you are screening.
  }
  \item{QualityBand}{Character; the shortname code for the quality data band that you are using to screen Band for poor quality data.
  }
  \item{QualityScores}{Numeric integer vector or matrix; the quality control control that matches your Data input. QualityScores should be the same length/dimensions as Data.
  }
  \item{QualityThreshold}{Numeric integer; set the threshold between acceptable and unacceptable quality. Any pixels of lower quality than the class set by QualityThreshold will be removed, and those equal to or of higher quality will be kept. QualityThreshold should be a number within the range of possible QualityScores for the given Product QA data.
  }
}
\details{Although there are several data bands within each MODIS product, only one band type of data should be passed to QualityCheck at a given time, as different bands within a product may have different NoDataFill values, and may even have different data bands that contain their relevant quality information.
}
\value{Data, as it was, but with poor quality data and no data fill values replaced with NAs.
}
\references{
  \url{https://daacmodis.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl}
}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSummaries]{MODISSummaries}}}
\examples{
\dontrun{
# dontrun() used because running the example requires internet access.
data(QualityCheckExample)
EVIdata <- QualityCheckExample[1:5, ]
QAdata <- QualityCheckExample[6:10, ]

QualityCheck(Data=EVIdata, Product="MOD13Q1", Band="250m_16_days_EVI",
              NoDataFill=-3000, QualityBand="250m_16_days_pixel_reliability",
              QualityScores=QAdata, QualityThreshold=0)
        }
}
