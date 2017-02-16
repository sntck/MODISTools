\name{MODISTransects}
\alias{MODISTransects}
\title{MODIS Transect Subset Tool}
\description{A function that downloads batches of MODIS data transects.
}
\usage{MODISTransects(LoadData, FileSep = NULL, Product, Bands, Size,
SaveDir = ".", StartDate = FALSE, TimeSeriesLength = 0)
}
\arguments{
  \item{LoadData}{Input data: either the name of an object already in the workspace, or a file to be read in by specifying its file path as a character string. For data input requirements, see details. All arguments to be passed to MODISSubsets(); see ?MODISSubsets for more information.
  }
  \item{FileSep}{If LoadData is a character string that corresponds to a file path, choose the delimiter character for that file (e.g. "," for comma separated).
  }
  \item{Product}{Character string; code denoting which MODIS product to be requested.
  }
  \item{Bands}{Character vector; which bands of data within specified product are of interest.
  }
  \item{Size}{Numeric vector of two non-negative integers defining the dimensions of tile requested at each location. The first element identifies the distance from the centre to the bottom/top (in both directions), and the second element to the left/right (in both directions) in km. For example, Size = c(0,0) identifies the centre pixel only, whereas Size = c(1,1) identifies a tile of 2kmsq.
  }
  \item{SaveDir}{Character string; an optional argument to specify the subdirectory where downloaded ascii files should be saved: default SaveDir = "." saves the files to the working directory.
  }
  \item{StartDate}{Logical; indicate whether the input dataset contains information on the time-series start date. If StartDate = TRUE, start dates will be taken from the input data and will expect the data frame to have a column named start.date. Default is StartDate = FALSE, whereby the input data is assumed to have only time-series end date and the length of time-series recorded prior to that date is determined by another optional argument, TimeSeriesLength.
  }
  \item{TimeSeriesLength}{Non-negative integer; if StartDate = FALSE specified the length of time-series. Default TimeSeriesLength = 0, giving a time-series length of 1 year, taken from the beginning to the end of the year.
  }
}
\details{File input requirements are a transect ID, a start lat and long, and an end lat and long for each transect, and an end date (start date optional) for the requested time-series for each transect. These fields must be labelled with headers: "transect"; "start.lat"; "start.long"; "end.lat"; "end.long"; "start.date", and; "end.date" respectively.

All data types requested must be of the same resolution (pixel size). See ?MODISSubsets for more details.

The transects lengths requested must be longer than the requested pixel size. Therefore, each transect must contain more than 1 pixel.
}
\value{ASCII file written for each location, containing the specified time-series for each band of data requested. File name states the transect ID that the location is within, and a unique ID for that specific location.
}
\references{
  \url{https://daacmodis.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl}
}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{
\dontrun{
# dontrun() used because running the example requires internet access,
# and takes over a minute to run.
data(TransectExample)

MODISTransects(LoadData = TransectExample, Product = "MOD13Q1",
    Bands = c("250m_16_days_EVI", "250m_16_days_pixel_reliability"),
    Size = c(0,0), StartDate = TRUE)
    }
}
