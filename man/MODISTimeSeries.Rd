\name{MODISTimeSeries}
\alias{MODISTimeSeries}
\title{Create one matrix of data from many downloaded timeseries.
}
\description{Take many downloaded timeseries of a MODIS data band and simplify them into one list or matrix of data, with a row for each date and a column for each unique timeseries (i.e., each ASCII file).
}
\usage{
MODISTimeSeries(Dir, Band, Simplify = FALSE)
}
\arguments{
  \item{Dir}{Character; the directory in which to search for downloaded ASCII files of MODIS data.
  }
  \item{Band}{Character; the MODIS data band shortname for the downloaded data.
  }
  \item{Simplify}{Logical; specifies whether the data output should be presented as a list (by default) or simplified to one matrix. See details for more information.}
}
\details{Dir can contain files other than the ASCII files containing data for Band, including other MODIS ASCII files. But, only the data for Band will be included in the output dataset. The names of the downloaded ASCII files must be their original names, containing their correct data product code. The timeseries length of different subsets can be different (the output dataset will contain NAs in the final rows of any timeseries that are shorter than the maximum).

If Simplify = FALSE, each element in the list represents a unique time series tile as a matrix, with columns for each pixel in that tile and rows for each date in the time series. If Simplify = TRUE, the matrices for each time series tile will be appended together (columnwise) to form one large matrix with the same number of rows (for each date) but many columns (for all pixels of interest). If, however, the number of dates varies in different timeseries, the number of rows in each tile matrix will not be equal and these data cannot be simplified into a matrix. In this case, the data will always be returned as a list.
}
\value{A list with elements for each time series tile (i.e., each MODIS data file), and a matrix with columns for pixels and rows for dates in each element. Or, one large matrix with rows for each date and columns for each unique pixel across all MODIS data of interest (found in Dir).
}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}
         \code{\link[MODISTools:MODISSummaries]{MODISSummaries}}
}
\examples{
\dontrun{
# dontrun() used because running the example requires internet access, 
# and takes over a minute to run.
time.series <- data.frame(lat = c(51.41363, 51.41421),
                          long = c(-0.64875, -0.641607),
                          start.date = c(2002, 2002),
                          end.date = c(2004, 2004),
                          ID = c(1, 2))

MODISSubsets(LoadDat = time.series, Product = "MOD13Q1",     
    Bands = c("250m_16_days_EVI"), Size = c(0,0), StartDate = TRUE)

ts <- MODISTimeSeries(Dir = ".", Band = "250m_16_days_EVI", Simplify = FALSE)
class(ts)
dim(ts[[1]])
dim(ts[[2]])

ts <- MODISTimeSeries(Dir = ".", Band = "250m_16_days_EVI", Simplify = TRUE)
class(ts)
dim(ts)

time.series$end.date[2] <- 2005
MODISSubsets(LoadDat = time.series, Product = "MOD13Q1",     
    Bands = c("250m_16_days_EVI"), Size = c(1,1), StartDate = TRUE)
    
ts <- MODISTimeSeries(Dir = ".", Band = "250m_16_days_EVI", Simplify = TRUE)
class(ts)
  }
}