\name{MODISTimeSeries}
\alias{MODISTimeSeries}
\title{Create one matrix of data from many downloaded timeseries.
}
\description{Take many downloaded timeseries of a MODIS data band and collapse them into one matrix of data, with a row for each date and a column for each unique timeseries (i.e., each ASCII file).
}
\usage{
MODISTimeSeries(Dir, Band)
}
\arguments{
  \item{Dir}{Character; the directory in which to search for downloaded ASCII files of MODIS data.
  }
  \item{Band}{Character; the MODIS data band shortname for the downloaded data.
  }
}
\details{Dir can contain files other than the ASCII files containing data for Band, including other MODIS ASCII files. But, only the data for Band will be included in the output dataset. The names of the downloaded ASCII files must be their original names, containing their correct data product code. The timeseries length of different subsets can be different (the output dataset will contain NAs in the final rows of any timeseries that are shorter than the maximum).
}
\value{A matrix, with rows for each date in the timeseries, and columns for each unique timeseries.
}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}
         \code{\link[MODISTools:MODISSummaries]{MODISSummaries}}
}
\examples{
\dontrun{
# dontrun() used because running the example requires internet access, 
# and takes over a minute to run.
data(SubsetExample)

MODISSubsets(LoadDat = SubsetExample, Product = "MOD13Q1",     
    Bands = c("250m_16_days_EVI","250m_16_days_pixel_reliability"), 
    Size = c(0,0), StartDate = TRUE)

MODISTimeSeries(Dir = ".", Band = "250m_16_days_EVI")
  }
}