\name{LandCover}
\alias{LandCover}
\title{Summaries on tiles of land cover data.}
\description{The MODIS product MCD12Q1 contains data bands for land cover types. This function summarises that data, reporting: the most common land cover type, the richness of land cover types, and landscape diversity and heterogeneity using Simpson's D and Simpson's measure of evenness.
}
\usage{
LandCover(Dir=".", Band)
}
\arguments{
  \item{Dir}{Character string; an optional argument to specify the subdirectory where downloaded ascii files to be processed are located and the ouput is written: default Dir="." extracts files from the working directory.
  }
  \item{Band}{Character string; the shortname code that identifies what band type of land cover data from the MCD12Q1 product is being processed.
  }
}
\details{No data fill values are removed from the tiles before summaries are reported. Additional to the no data fill values, the land cover bands include an 'Unclassified' land cover class. This would indicate poor quality, but is not removed.
}
\value{One csv file is written as an output to the function. It contains all of the summaries for each tile input, from all ascii files read in. Each row corresponds to a different tile of pixels - from a different time-step in the time-series, or from a different time-series (ascii file) altogether. Columns are: latitude, longitude, date, land cover band type, most common land cover class, landscape richness, Simpson's D, Simpson's evenness, proportion of data that are no data fill values.
}
\references{
  \url{https://lpdaac.usgs.gov/products/modis_products_table/mcd12q1}
}
\author{Sean Tuck}
\seealso{ \code{\link[MODISTools:MODISSubsets]{MODISSubsets}}
          \code{\link[MODISTools:MODISSummaries]{MODISSummaries}}
}
\examples{
\dontrun{ # dontrun() used because running the example requires internet access.
data(SubsetExample)
# Extend the years to download for
SubsetExample$start.date <- 2001
SubsetExample$end.date <- 2009
MODISSubsets(LoadDat=SubsetExample, Product="MCD12Q1", Bands=c("Land_Cover_Type_1"),
             Size=c(3,3), StartDate=TRUE)

LandCover(Band="Land_Cover_Type_1")
        }
}