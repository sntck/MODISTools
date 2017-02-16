\name{GetDates}
\alias{GetDates}
\title{Get list of available dates for requested time-series}
\description{Internal function that uses the MODIS SOAP Web Service to retrieve a list of all dates when the requested data, specified by MODIS product and a location, are available for download.}
\usage{GetDates(Lat, Long, Product)}
\arguments{
  \item{Lat}{Numeric; a decimal degrees latitude in WGS-1984 coordinate system.}
  \item{Long}{Numeric; a decimal degrees longitude in WGS-1984 coordinate system.}
  \item{Product}{Character string; a product code to request subsets from. The MODIS product table shows all available products and their respective product titles (see references).}
}
\value{A character vector; each element representing a time-step (in MODIS date format) where data is available.}
\references{
  \url{https://daacmodis.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl}
}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{
\dontrun{ # dontrun() used because running the example requires internet access.
GetDates(Lat=51.41363, Long=-0.64875, Product="MOD13Q1")}
}
