\name{GetBands}
\alias{GetBands}
\title{Get MODIS Product Data Band Names}
\description{Internal function that uses the MODIS SOAP Web Service to retrieve a list of all of the codes to identify the data bands within a specified MODIS product.}
\usage{GetBands(Product)}
\arguments{
  \item{Product}{Character string; the unique code for a given MODIS product. Will only accept one product code. See references for the product list.}
}
\value{A character vector; containing the codes for each data band.}
\references{
  \url{https://daacmodis.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl}
}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{
\dontrun{ # dontrun() used because running the example requires internet access.
GetBands("MOD13Q1")}
}
