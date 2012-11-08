\name{E. GetBands}
\alias{GetBands}
\title{Get MODIS Product Data Band Names}
\description{Function that uses the MODIS SOAP Web Service (function getbands) to retrieve a list of all of the codes to identify the data bands within a specified MODIS product.}
\usage{GetBands(Product)}
\arguments{
  \item{Product}{Character string; the unique code for a given MODIS product. Will only accept one product code. See references for the product list.}
}
\value{A character vector; containing the codes for each data band.}
\references{
  http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html
  
  http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl
  
  https://lpdaac.usgs.gov/products/modis_products_table
}    
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{
\dontrun{GetBands('MOD13Q1')} # dontrun() used because running the example requires internet access.
}