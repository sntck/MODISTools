\name{F. GetProducts}
\alias{GetProducts}
\title{Get MODIS Product List}
\description{Function that uses the MODIS SOAP Web Service (function getproducts) to retrieve a list of all of the codes for available MODIS products.}
\usage{GetProducts()}
\arguments{No input.}
\value{A character vector; containing the codes for each product.}
\references{
  http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html
  
  http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl
  
  https://lpdaac.usgs.gov/products/modis_products_table
}    
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{
\dontrun{GetProducts()} # dontrun() used because running the example requires internet access.
}