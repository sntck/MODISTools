\name{GetProducts}
\alias{GetProducts}
\title{Get MODIS Product List}
\description{Internal function that uses the MODIS SOAP Web Service to retrieve a list of all of the codes for available MODIS products.}
\usage{GetProducts()}
\arguments{No input.}
\value{A character vector; containing the codes for each product.}
\references{
  \url{http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html}

  \url{http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl}

  \url{http://daacweb-dev.ornl.gov/MODIS/MODIS-menu/products.html}
}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{
\dontrun{ # dontrun() used because running the example requires internet access.
GetProducts()}
}