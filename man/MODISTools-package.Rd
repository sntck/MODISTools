\name{A. MODISTools}
\alias{MODIS}
\docType{package}
\title{MODIS subsetting tools}
\description{Provides a batch method of retrieving subsets of MODIS data and processing them to a format ready for user friendly application in R such as statistical modelling.}
\details{Imports SSOAP(>= 0.91-0), XMLSchema (>= 0.8-0), and XML and RCurl. The main purpose of this package is to employ the MODIS SOAP Web Service to remotely interrogate the MODIS archive and locally download the requested datasets. The most important function is MODISSubsets, which allows the user to request subsets from a given MODIS product for multiple time-series. Each time-series is defined by a coordinate location (WGS-1984), a specified surrounding extent of pixels, and a start and end date. The relevant MODIS product data is then extracted for these location/time combinations. Automating this as a batch process greatly reduces time, effort, and human error. Alternatively, MODISTransects expands upon MODISSubsets by extracting MODIS data along a transect, and its surrounding neighbourhood. Downloaded subsets are saved in ascii files, which are then accessed by MODISSummaries for computing summary statistics. It also organises downloaded data back with the original input data into a csv file that can be easily used for modelling; this provides efficient storage of data and a transparent process from data collection, to processing, to a form that is ready for final use.}
\author{Sean Tuck}
\references{
  http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html
  
  http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl
  
  https://lpdaac.usgs.gov/products/modis_products_table
  
  http://www.omegahat.org/SSOAP/
}  
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
