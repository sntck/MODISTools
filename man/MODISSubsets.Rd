\name{B. MODISSubsets}
\alias{MODISSubsets}
\title{MODIS Subset Tool}
\description{Uses the MODIS SOAP Web Service and employs the getsubset function (see references) as a batch process to remotely extract and locally download time-series subsets of a 
MODIS product for a given area surrounding many locations.}
\usage{MODISSubsets(LoadDat, LoadMethod = "object" | "ext.file",
FileSep = NULL, Product, Bands, Size = c(), 
SaveDir = NULL, StartDate = FALSE, TimeSeriesLength = 2,
DateFormat = "year" | "posixt", Transect=FALSE)}
\arguments{
  \item{LoadDat}{Input dataset: either a file to be read or an R object that has location data, dates (end date, and optionally start date) and study ID for each location. For required 
  data organisation, see Details.}
  \item{LoadMethod}{Character string; method of location data input. If load.dat is a text file method="ext.file", if load.dat is an object method="object".}
  \item{FileSep}{If method="ext.file", choose the field separator character.}
  \item{Product}{Character string; a product code to request subsets from. The MODIS product table shows all available products and their respective product titles (see references).}
  \item{Bands}{A character vector; each string a code to retrieve the desired data bands within the specified product to be requested. To get a list of the available bands in a product 
  of interest, use Get.Bands().}
  \item{Size}{Numeric vector of two non-negative integers defining the dimensions of tile requested at each location. The first element identifies the distance from the centre to the 
  bottom/top (in both directions), and the second element to the left/right (in both directions) in km. For example, Size=c(0,0) identifies the centre pixel only, whereas Size=c(1,1) 
  identifies a tile of 2kmsq.}
  \item{SaveDir}{Character string; an optional argument to specify the subdirectory where downloaded ascii files should be saved: default save.dir=NULL saves the files to the working directory.}
  \item{StartDate}{Logical; indicate whether the input dataset contains information on the time-series start date. If StartDate=TRUE, start dates will be taken from the input data and will
    expect the data frame to have a column named start.date. Default is StartDate=FALSE, whereby the input data is assumed to have only time-series end date and the length of time-series recorded
    prior to that date is determined by another optional argument, TimeSeriesLength.}
  \item{TimeSeriesLength}{Non-negative integer; if StartDate=FALSE specified the length of time-series. Default TimeSeriesLength=2, giving a time-series length of 3 years, taken from the beginning 
  of the first year, to the end of the last year.}
  \item{DateFormat}{If DateFormat="year", date information is expected to be given as years. If DateFormat="posixt", date information is expected to be given in a POSIXt format (YYYY-MM-DD).
  If DateFormat="posixt" and StartDate=FALSE, the start date is set to the first day of end date-TimeSeries (YYYY-01-01).}
  \item{Transect}{Logical; argument to be used when MODISSubsets() is called in MODISTransects() to merge all subset points for each transect into one file. Should not be optionally used, 
  only useful for MODISTransects().}
}
\details{The data should be organised accordingly: "lat" and "long" columns showing WGS-1984 decimal degrees latitudes and longitudes respectively; "end.date" for study end date (and 
  optionally "start.date"); "ID" containing a unique ID for each unique time-series (in time or space). If IDs do not contain unique time-series, then they will be ignored and unique subset
  IDs will be assigned. Column names should be exactly as shown.}
\value{An ascii file written for each unique time-series (unique location, or unique time-series at a duplicated location), containing all data requested, for each band, for every pixel in 
the specified tile size. A subset download csv file will also be written, that contains all the unique time-series information, and a status report for each time-series telling the user if
the respective time-series was downloaded successfully or identifying any problems with that particular download.}
\references{
  http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html
  
  https://lpdaac.usgs.gov/products/modis_products_table
  
  http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl  
}
\author{Sean Tuck}
\seealso{\code{\link{help}}}
\examples{
\dontrun{data(SubsetExample)
MODISSubsets(LoadDat=SubsetExample, LoadMethod='object', Product='MOD13Q1',     # dontrun() used because running the example requires internet access, and takes a minute or two to run.
    Bands=c('250m_16_days_EVI','250m_16_days_pixel_reliability'), 
    Size=c(0,0), StartDate=TRUE, DateFormat='year')
MODISSummaries(LoadDat=SubsetExample, LoadMethod='object', Band='250m_16_days_EVI', ValidRange=c(-2000,10000), NoDataValue=-3000, ScaleFactor=0.0001)}    
}