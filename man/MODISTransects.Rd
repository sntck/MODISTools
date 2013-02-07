\name{D. MODISTransects}
\alias{MODISTransects}
\title{MODIS Transect Subset Tool}
\description{A function that downloads batches of MODIS data transects.}
\usage{MODISTransects(LoadData, LoadMethod = "object" | "ext.file", 
FileSep = NULL, Product, Bands, Size = c(), SaveDir = "./", StartDate = FALSE, 
TimeSeriesLength = 2, DateFormat = "year" | "posixt")}
\arguments{
  \item{LoadData}{Input data: file or object in R environment. For data input requirements, see details. All arguments to be passed to MODISSubsets(); see ?MODISSubsets for more information.}
  \item{LoadMethod}{Character string; method of location data input. If LoadData is a text file LoadMethod="ext.file", if LoadData is an object LoadMethod="object".}
  \item{FileSep}{If LoadMethod="ext.file", choose the field separator character.}
  \item{Product}{Character string; code denoting which MODIS product to be requested.}
  \item{Bands}{Character vector; which bands of data within specified product are of interest.}
  \item{Size}{Numeric vector of two non-negative integers defining the dimensions of tile requested at each location. The first element identifies the distance from the centre to the 
  bottom/top (in both directions), and the second element to the left/right (in both directions) in km. For example, Size=c(0,0) identifies the centre pixel only, whereas Size=c(1,1) 
  identifies a tile of 2kmsq.}
  \item{SaveDir}{Character string; an optional argument to specify the subdirectory where downloaded ascii files should be saved: default SaveDir="./" saves the files to the working directory.}
  \item{StartDate}{Logical; indicate whether the input dataset contains information on the time-series start date. If StartDate=TRUE, start dates will be taken from the input data and will
    expect the data frame to have a column named start.date. Default is StartDate=FALSE, whereby the input data is assumed to have only time-series end date and the length of time-series recorded
    prior to that date is determined by another optional argument, TimeSeriesLength.}
  \item{TimeSeriesLength}{Non-negative integer; if StartDate=FALSE specified the length of time-series. Default TimeSeriesLength=2, giving a time-series length of 3 years, taken from the beginning 
  of the first year, to the end of the last year.}
  \item{DateFormat}{If DateFormat="year", date information is expected to be given as years. If DateFormat="posixt", date information is expected to be given in a POSIXt format (YYYY-MM-DD).
  If DateFormat="posixt" and StartDate=FALSE, the start date is set to the first day of end date-TimeSeries (YYYY-01-01).}
}
\details{File input requirements are a transect ID, a start lat and long, and an end lat and long for each transect, and an end date (start date optional) for the requested time-series 
for each transect. These fields must be labelled with headers: "transect"; "start.lat"; "start.long"; "end.lat"; "end.long"; "start.date", and; "end.date" respectively.
All data types requested must be of the same resolution (pixel size). See ?MODIS.Subsets for more details.}
\value{ascii file written for each location, containing the specified time-series for each band of data requested. File name states the transect ID that the location is within, and a unique
ID for that specific location.}
\references{
  http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html
  
  http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl
  
  https://lpdaac.usgs.gov/products/modis_products_table
}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{
\dontrun{data(TransectExample)
MODISTransects(LoadData=TransectExample, LoadMethod='object', Product='MOD13Q1',     # dontrun() used because running the example requires internet access, and takes a minute or two to run.
    Bands=c('250m_16_days_EVI','250m_16_days_pixel_reliability'), 
    Size=c(0,0), StartDate=TRUE, DateFormat='year')}
}