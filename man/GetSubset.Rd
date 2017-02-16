\name{GetSubset}
\alias{GetSubset}
\title{Subset download using MODIS web service method.}
\description{Internal function that uses the MODIS SOAP Web Service (see references) getsubset method to download a requested time-series subset of a MODIS product for a given area surrounding many locations. This function is used by the MODISSubsets function, which is the main subset download tool for the user.}
\usage{GetSubset(Lat, Long, Product, Band, StartDate, EndDate, KmAboveBelow, KmLeftRight)}
\arguments{
  \item{Lat}{Numeric; a decimal degrees latitude in WGS-1984 coordinate system.}
  \item{Long}{Numeric; a decimal degrees longitude in WGS-1984 coordinate system.}
  \item{Product}{A character string; a product code to request the subset from. The MODIS product table shows all available products and their respective product titles (see references).}
  \item{Band}{A character string; a code or vector of codes to retrieve the desired data band(s) within the specified product to be requested. To get a list of the available bands in a product of interest use GetBands().}
  \item{StartDate}{Character; in MODIS date format, listing the date to begin the time-series for each corresponding location.}
  \item{EndDate}{Character; in MODIS date format, listing the date to end the time-series for each corresponding location.}
  \item{KmAboveBelow}{An integer; the distance, in kilometres, from the centre of the tile of pixels to the top and bottom of the tile. So, if KmAboveBelow=1, the total distance from top to bottom of the tile will be 2km.}
  \item{KmLeftRight}{An integer; the distance, in kilometres, from the centre of the tile of pixels to the left and right sides of the tile. So, if KmLeftRight=1, the total distance from side to side of the tile will be 2km.}
}
\value{A data frame containing:
xll=The x coordinate, in the MODIS coordinate system, of the lower left corner of the pixel in which the location data falls.
yll=The y coordinate, in the MODIS coordinate system, of the lower left corner of the pixel in which the location data falls.
pixelsize=The actual calculated width of the pixels of interest, in metres.
nrow=Corresponds to KmAboveBelow argument.
ncol=Corresponds to KmLeftRight argument.
band=Corresponds to band argument.
scale=The number by which raw downloaded data values can be scaled by. Specific to each band.
lat=Corresponds to the Lat argument.
long=Corresponds to the Long argument.
subset=The downloaded data, as a vector of character strings, with length number of time-steps requested, that include data attached to metadata.

This data frame is then passed to MODISSubsets.}
\references{
  \url{https://daacmodis.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl}
}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{
\dontrun{
# dontrun() used because running the example requires internet access,
# and takes over a minute to run.
data(SubsetExample)
GetSubset(Lat=SubsetExample[ ,1], Long=SubsetExample[ ,2], Product="MOD13Q1",
    Band="250m_16_days_EVI", StartDate="A2001001", EndDate="A2001025",
    KmAboveBelow=0, KmLeftRight=0)}
}
