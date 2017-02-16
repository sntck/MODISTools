# Full length testing script for MODISTools R package.
# ----------------------------------------------------
# The CRAN testing farms has time constraints, creating
# a limit to the amount of testing possible. This full
# length script complements a smaller version in the R
# package itself, which is to be run locally before
# releasing any updates.
# -----------------------------------------------------
# This testing checks for internet connectivity, the
# connection to the MODIS SOAP WSDL Server and it's Web
# Service Description Language, for the XML response from
# the Web Service method, and for the functions of MODISTools.
# ===========================================================

# Load data to be used for testing.
rm(list = ls())
library(MODISTools)
data(SubsetExample, FindIDExample, QualityCheckExample, TransectExample,
     EndCoordinatesExample, ConvertExample)
library(RCurl)  # Will use some RCurl and XML functions explicitly in testing.
library(XML)

options(warn = 2)

### Following lines of code testing for internet connectivity and server access, are
### from R testing: .../tests/internet.R
# Check for internet capability.
if(!capabilities("http/ftp")) q()

# Check for internet connectivity.
if(.Platform$OS.type == "unix" && is.null(nsl("cran.r-project.org"))) q()

# Check we can reach the server for lpdaac modis web service.
if(.Platform$OS.type == "unix" && is.null(nsl("daac.ornl.gov"))) q()

# Check the web service is currently responsive.
if(class(try(GetProducts(), silent = TRUE)) == "try-error") q()

# Check MODIS subset uses this output to produce correctly downloaded files.
if(grepl("Server is busy handling other requests",
         GetSubset(Lat = SubsetExample$lat, Long = SubsetExample$long, Product = "MCD12Q1",
                   Band = "Land_Cover_Type_1", StartDate = "A2005001", EndDate = "A2005001",
                   KmAboveBelow = 0, KmLeftRight = 0)$subset[1])){
  q()
}
###

### Check the XML response is as expected.
getsubset.xml <- paste('
<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                       xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:mod="', daacmodis, '/MODIS_webservice">
                       <soapenv:Header/>
                       <soapenv:Body>
                       <mod:getsubset soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
                       <Latitude xsi:type="xsd:float">', 51.41363, '</Latitude>
                       <Longitude xsi:type="xsd:float">', -0.64875, '</Longitude>
                       <Product xsi:type="xsd:string">', "MOD13Q1", '</Product>
                       <Band xsi:type="xsd:string">', "250m_16_days_EVI", '</Band>
                       <MODIS_Subset_Start_Date xsi:type="xsd:string">', "A2001001", '</MODIS_Subset_Start_Date>
                       <MODIS_Subset_End_Date xsi:type="xsd:string">', "A2001025", '</MODIS_Subset_End_Date>
                       <Km_Above_Below xsi:type="xsd:string">', 0, '</Km_Above_Below>
                       <Km_Left_Right xsi:type="xsd:string">', 0, '</Km_Left_Right>
                       </mod:getsubset>
                       </soapenv:Body>
                       </soapenv:Envelope>',
                        sep = "")

header.fields <- c(Accept = "text/xml",
                   Accept = "multipart/*",
                   'Content-Type' = "text/xml; charset=utf-8",
                   SOAPAction = "")

reader <- basicTextGatherer()
header <- basicTextGatherer()

curlPerform(url = paste0(daacmodis, wsdl_doc),
            httpheader = header.fields,
            postfields = getsubset.xml,
            writefunction = reader$update,
            verbose = FALSE)

# Check the server is not down by insepcting the XML response for internal server error message.
if(grepl("Internal Server Error", reader$value())) q()

xmlRoot(xmlTreeParse(reader$value()))
###

### Check some internal functions
# Check FindID example
FindID(ID = SubsetExample, Data = FindIDExample)

# Check QualityCheck example
EVIdata <- QualityCheckExample[1:5, ]
QAdata <- QualityCheckExample[6:10, ]

QualityCheck(Data = EVIdata, Product = "MOD13Q1", Band = "250m_16_days_EVI",
             NoDataFill = -3000, QualityBand = "250m_16_days_pixel_reliability",
             QualityScores = QAdata, QualityThreshold = 0)

# Check the web service get methods
GetProducts()
GetBands('MOD13Q1')
GetDates(SubsetExample$lat, SubsetExample$long, 'MOD13Q1')
###


### Check the main functions: MODISSubsets, MODISSummaries, and MODISTransects

# Check we can still reach the server for lpdaac modis web service before running functions that request.
if(.Platform$OS.type == "unix" && is.null(nsl("daac.ornl.gov"))) q()
# Check MODIS subset uses this output to produce correctly downloaded files.
if(grepl("Server is busy handling other requests",
         GetSubset(Lat = SubsetExample$lat, Long = SubsetExample$long, Product = "MCD12Q1", Band = "Land_Cover_Type_1",
                   StartDate = "A2005001", EndDate = "A2005001", KmAboveBelow = 0, KmLeftRight = 0)$subset[1])){
  q()
} else {
  # Check GetSubset is producing the correct output.
  Dates <- GetDates(FindIDExample$lat[1], FindIDExample$long[1], Product = 'MOD13Q1')[1:10]

  gsub.try <- try(GetSubset(Lat = FindIDExample$lat[1], Long = FindIDExample$long[1],
                            Product = 'MOD13Q1', Band = '250m_16_days_EVI', StartDate = Dates[1],
                            EndDate = Dates[5], KmAboveBelow = 0, KmLeftRight = 0))
  if(class(gsub.try) == "try-error") q()
  gsub.try

  ###
  # MODISSubsets check using multiple bands and multiple-pixel tiles over multiple time-steps.
  MODISSubsets(LoadDat = FindIDExample, Product = "MOD13Q1",
               Bands = c("250m_16_days_EVI","250m_16_days_NDVI","250m_16_days_pixel_reliability"),
               Size = c(1,1), StartDate = TRUE)
  ###
}

# Check we can still reach the server for lpdaac modis web service before running functions that request.
if(.Platform$OS.type == "unix" && is.null(nsl("daac.ornl.gov"))) q()
# Check example run of MODISSummaries.
if(grepl("Server is busy handling other requests",
         GetSubset(Lat = SubsetExample$lat, Long = SubsetExample$long, Product = "MOD13Q1", Band = "250m_16_days_EVI",
                   StartDate = "A2000049", EndDate = "A2000049", KmAboveBelow = 0, KmLeftRight = 0)$subset[1])){
  q()
} else {

  ###
  # MODISSummaries check without QualityCheck
  MODISSummaries(LoadDat = FindIDExample, Product = "MOD13Q1",
                 Bands = c("250m_16_days_EVI","250m_16_days_NDVI"), ValidRange = c(-2000,10000),
                 NoDataFill = -3000, ScaleFactor = 0.0001, StartDate = TRUE)

  # MODISSummaries check with QualityCheck
  MODISSummaries(LoadDat = FindIDExample, Product = "MOD13Q1",
                 Bands = c("250m_16_days_EVI","250m_16_days_NDVI"), ValidRange = c(-2000,10000),
                 NoDataFill = -3000, ScaleFactor = 0.0001, StartDate = TRUE, QualityScreen = TRUE,
                 QualityBand = "250m_16_days_pixel_reliability", QualityThreshold = 0)
  ###
}

###
# Check the MODISSummaries file outputs are consistent.
SummaryFile <- read.csv(list.files(pattern = "MODIS_Summary")[1])
DataFile <- read.csv(list.files(pattern = "MODIS_Data")[1])
file.check <- all(SummaryFile$mean.band == DataFile[1,which(grepl("pixel", names(DataFile)))])
if(is.na(file.check)){
  warning("The two output files from MODISSummaries are not consistent.")
}
if(!file.check){
  warning("The two output files from MODISSummaries are not consistent.")
}
###

# Check again that the web service is responsive.
if(class(try(GetProducts(), silent = TRUE)) == "try-error") q()

# Check we can still reach the server for lpdaac modis web service before running functions that request.
if(.Platform$OS.type == "unix" && is.null(nsl("daacmodis.ornl.gov"))) q()
# Check example of MODISTransects
if(grepl("Server is busy handling other requests",
         GetSubset(Lat = SubsetExample$lat, Long = SubsetExample$long, Product = "MOD13Q1", Band = "250m_16_days_EVI",
                   StartDate = "A2000049", EndDate = "A2000049", KmAboveBelow = 0, KmLeftRight = 0)$subset[1])){
  q()
} else {

  ###
  MODISTransects(LoadData = TransectExample, Product = "MCD12Q1", Bands = c("Land_Cover_Type_1"),
                 Size = c(0,0), StartDate = TRUE)
  ###
}

### End of checking for main functions

### Checking for secondary functions
# Check EndCoordinates example
EndCoordinates(LoadDat = EndCoordinatesExample, Distance = 2000, Angle = 90, AngleUnits = "degrees")

# Check ConvertToDD example
convert.deg1 <- ConvertToDD(XY = ConvertExample, LatColName = "lat", LongColName = "long")
convert.eg2 <- data.frame(lat = c("51d24.106'S","51d24.922'S","51d24.106'S","51d24.772'S",
                                  "51d24m51.106sS","51d24m37.922sS","51d24m42.106sS","51d24m47.772sS"),
                          long = c("0d38.018'E","0d38.772'E","0d38.664'E","0d38.043'E","0d38m56.018sE",
                                   "0d38m31.772sE","0d38m17.664sE","0d38m42.043sE"))
convert.deg2 <- ConvertToDD(XY = convert.eg2, LatColName = "lat", LongColName = "long")

if(!all.equal(convert.deg1, (convert.deg2 * -1))) stop('ConvertToDD function not translating hemispheres correctly.')

# Check ExtractTile example
TileExample <- read.csv(list.files(pattern = "MODIS_Data")[1])
TileExample <- TileExample[1,which(grepl("pixel", names(TileExample)))[1:81]]

dim(TileExample)
matrix(TileExample, nrow = 9, ncol = 9, byrow = TRUE)

dim(ExtractTile(Data = TileExample, Rows = c(9,1), Cols = c(9,1), Grid = FALSE))
ExtractTile(Data = TileExample, Rows = c(9,1), Cols = c(9,1), Grid = FALSE)

ExtractTile(Data = TileExample, Rows = c(9,1), Cols = c(9,1), Grid = TRUE)

# Check LandCover on previously downloaded data from MODISSubsets
LandCover(Band = "Land_Cover_Type_1")

# Check MODISTimeSeries example
MODISTimeSeries(Dir = ".", Band = "250m_16_days_EVI")

# Check UpdateSubsets
rm(list = ls())
dir.create('./UpdateSubsetsEx')
setwd('./UpdateSubsetsEx')
data(SubsetExample, FindIDExample)

MODISSubsets(LoadDat = SubsetExample, Product = "MOD13Q1",
             Bands = c("250m_16_days_EVI","250m_16_days_pixel_reliability"),
             Size = c(0,0), StartDate = TRUE)
list.files()

reduced.subset <- UpdateSubsets(LoadDat = FindIDExample)
MODISSubsets(LoadDat = reduced.subset, Product = "MOD13Q1",
             Bands = c("250m_16_days_EVI","250m_16_days_pixel_reliability"),
             Size = c(0,0), StartDate = TRUE)
list.files()
###

rm(list = ls())
options(warn = 0)

### END OF TESTING SCRIPT
