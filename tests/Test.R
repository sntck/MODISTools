# Testing for internet connectivity, the connection to the MODIS SOAP WSDL Server and it's Web Service
# Description Language, for the XML response from the Web Service method, and for the functions of
# MODISTools.

# Load data to be used for testing.
data(SubsetExample, FindIDExample, QualityCheckExample, TransectExample, 
     EndCoordinatesExample, ConvertExample)

# Check for internet capability.
if(!capabilities()["http/ftp"]) q()

# Check for internet connectivity.
if(.Platform$OS.type == "unix" && is.null(nsl("cran.r-project.org"))) q()

# Check we can reach the server for lpdaac modis web service.
if(.Platform$OS.type == "unix" && is.null(nsl("daac.ornl.gov"))) q()

urlCheck<- try(curlPerform(url="http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.pl"))
if(class(urlCheck) == "try-error") q()

# Check the XML response is as expected.
getsubset.xml <- paste('
    <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:mod="http://daac.ornl.gov/MODIS_webservice">
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
                      sep="")

header.fields <- c(Accept = "text/xml",
                    Accept = "multipart/*",
                   'Content-Type' = "text/xml; charset=utf-8",
                    SOAPAction="")

reader <- basicTextGatherer()
header <- basicTextGatherer()
curlPerform(url = "http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.pl",
          httpheader = header.fields,
          postfields = getsubset.xml,
          writefunction = reader$update,
          verbose=FALSE)

xmlRoot(xmlTreeParse(reader$value()))

# Check GetSubset is producing the correct output.
# Use GetProducts, GetBands, and GetDates, to specify the GetSubset request.
Product <- GetProducts()[1]
Band <- GetBands(Product)[1]
Dates <- GetDates(SubsetExample$lat, SubsetExample$long, Product)[1:2]

GetSubset(Lat=SubsetExample$lat, Long=SubsetExample$long, Product=Product, Band=Band, 
          StartDate=Dates[1], EndDate=Dates[1], KmAboveBelow=0, KmLeftRight=0)

# Check FindID example
FindID(ID=SubsetExample, Data=FindIDExample)

# Check QualityCheck example
EVIdata <- QualityCheckExample[1:5, ]
QAdata <- QualityCheckExample[6:10, ]

QualityCheck(Data=EVIdata, Product="MOD13Q1", Band="250m_16_days_EVI", NoDataFill=-3000, 
          QualityBand="250m_16_days_pixel_reliability", QualityScores=QAdata, QualityThreshold=0)

# Check MODIS subset uses this output to produce correctly downloaded files.
MODISSubsets(LoadDat=SubsetExample, Product="MOD13Q1",
          Bands=c("250m_16_days_EVI","250m_16_days_pixel_reliability"),
          Size=c(0,0), StartDate=TRUE)

# Check example run of MODISSummaries.
MODISSummaries(LoadDat=SubsetExample, Product="MOD13Q1", Band="250m_16_days_EVI", 
          ValidRange=c(-2000,10000), NoDataFill=-3000, ScaleFactor=0.0001, StartDate=TRUE,
          QualityScreen=TRUE, QualityBand="250m_16_days_pixel_reliability", QualityThreshold=0)

# Check the MODISSummaries file outputs are consistent.
SummaryFile <- read.csv(list.files(pattern="MODIS Summary"))
DataFile <- read.csv(list.files(pattern="MODIS Data"))
# Take mean.band column from summary and arrange into matrix byrow=T, to put pixels in each tile to each
# row, like data file. Take all unique rows, based on band.pzels columns, from data file to make the matrix
# as that made from summary file. If all points in matrix match, then bingo.

# Check example of MODISTransects
MODISTransects(LoadData=TransectExample, Product="MOD13Q1",     
          Bands=c("250m_16_days_EVI", "250m_16_days_pixel_reliability"), 
          Size=c(0,0), StartDate=TRUE)

# Check EndCoordinates example
EndCoordinates(LoadDat=EndCoordinatesExample, Distance = 2000, Angle = 90, AngleUnits = "degrees")

# Check ConvertToDD example
ConvertToDD(XY=ConvertExample, LatColName="lat", LongColName="long")

