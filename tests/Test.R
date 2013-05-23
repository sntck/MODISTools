# Testing for internet connectivity, the connection to the MODIS SOAP WSDL Server and it's Web Service
# Description Language, for the XML response from the Web Service method, and for the functions of
# MODISTools.

# Load data to be used for testing.
data(SubsetExample)

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

# Check MODIS subset uses this output to produce correctly downloaded files.
MODISSubsets(LoadDat=SubsetExample, Product="MOD13Q1",
          Bands=c("250m_16_days_EVI","250m_16_days_pixel_reliability"),
          Size=c(0,0), StartDate=TRUE, DateFormat="year")

