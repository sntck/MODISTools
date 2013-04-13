GetSubset<- function(Lat, Long, Product, Band, StartDate, EndDate, KmAboveBelow, KmLeftRight){
  getsubset.xml<- paste('
    <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:mod="http://daac.ornl.gov/MODIS_webservice">
                        <soapenv:Header/>
                        <soapenv:Body>
                        <mod:getsubset soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
                        <Latitude xsi:type="xsd:float">', Lat, '</Latitude>
                        <Longitude xsi:type="xsd:float">', Long, '</Longitude>
                        <Product xsi:type="xsd:string">', Product, '</Product>
                        <Band xsi:type="xsd:string">', Band, '</Band>
                        <MODIS_Subset_Start_Date xsi:type="xsd:string">', StartDate, '</MODIS_Subset_Start_Date>
                        <MODIS_Subset_End_Date xsi:type="xsd:string">', EndDate, '</MODIS_Subset_End_Date>
                        <Km_Above_Below xsi:type="xsd:string">', KmAboveBelow, '</Km_Above_Below>
                        <Km_Left_Right xsi:type="xsd:string">', KmLeftRight, '</Km_Left_Right>
                        </mod:getsubset>
                        </soapenv:Body>
                        </soapenv:Envelope>',
                      sep="")
  
  header.fields<- 
    c(Accept = "text/xml",
      Accept = "multipart/*",
      'Content-Type' = "text/xml; charset=utf-8",
      SOAPAction="")
  
  reader<- basicTextGatherer()
  header<- basicTextGatherer()
  curlPerform(url = "http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.pl",
              httpheader = header.fields,
              postfields = getsubset.xml,
              writefunction = reader$update,
              verbose=FALSE)

  xmlres<- xmlRoot(xmlTreeParse(reader$value()))
  modisres<- xmlSApply( xmlres[[1]], 
                        function(x) xmlSApply(x,
                                              function(x) xmlSApply(x,
                                                                    function(x) xmlSApply(x,xmlValue))) 
  )
  
  modisres<- as.data.frame(t(unname(modisres[-c(7,11)])))
  names(modisres)<- c("xll", "yll", "pixelsize", "nrow", "ncol", "band", "scale", "lat", "long", "subset")
  
  return(modisres)
}