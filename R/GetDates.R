GetDates<- function(Lat, Long, Product){
  getdates.xml<- paste('
    <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:mod="http://daac.ornl.gov/MODIS_webservice">
      <soapenv:Header/>
      <soapenv:Body>
        <mod:getdates soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
          <Latitude xsi:type="xsd:float">', Lat, '</Latitude>
          <Longitude xsi:type="xsd:float">', Long, '</Longitude>
          <Product xsi:type="xsd:string">', Product, '</Product>
        </mod:getdates>
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
              postfields = getdates.xml,
              writefunction = reader$update,
              verbose=FALSE)
  
  xmlres<- xmlRoot(xmlTreeParse(reader$value()))
  datesres<- xmlSApply( xmlres[[1]], 
                        function(x) xmlSApply(x,
                                              function(x) xmlSApply(x,
                                                                    function(x) xmlSApply(x,xmlValue))) 
  )
  
  return(as.vector(datesres))
}