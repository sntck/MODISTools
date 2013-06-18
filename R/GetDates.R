GetDates<- function(Lat, Long, Product){
  if(!any(Product == GetProducts())){
    stop("The product name entered does not match any available products. 
           See GetProducts() for available products.")
  }
  
  if(!is.numeric(Lat) | !is.numeric(Long)){
    stop("Lat and Long inputs must be numeric.")
  }
  if(length(Lat) != 1 | length(Long) != 1){
    stop("Incorrect number of Lats and Longs supplied. Input only 1 coordinate (Lat and Long) at a time.")
  }
  if(abs(Lat) > 90 | abs(Long) > 180){
    stop("Detected a lat or long beyond the range of valid coordinates.")
  } 
  
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
  if(colnames(datesres) == "Fault"){
    return(NA)
  } else{
    return(as.vector(datesres))
  }
}