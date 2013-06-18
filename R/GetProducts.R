GetProducts<- function(){
  getproducts.xml<- paste('
    <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:mod="http://daac.ornl.gov/MODIS_webservice">
      <soapenv:Header/>
      <soapenv:Body>
        <mod:getproducts soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"/>
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
              postfields = getproducts.xml,
              writefunction = reader$update,
              verbose=FALSE)
  
  xmlres<- xmlRoot(xmlTreeParse(reader$value()))
  productsres<- xmlSApply( xmlres[[1]], 
                           function(x) xmlSApply(x,
                                                 function(x) xmlSApply(x,xmlValue)) 
  )
  
  if(colnames(productsres) == "Fault"){
    return(NA)
  } else{
    return(as.vector(productsres))
  }
}