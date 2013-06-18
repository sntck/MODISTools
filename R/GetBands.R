GetBands<- function(Product){
  if(!any(Product == GetProducts())){
    stop("The product name entered does not match any available products. 
           See GetProducts() for available products.")
  }
  
  getbands.xml<- paste('
    <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:mod="http://daac.ornl.gov/MODIS_webservice">
      <soapenv:Header/>
      <soapenv:Body>
        <mod:getbands soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
          <Product xsi:type="xsd:string">', Product, '</Product>
        </mod:getbands>
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
              postfields = getbands.xml,
              writefunction = reader$update,
              verbose=FALSE)
  
  xmlres<- xmlRoot(xmlTreeParse(reader$value()))
  bandsres<- xmlSApply( xmlres[[1]], 
                        function(x) xmlSApply(x,
                                              function(x) xmlSApply(x,xmlValue)) 
  )
  
  if(colnames(bandsres) == "Fault"){
    return(NA)
  } else{
    return(as.vector(bandsres))
  }
}
