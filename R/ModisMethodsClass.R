# =============================================================================
# A class definition for ModisMethods, a class containing misc static methods.
# These methods are used across several core functions and, organised in this
# way, do not require instantiating an unnecessary object.
# =============================================================================

.ModisMethods <- R6Class(".ModisMethods",
    public = list(
      ##### Public methods
      getSubset = function(lat, long, product, band, startDate, endDate, kmAboveBelow, kmLeftRight)
      {
        ## Send XML request to MODIS SOAP web service and retrieve the XML response subset data.
        xmlRequest <- paste0('
          <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:mod="http://daac.ornl.gov/MODIS_webservice">
                <soapenv:Header/>
                <soapenv:Body>
                <mod:getsubset soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
                <Latitude xsi:type="xsd:float">', lat, '</Latitude>
                <Longitude xsi:type="xsd:float">', long, '</Longitude>
                <Product xsi:type="xsd:string">', product, '</Product>
                <Band xsi:type="xsd:string">', band, '</Band>
                <MODIS_Subset_Start_Date xsi:type="xsd:string">', startDate, '</MODIS_Subset_Start_Date>
                <MODIS_Subset_End_Date xsi:type="xsd:string">', endDate, '</MODIS_Subset_End_Date>
                <Km_Above_Below xsi:type="xsd:string">', kmAboveBelow, '</Km_Above_Below>
                <Km_Left_Right xsi:type="xsd:string">', kmLeftRight, '</Km_Left_Right>
                </mod:getsubset>
                </soapenv:Body>
                </soapenv:Envelope>')

        headerFields <- c(Accept = "text/xml",
                          Accept = "multipart/*",
                          "Content-Type" = "text/xml; charset=utf-8",
                          SOAPAction = '')

        reader <- basicTextGatherer()
        header <- basicTextGatherer()

        curlPerform(url = "http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.pl",
                    httpheader = headerFields,
                    postfields = xmlRequest,
                    writefunction = reader$update,
                    verbose = FALSE)

        ## Check the server is not down by insepcting the XML response for internal server error message.
        if(grepl("Internal Server Error", reader$value()))
          stop("Web service failure: the ORNL DAAC server seems to be down, please try again later.")

        xmlResponse <- xmlRoot(xmlTreeParse(reader$value()))
        modisResponse <- xmlSApply(xmlResponse[[1]],
                         function(x) xmlSApply(x,
                              function(x) xmlSApply(x,
                                   function(x) xmlSApply(x,xmlValue))))

        if(colnames(modisResponse) == "Fault"){
          if(length(modisResponse["faultstring.text", ][[1]]) == 0){
            stop("Downloading from the web service is currently not working. Please try again later.")
          }
          stop(modisResponse["faultstring.text", ])
        } else {
          modisResponse <- data.frame(t(unname(modisResponse[-c(7,11)])))
          names(modisResponse) <- c("xll", "yll", "pixelsize", "nrow", "ncol", "band", "scale", "lat", "long", "subset")
          return(modisResponse)
        }
      },
      ##
      getProducts = function()
      {
        ## Send XML request to MODIS SOAP web service and retrieve the XML response products list.
        xmlRequest <- paste0('
          <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:mod="http://daac.ornl.gov/MODIS_webservice">
                <soapenv:Header/>
                <soapenv:Body>
                <mod:getproducts soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"/>
                </soapenv:Body>
                </soapenv:Envelope>')

        headerFields <- c(Accept = "text/xml",
                          Accept = "multipart/*",
                          "Content-Type" = "text/xml; charset=utf-8",
                          SOAPAction = '')

        reader <- basicTextGatherer()
        header <- basicTextGatherer()

        curlPerform(url = "http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.pl",
                    httpheader = headerFields,
                    postfields = xmlRequest,
                    writefunction = reader$update,
                    verbose = FALSE)

        ## Check the server is not down by insepcting the XML response for internal server error message.
        if(grepl("Internal Server Error", reader$value()))
          stop("Web service failure: the ORNL DAAC server seems to be down, please try again later.")

        xmlResponse <- xmlRoot(xmlTreeParse(reader$value()))
        modisResponse <- xmlSApply(xmlResponse[[1]],
                         function(x) xmlSApply(x,
                              function(x) xmlSApply(x,xmlValue)))

        if(colnames(modisResponse) == "Fault"){
          if(length(modisResponse["faultstring.text", ][[1]]) == 0)
            stop("Downloading from the web service is currently not working. Please try again later.")

          stop(modisResponse["faultstring.text", ])
        } else {
          return(as.vector(modisResponse))
        }
      },
      ##
      getBands = function(product)
      {
        ## Send XML request to MODIS SOAP web service and retrieve the XML response bands list for product.
        xmlRequest <- paste0('
          <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:mod="http://daac.ornl.gov/MODIS_webservice">
                <soapenv:Header/>
                <soapenv:Body>
                <mod:getbands soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
                <Product xsi:type="xsd:string">', product, '</Product>
                </mod:getbands>
                </soapenv:Body>
                </soapenv:Envelope>')

        headerFields <- c(Accept = "text/xml",
                          Accept = "multipart/*",
                          "Content-Type" = "text/xml; charset=utf-8",
                          SOAPAction = '')

        reader <- basicTextGatherer()
        header <- basicTextGatherer()

        curlPerform(url = "http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.pl",
                    httpheader = headerFields,
                    postfields = xmlRequest,
                    writefunction = reader$update,
                    verbose = FALSE)

        ## Check the server is not down by insepcting the XML response for internal server error message.
        if(grepl("Internal Server Error", reader$value()))
          stop("Web service failure: the ORNL DAAC server seems to be down, please try again later.")

        xmlResponse <- xmlRoot(xmlTreeParse(reader$value()))
        modisResponse <- xmlSApply(xmlResponse[[1]],
                         function(x) xmlSApply(x,
                              function(x) xmlSApply(x,xmlValue)))

        if(colnames(modisResponse) == "Fault"){
          if(length(modisResponse["faultstring.text", ][[1]]) == 0)
            stop("Downloading from the web service is currently not working. Please try again later.")

          stop(modisResponse["faultstring.text", ])
        } else {
          return(as.vector(modisResponse))
        }
      },
      ##
      getDates = function(lat, long, product)
      {
        ## Send XML request to MODIS SOAP web service and retrieve the XML response dates list for lat, long, product.
        xmlRequest <- paste0('
          <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:mod="http://daac.ornl.gov/MODIS_webservice">
                <soapenv:Header/>
                <soapenv:Body>
                <mod:getdates soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
                <Latitude xsi:type="xsd:float">', lat, '</Latitude>
                <Longitude xsi:type="xsd:float">', long, '</Longitude>
                <Product xsi:type="xsd:string">', product, '</Product>
                </mod:getdates>
                </soapenv:Body>
                </soapenv:Envelope>')

        headerFields <- c(Accept = "text/xml",
                          Accept = "multipart/*",
                          "Content-Type" = "text/xml; charset=utf-8",
                          SOAPAction = '')

        reader <- basicTextGatherer()
        header <- basicTextGatherer()

        curlPerform(url = "http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.pl",
                    httpheader = headerFields,
                    postfields = xmlRequest,
                    writefunction = reader$update,
                    verbose = FALSE)

        ## Check the server is not down by insepcting the XML response for internal server error message.
        if(grepl("Internal Server Error", reader$value()))
          stop("Web service failure: the ORNL DAAC server seems to be down, please try again later.")

        xmlResponse <- xmlRoot(xmlTreeParse(reader$value()))
        modisResponse <- xmlSApply(xmlResponse[[1]],
                         function(x) xmlSApply(x,
                              function(x) xmlSApply(x,
                                   function(x) xmlSApply(x,xmlValue))))

        if(colnames(modisResponse) == "Fault"){
          if(length(modisResponse["faultstring.text", ][[1]]) == 0)
            stop("Downloading from the web service is currently not working. Please try again later.")

          stop(modisResponse["faultstring.text", ])
        } else {
          return(as.vector(modisResponse))
        }
      },
      ##
      writePrjFile = function(filePath)
      {
        ## Write a PRJ file detailing the MODIS projection system
        ## So the downloaded data can be easily converted to ASCII grid files.
        prj <- paste0('PROJCS["Sinusoidal",GEOGCS["GCS_Undefined",DATUM["Undefined",',
                      'SPHEROID["User_Defined_Spheroid",6371007.181,0.0]],PRIMEM["Greenwich",0.0],',
                      'UNIT["Degree",0.0174532925199433]],PROJECTION["Sinusoidal"],',
                      'PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],',
                      'PARAMETER["Central_Meridian",0.0],UNIT["Meter",1.0]]')
        write(prj, file = filePath)
      },
      ##
      findID = function(id, data)
      {
        ## Match rows within two data.frames using subset IDs.
        tryCatch(stopifnot(all(names(id) %in% names(data))),
                 error = function(e) stop(simpleError("All names within id must match rows in data.")))

        matchSet <- data[ ,match(names(id), names(data))]
        rowMatches <- apply(matchSet, 1, match, id)

        ifelse(length(which(!is.na(apply(rowMatches, 2, sum)))) == 0,
               return(cat("No matches found.\n")),
               return(which(!is.na(apply(rowMatches, 2, sum)))))
      }
    )
)