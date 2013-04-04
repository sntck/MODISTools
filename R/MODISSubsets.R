MODISSubsets <-
function(LoadDat, LoadMethod="object" | "ext.file", FileSep=NULL, Product, Bands, Size=c(), SaveDir="./", StartDate=FALSE, TimeSeriesLength=2, DateFormat="year" | "posixt", Transect=FALSE)
{
    # Load data of locations; external data file, or an R object.
    if(LoadMethod == "object") { dat<- data.frame(LoadDat) }
    if(LoadMethod == "ext.file") { dat<- read.delim(LoadDat, sep=FileSep) }
    
    # Test for missing lat/long data
    if(any(is.na(dat$lat) != is.na(dat$long)) == TRUE) { stop("Not equal amount of lats and longs: there must be locations with incomplete coordinate information.") }
    
    # Find all unique time-series wanted, for each unique location.
    Start<- rep(StartDate, length(dat$lat[!is.na(dat$lat)]))
    ifelse(Start == TRUE, lat.long<- unique(cbind(lat=dat$lat[!is.na(dat$lat)],long=dat$long[!is.na(dat$lat)],
          end.date=dat$end.date[!is.na(dat$lat)],start.date=dat$start.date[!is.na(dat$lat)])), 
          lat.long<- unique(cbind(lat=dat$lat[!is.na(dat$lat)],long=dat$long[!is.na(dat$lat)],
          end.date=dat$end.date[!is.na(dat$lat)])))
    print(paste("Found ",nrow(lat.long)," unique time-series to download.",sep=""))
    
    Start<- rep(StartDate, nrow(lat.long))
    if(nrow(lat.long) != length(unique(dat$ID))) {
      ifelse(Start == TRUE, ID<- paste(lat.long[,1],lat.long[,2],lat.long[,3],lat.long[,4],sep=''), 
            ID<- paste(lat.long[,1],lat.long[,2],lat.long[,3],sep=''))
      lat.long<- data.frame(SubsetID=ID,lat.long,Status=rep(NA,nrow(lat.long)))
      print("IDs do not contain unique time-series: using subset IDs instead.")           
    } else {
      lat.long<- data.frame(SubsetID=unique(dat$ID),lat.long,Status=rep(NA,nrow(lat.long)))
    }
    # Code has now identified which subscripts in the larger data file correspond to unique locations,
    # making sure all are considered, so that corresponding information specific to each location 
    # such as date and ID can be easily retrieved. If the number of unique IDs identified does not equal the number
    # of unique time-series, then IDs are created using each time-series's unique information.
    
    # Take date information for each time-series, in either 'year' or 'posixt', and turn them into MODIS 
    # date codes (Julian format).
    if(DateFormat == "year") {
        if(StartDate == FALSE){
          start.date<- strptime(paste(lat.long[,4]-TimeSeriesLength,"-01-01",sep=""), "%Y-%m-%d")
        } else if(StartDate == TRUE){
          start.date<- strptime(paste(lat.long[,5],"-01-01",sep=""),"%Y-%m-%d")
        }
        # Put start and end dates in POSIXlt format.
        end.date<- strptime(paste(lat.long[,4],"-12-31",sep=""),"%Y-%m-%d")
        start.day<- start.date$yday
        start.day[nchar(start.day) == 2]<- paste(0, start.day[nchar(start.day) == 2], sep="")
        start.day[nchar(start.day) == 1]<- paste(0, 0, start.day[nchar(start.day) == 1], sep="")
        end.day<- end.date$yday
        end.day[nchar(end.day) == 2]<- paste(0, end.day[nchar(end.day) == 2], sep="")
        end.day[nchar(end.day) == 1]<- paste(0, 0, end.day[nchar(end.day) == 1], sep="")
        # Write dates into format compatible with MODIS date IDs (Julian format: YYYYDDD).
        MODIS.start<- paste("A", substr(start.date, 1, 4), start.day, sep="")
        MODIS.end<- paste("A", substr(end.date, 1, 4), end.day, sep="")
    }
    
    if(DateFormat == "posixt") {
        if(StartDate == FALSE){
          start.date<- strptime(paste(lat.long[,4]-TimeSeriesLength,"-01-01",sep=""),"%Y-%m-%d")
        } else if(StartDate == TRUE){
          start.date<- strptime(lat.long[,5],"%Y-%m-%d")
        } 
        end.date<- strptime(lat.long[,4],"%Y-%m-%d")
        start.day<- start.date$yday
        start.day[nchar(start.day) == 2]<- paste(0, start.day[nchar(start.day) == 2], sep="")
        start.day[nchar(start.day) == 1]<- paste(0, 0, start.day[nchar(start.day) == 1], sep="")
        end.day<- end.date$yday
        end.day[nchar(end.day) == 2]<- paste(0, end.day[nchar(end.day) == 2], sep="")
        end.day[nchar(end.day) == 1]<- paste(0, 0, end.day[nchar(end.day) == 1], sep="")
        # Write dates into format compatible with MODIS date IDs (Julian format: YYYYDDD).
        MODIS.start<- paste("A", substr(start.date, 1, 4), start.day, sep="")
        MODIS.end<- paste("A", substr(end.date, 1, 4), end.day, sep="")
    }
    # Unique time-series are now extracted from input dataset and organised into a format useful for download.
    #####
    
    # Get the MODIS Web Service Description Language and set up SOAP-Client interface.
    ornlMODIS<- processWSDL("http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.wsdl")
    ornlMODISFuncs<- genSOAPClientInterface(operations=ornlMODIS@operations[[1]], def=ornlMODIS)
    # Retrieve the list of date codes to be requested and organise them in batches of time series's of length 10.
    dates<- ornlMODISFuncs@functions$getdates(lat.long[1,2], lat.long[1,3], Product)
    
    # Use the getsubset function as described (http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html) to 
    # retrieve data subsets for each time-series of a set of product bands, at a defined surrounding area, saving 
    # the data for each time-series into separate ascii files in /pixels dir in the working directory.
    for(i in 1:nrow(lat.long)) {                          
      # Loop set up to make request and write a subset file for each location.
      
      # Find the start date and end date specific for each subset.
      start.dates<- which(dates >= MODIS.start[i])
      end.dates<- which(dates >= MODIS.end[i])
      # Extract the string of time-steps in between start.dates & end.dates for the given time-series.
      date.res<- start.dates[which(start.dates %in% end.dates == FALSE)]
      
      # Organise relevant MODIS dates into batches of 10. Web service getsubset function will only take 10 at a time.
      # First, use the modulo to fill up any remaining rows in the final column to avoid data recycling.
      dateNAfiller<- rep(NA, 10 - (length(date.res) %% 10))
      date.list<- matrix(c(dates[date.res],dateNAfiller), nrow=10)
      
      # Initialise objects that will store downloaded data.
      result<- list(NA)
      subsets<- c()
      print(paste("Getting subset for location ",i," of ",nrow(lat.long),"...", sep=""))
      for(n in 1:length(Bands)) {               
        # The subset request will be looped for each band specified, individually requesting all time-series in a 
        # given product band, dropping it all into subsets object, before reiterating for the next band.
        if(ncol(date.list) > 1) {               
          # Above statement stops (ncol(date.list)-1)=0 occurring in the loop (i.e. ask for the 0th column of dates).
          
          for(x in 1:(ncol(date.list)-1)) {     
            # getsubset function return object of ModisData class, with a subset slot that only allows 10 elements 
            # (i.e. 10 dates), looped until all requested dates have been retrieved.
            # Retrieve the batch of MODIS data and store in result
            result[[n]]<- try(ornlMODISFuncs@functions$getsubset(lat.long[i,2], lat.long[i,3], Product, Bands[n], 
                  date.list[1,x], date.list[10,x], Size[1], Size[2]))
            
            # Check data was actually downloaded. If not, wait 30 secs and then try again. If retrieval fails 50 times
            # consecutively, then the download will time out and the function call will abort.
            if(class(result[[n]]) == "try-error"){
              timer<- 1
              while(timer <= 50){
                print(paste("Connection to the MODIS Web Service failed: trying again in 30secs...attempt ",timer,sep=""))
                Sys.sleep(30)
                result[[n]]<- try(ornlMODISFuncs@functions$getsubset(lat.long[i,2], lat.long[i,3], Product, Bands[n], 
                      date.list[1,x], date.list[10,x], Size[1], Size[2]))
                timer<- timer+1
                ifelse(class(result[[n]]) == "try-error", next, break)
              }
              ifelse(class(result[[n]]) == "try-error", print("Connection to the MODIS Web Service failed: 
                    Subset requested timed out after 10 failed attempts...stopping subset download."), break)
              stop(result[[n]])
            }
            
            # Store useful (extract time-series data from Modisdata class object) retrieved data in subsets.
            # This loop will retrieve data, if more than 10 time-steps are requested, until the final column, which
            # is downloaded after this loop.
            subsets<- as.vector(c(subsets, result[[n]]@subset))
          }
        } # End of loop that reiterates for multiple batches of time-steps if the time-series is > 10 time-steps long.
        
        # This will download the last column of dates left (either the final column or the only column if less than 10
        # dates in the time-series).
        result[[n]]<- try(ornlMODISFuncs@functions$getsubset(lat.long[i,2], lat.long[i,3], Product, Bands[n], date.list[1,ncol(date.list)],
              date.list[which(date.list[,ncol(date.list)] >= dates[max(date.res)]), ncol(date.list)], Size[1], Size[2]))
        # Final batch of dates request, which finishes at end.date removing any recycled dates at the end of matrix 
        # (if total no. of dates is not a multiple of 10).
        
        # The same download check (see there for comments) as above, for final data retrieval for a given product band.
        if(class(result[[n]]) == "try-error"){
          timer<- 1
          while(timer <= 50){
            print(paste("Connection to the MODIS Web Service failed: trying again in 30secs...attempt ",timer,sep=""))
            Sys.sleep(30)
            # Final batch of dates, finishes at end.date.
            result[[n]]<- try(ornlMODISFuncs@functions$getsubset(lat.long[i,2], lat.long[i,3], Product, Bands[n], 
                  date.list[1,ncol(date.list)], date.list[which(date.list[,ncol(date.list)] >= dates[max(date.res)]), 
                  ncol(date.list)], Size[1], Size[2]))
            timer<- timer+1
            ifelse(class(result[[n]]) == "try-error", next, break)
          }
          ifelse(class(result[[n]]) == "try-error", print("Connection to the MODIS Web Service failed: 
                Subset requested timed out after 10 failed attempts...stopping subset download."), break)
          stop(result[[n]])
        }
        
        # Check downloaded subset request contains data: if it contains the following message instead, abort function.
        if(regexpr("Server is busy handling other requests in queue. Please try your subset order later.We apologize 
              for the inconvenience", result[[n]]@subset[1]) != -1) {
          stop("Server is busy handling other requests in queue. Please try your subset order later.")
        }
        
        # All MODIS data for a given product band now retrieved and stored in subsets.
        subsets<- as.vector(c(subsets, result[[n]]@subset))                                             
      } # End of loop that iterates subset request for each product band.
      
      # Check that there is no missing data in the download & log download status accordingly.
      if(length(subsets) != (length(date.res)*length(Bands))){
        # Add missing data status for this time-series to lat.long for the download summary file & print warning message.
        ifelse(StartDate == TRUE, lat.long[i,6]<- "Missing data in subset: try downloading again", 
              lat.long[i,5]<- "Missing data in subset: try downloading again")
        print(paste("There is missing information in the subset downloaded for time-series ",lat.long$ID[i],
              ". See subset download file.",sep=""))
      } else {
        # Add successful status for this time-series to lat.long for the download summary file.
        ifelse(StartDate == TRUE, lat.long[i,6]<- "Successful download", lat.long[i,5]<- "Successful download")
      }
      
      # Write an ascii file with all dates for each band at a given location into the working directory.
      if(Transect == FALSE){ write(subsets, file=paste(SaveDir,lat.long[i,1],"_",Product,".asc", sep=""), sep="") }
      if(Transect == TRUE){
        if(i == 1){ write(subsets, file=paste(SaveDir,lat.long[i,1],"_",Product,".asc", sep=""), sep="") }
        if(i != 1){ write(subsets, file=paste(SaveDir,lat.long[i,1],"_",Product,".asc", sep=""), sep="", append=TRUE) }
      }
      if(i == nrow(lat.long)) { print("Full subset download complete. Writing the subset download file...") }
    } # End of loop that retrieves data. All downloaded data now saved in ascii files for each time-series.
    
    # Write a summary file with IDs and unique time-series information .
    if(Transect == FALSE){ write.table(lat.long, file=paste(SaveDir,"Subset Download ",Sys.Date(),".csv",sep=""), 
          col.names=TRUE, row.names=FALSE, sep=",") }
    if(Transect == TRUE){
      ifelse(SaveDir == "./", DirList<- list.files(), DirList<- list.files(path=SaveDir))
      w.transect<- regexpr("Point", dat$ID[1])
      transect.id<- substr(dat$ID[1], 1, w.transect-1)
      if(any(DirList == paste(SaveDir,transect.id,"_Subset Download ",Sys.Date(),".csv",sep="")) == FALSE){ 
        write.table(lat.long, file=paste(SaveDir,transect.id,"_Subset Download ",Sys.Date(),".csv",sep=""), 
              col.names=TRUE, row.names=FALSE, sep=",") 
      }
      if(any(DirList == paste(SaveDir,transect.id,"_Subset Download ",Sys.Date(),".csv",sep=""))){ 
        write.table(lat.long, file=paste(SaveDir,transect.id,"_Subset Download ",Sys.Date(),".csv",sep=""), 
              col.names=FALSE, row.names=FALSE, sep=",",append=TRUE) 
      }
    }
    
    # Print message to confirm downloads are complete and to remind the user to check summary file for any missing data.
    if(Transect == FALSE){ 
      print("Done! Check the subset download file for correct subset information and any download messages.") 
    }                                                                                    
}