MODISSubsets <-
function(LoadDat, LoadMethod='object' | 'ext.file', FileSep=NULL, Product, Bands, Size=c(), SaveDir=NULL, StartDate=FALSE, TimeSeriesLength=2, DateFormat='year' | 'posixt', Transect=FALSE)
{                                       
  if(LoadMethod == 'object') { dat<- data.frame(LoadDat) }                # Load data of locations; external data file, or an R object.
  if(LoadMethod == 'ext.file') { dat<- read.delim(LoadDat, sep=FileSep) }
  if(any(is.na(dat$lat) != is.na(dat$long)) == TRUE) { stop('Not equal amount of lats and longs: there must be locations with incomplete coordinate information.') }  # Test for missing lat/long data
  Start<- rep(StartDate, length(dat$lat[!is.na(dat$lat)]))
  ifelse(Start == TRUE, lat.long<- unique(cbind(lat=dat$lat[!is.na(dat$lat)],long=dat$long[!is.na(dat$lat)],end.date=dat$end.date[!is.na(dat$lat)],start.date=dat$start.date[!is.na(dat$lat)])), lat.long<- unique(cbind(lat=dat$lat[!is.na(dat$lat)],long=dat$long[!is.na(dat$lat)],end.date=dat$end.date[!is.na(dat$lat)])))               # Finds all unique time-series wanted, for each unique location.
  print(paste('Found ',nrow(lat.long),' unique time-series to download.',sep=''))
  if(nrow(lat.long) != length(unique(dat$ID))) {
    for(x in nrow(lat.long)){
      ifelse(StartDate == TRUE, ID<- paste(lat.long[x,1],lat.long[x,2],lat.long[x,3],lat.long[x,4],sep=''), ID<- paste(lat.long[x,1],lat.long[x,2],lat.long[x,3],sep=''))
    }  
    lat.long<- cbind(SubsetID=ID,lat.long,Status=rep(NA,nrow(lat.long)))        # Code has identified which subscripts in the larger data file correspond to unique locations,
    print('IDs do not contain unique time-series: using subset IDs instead.')   # making sure all are considered, so that corresponding information specific to each location such as date and ID can be easily retrieved.
  } else {
    lat.long<- cbind(SubsetID=unique(dat$ID),lat.long,Status=rep(NA,nrow(lat.long)))
  }
  Start<- rep(StartDate, nrow(lat.long))
  if(DateFormat == 'year') {
    ifelse(StartDate == FALSE, start.date<- strptime(paste(lat.long[,4]-TimeSeriesLength,'-01-01',sep=''),'%Y-%m-%d'), start.date<- strptime(paste(lat.long[,5],'-01-01',sep=''),'%Y-%m-%d'))
    end.date<- strptime(paste(lat.long[,4],'-12-31',sep=''),'%Y-%m-%d')                 # Put start and end dates in POSIXlt format.                       
    start.day<- start.date$yday
    start.day[nchar(start.day) == 2]<- paste(0, start.day[nchar(start.day) == 2], sep='')
    start.day[nchar(start.day) == 1]<- paste(0, 0, start.day[nchar(start.day) == 1], sep='')
    end.day<- end.date$yday     
    end.day[nchar(end.day) == 2]<- paste(0, end.day[nchar(end.day) == 2], sep='')
    end.day[nchar(end.day) == 1]<- paste(0, 0, end.day[nchar(end.day) == 1], sep='')
    MODIS.start<- paste('A', substr(start.date, 1, 4), start.day, sep='')               # Write dates into format compatible with MODIS date IDs.
    MODIS.end<- paste('A', substr(end.date, 1, 4), end.day, sep='')  
  } 
  if(DateFormat == 'posixt') {
    ifelse(StartDate == FALSE, start.date<- strptime(paste(lat.long[,4]-TimeSeriesLength,'-01-01',sep=''),'%Y-%m-%d'), start.date<- strptime(lat.long[,5],'%Y-%m-%d'))
    end.date<- strptime(lat.long[,4],'%Y-%m-%d')
    start.day<- start.date$yday
    start.day[nchar(start.day) == 2]<- paste(0, start.day[nchar(start.day) == 2], sep='')
    start.day[nchar(start.day) == 1]<- paste(0, 0, start.day[nchar(start.day) == 1], sep='')
    end.day<- end.date$yday
    end.day[nchar(end.day) == 2]<- paste(0, end.day[nchar(end.day) == 2], sep='')
    end.day[nchar(end.day) == 1]<- paste(0, 0, end.day[nchar(end.day) == 1], sep='')
    MODIS.start<- paste('A', substr(start.date, 1, 4), start.day, sep='')               # Write dates into format compatible with MODIS date IDs.
    MODIS.end<- paste('A', substr(end.date, 1, 4), end.day, sep='')
  }   
  ##########
  # Get the MODIS Web Service Description Language and set up SOAP-Client interface.
  ornlMODIS = processWSDL('http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.wsdl')
  ornlMODISFuncs = genSOAPClientInterface(operations=ornlMODIS@operations[[1]], def=ornlMODIS)
  ########## Retrieve the list of date codes to be requested and organise them in batches of time series' of length 10.
  dates = ornlMODISFuncs@functions$getdates(lat.long[1,2], lat.long[1,3], Product)
  ##########  
  # Use the getsubset function as described (http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html) to retrieve data subsets for each location
  # of a set of product bands, at a defined surrounding area, saving the data for each location into separate ascii files in /pixels dir in the working directory.
  for(i in 1:nrow(lat.long)) {                          # Loop set up to make request and write a subset file for each location.
    start.dates<- which(dates >= MODIS.start[i])        # Finds the start date and end date specific for each subset.
    end.dates<- which(dates >= MODIS.end[i])
    date.res<- start.dates[which(start.dates %in% end.dates == FALSE)]
    options(warn=-1); date.list<- matrix(dates[date.res], nrow=10); options(warn=0)               # Organises relevant MODIS dates into batches of 10.
    result<- list(NA)
    subsets<- c()
    print(paste('Getting subset for location ',i,' of ',nrow(lat.long),'...', sep=''))
    for(n in 1:length(Bands)) {               # Loop for each band specified, to be requested individually and then dropped into subsets.
      if(ncol(date.list) > 1) {               # Stops (ncol(date.list)-1) = 0 in the for loop.
        for(x in 1:(ncol(date.list)-1)) {     # getsubset function return object of ModisData class, with a subset slot that only allows 10 elements (i.e. 10 dates), looped until all requested dates have been retrieved.
           result[[n]] = try(ornlMODISFuncs@functions$getsubset(lat.long[i,2], lat.long[i,3], Product, Bands[n], date.list[1,x], date.list[10,x], Size[1], Size[2]))
           if(class(result[[n]]) == 'try-error'){
              timer<- 1
              while(timer <= 50){
                  print(paste('Connection to the MODIS Web Service failed: trying again in 30secs...attempt ',timer,sep=''))
                  Sys.sleep(30)
                  result[[n]] = try(ornlMODISFuncs@functions$getsubset(lat.long[i,2], lat.long[i,3], Product, Bands[n], date.list[1,x], date.list[10,x], Size[1], Size[2]))
                  timer<- timer+1
                  ifelse(class(result[[n]]) == 'try-error', next, break)
              }
              ifelse(class(result[[n]]) == 'try-error', print('Connection to the MODIS Web Service failed: Subset requested timed out after 10 failed attempts...stopping subset download.'), break)
              stop(result[[n]])
           }
           subsets<- as.vector(c(subsets, result[[n]]@subset))   # Stores all useful retrieved data.
        }
      }                                                           
      result[[n]] = try(ornlMODISFuncs@functions$getsubset(lat.long[i,2], lat.long[i,3], Product, Bands[n], date.list[1,ncol(date.list)],    # Final batch of dates, finishes at end.date
                date.list[which(date.list[,ncol(date.list)] >= dates[max(date.res)]), ncol(date.list)], Size[1], Size[2]))
      if(class(result[[n]]) == 'try-error'){
          timer<- 1
          while(timer <= 50){
              print(paste('Connection to the MODIS Web Service failed: trying again in 30secs...attempt ',timer,sep=''))
              Sys.sleep(30)
              result[[n]] = try(ornlMODISFuncs@functions$getsubset(lat.long[i,2], lat.long[i,3], Product, Bands[n], date.list[1,ncol(date.list)],    # Final batch of dates, finishes at end.date
                    date.list[which(date.list[,ncol(date.list)] >= dates[max(date.res)]), ncol(date.list)], Size[1], Size[2]))
              timer<- timer+1
              ifelse(class(result[[n]]) == 'try-error', next, break)
          }
          ifelse(class(result[[n]]) == 'try-error', print('Connection to the MODIS Web Service failed: Subset requested timed out after 10 failed attempts...stopping subset download.'), break)
          stop(result[[n]])
      }          
      if(regexpr('Server is busy handling other requests in queue. Please try your subset order later.We apologize for the inconvenience', result[[n]]@subset[1]) != -1) {
          stop('Server is busy handling other requests in queue. Please try your subset order later.')
      }          
      subsets<- as.vector(c(subsets, result[[n]]@subset))                                             # Stores all useful retrieved data.
    }
    if(length(subsets) != (length(date.res)*length(Bands))){
        ifelse(StartDate == TRUE, lat.long[i,6]<- 'Missing data in subset: try downloading again', lat.long[i,5]<- 'Missing data in subset: try downloading again')
        print(paste('There is missing information in the subset downloaded for time-series ',lat.long$ID[i],'. See subset download file.',sep=''))
    } else {
        ifelse(StartDate == TRUE, lat.long[i,6]<- 'Successful download', lat.long[i,5]<- 'Successful download') 
    }    
    if(Transect == FALSE){ write(subsets, file=paste(SaveDir,lat.long[i,1],'_',Product,'.asc', sep=''), sep='') }             # Writes an ascii file with all dates for each band at a given location into the working directory.
    if(Transect == TRUE){
      if(i == 1){ write(subsets, file=paste(SaveDir,lat.long[i,1],'_',Product,'.asc', sep=''), sep='') }
      if(i != 1){ write(subsets, file=paste(SaveDir,lat.long[i,1],'_',Product,'.asc', sep=''), sep='', append=TRUE) }
    }  
    if(i == nrow(lat.long)) { print('Full subset download complete. Writing the subset download file...') }
  } 
  if(Transect == FALSE){ write.table(lat.long, file=paste(SaveDir,'Subset Download ',Sys.Date(),'.csv',sep=''), col.names=TRUE, row.names=FALSE, sep=',') }             # Writes an ascii file with all dates for each band at a given location into the working directory.
  if(Transect == TRUE){
    ifelse(SaveDir == NULL, DirList<- list.files(), DirList<- list.files(path=SaveDir))
    if(any(DirList == paste(SaveDir,'Subset Download ',Sys.Date(),'.csv',sep='')) == FALSE){ write.table(lat.long, file=paste(SaveDir,'Subset Download ',Sys.Date(),'.csv',sep=''), col.names=TRUE, row.names=FALSE, sep=',') }
    if(any(DirList == paste(SaveDir,'Subset Download ',Sys.Date(),'.csv',sep=''))){ write.table(lat.long, file=paste(SaveDir,'Subset Download ',Sys.Date(),'.csv',sep=''), col.names=FALSE, row.names=FALSE, sep=',',append=TRUE) }
  }   
  if(Transect == FALSE){ print('Done! Check the subset download file for correct subset information and any download messages.') }                                                                                    
}
