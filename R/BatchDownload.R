BatchDownload <- 
function(lat.long, dates, MODIS.start, MODIS.end, Bands, Product, Size, StartDate, Transect, SaveDir)
{   
    # Loop set up to make request and write a subset file for each location.
    for(i in 1:nrow(lat.long)){
      # Find the start date and end date specific for each subset.
      start.dates <- which(dates >= MODIS.start[i])
      end.dates <- which(dates >= MODIS.end[i])
      # Extract the string of time-steps in between start.dates & end.dates for the given time-series.
      date.res <- start.dates[which(!start.dates %in% end.dates)]
      
      # Organise relevant MODIS dates into batches of 10. Web service getsubset function will only take 10 at a time.
      # First, use the modulo to fill up any remaining rows in the final column to avoid data recycling.
      dateNAfiller <- rep(NA, 10 - (length(date.res) %% 10))
      date.list <- matrix(c(dates[date.res], dateNAfiller), nrow=10)
      
      # Initialise objects that will store downloaded data.
      subsets <- c()
      print(paste("Getting subset for location ", i, " of ", nrow(lat.long), "...", sep=""))
      for(n in 1:length(Bands)) {               
        # The subset request will be looped for each band specified, individually requesting all time-series in a 
        # given product band, dropping it all into subsets object, before reiterating for the next band.
        if(ncol(date.list) > 1) {               
          # Above statement stops (ncol(date.list)-1)=0 occurring in the loop (i.e. ask for the 0th column of dates).
          
          for(x in 1:(ncol(date.list) - 1)) {     
            # getsubset function return object of ModisData class, with a subset slot that only allows 10 elements 
            # (i.e. 10 dates), looped until all requested dates have been retrieved.
            # Retrieve the batch of MODIS data and store in result
            result <- try(GetSubset(lat.long[i,2], lat.long[i,3], Product, Bands[n], 
                                         date.list[1,x], date.list[10,x], Size[1], Size[2]))
            
            busy <- FALSE
            if(class(result) != "try-error"){
              busy <- grepl("Server is busy handling other requests", result$subset[1])
              if(busy) print("The server is busy handling other requests...")
            }
            
            # Check data was actually downloaded. If not, wait 30 secs and then try again. If retrieval fails 50 times
            # consecutively, then the download will time out and the function call will abort.
            if(class(result) == "try-error" || is.na(result) || busy){
              timer <- 1
              while(timer <= 30){
                print(paste("Connection to the MODIS Web Service failed: trying again in 30secs...attempt ", timer, sep=""))
                Sys.sleep(30)
                result <- try(GetSubset(lat.long[i,2], lat.long[i,3], Product, Bands[n], 
                                             date.list[1,x], date.list[10,x], Size[1], Size[2]))
                timer <- timer + 1
                ifelse(class(result) == "try-error" || is.na(result) || busy, next, break)
              }
              ifelse(class(result) == "try-error" || is.na(result) || busy,
                     print("Connection to the MODIS Web Service failed: 
                           Subset requested timed out after 10 failed attempts...stopping subset download."),
                     break)
              stop(result)
            }
            
            # Store useful (extract time-series data from Modisdata class object) retrieved data in subsets.
            # This loop will retrieve data, if more than 10 time-steps are requested, until the final column, which
            # is downloaded after this loop.
            subsets <- as.vector(c(subsets, result$subset[[1]]))
          }
          } # End of loop that reiterates for multiple batches of time-steps if the time-series is > 10 time-steps long.
        
        # This will download the last column of dates left (either the final column or the only column if less than 10
        # dates in the time-series).
        result <- try(GetSubset(lat.long[i,2], lat.long[i,3], Product, Bands[n], date.list[1,ncol(date.list)],
                                     date.list[which(date.list[ ,ncol(date.list)] >= dates[max(date.res)]),ncol(date.list)], Size[1], Size[2]))
        # Final batch of dates request, which finishes at end.date removing any recycled dates at the end of matrix 
        # (if total no. of dates is not a multiple of 10).
        
        busy <- FALSE
        if(class(result) != "try-error"){
          busy <- grepl("Server is busy handling other requests", result$subset[1])
          if(busy) print("The server is busy handling other requests...")
        }
        
        # The same download check (see there for comments) as above, for final data retrieval for a given product band.
        if(class(result) == "try-error" || is.na(result) || busy){
          timer <- 1
          while(timer <= 30){
            print(paste("Connection to the MODIS Web Service failed: trying again in 30secs...attempt ", timer, sep=""))
            Sys.sleep(30)
            # Final batch of dates, finishes at end.date.
            result <- try(GetSubset(lat.long[i,2], lat.long[i,3], Product, Bands[n], 
                                         date.list[1,ncol(date.list)], date.list[which(date.list[ ,ncol(date.list)] >= dates[max(date.res)]), 
                                                                                 ncol(date.list)], Size[1], Size[2]))
            timer <- timer + 1
            ifelse(class(result) == "try-error" || is.na(result) || busy, next, break)
          }
          ifelse(class(result) == "try-error" || is.na(result) || busy, 
                 print("Connection to the MODIS Web Service failed: 
                       Subset requested timed out after 10 failed attempts...stopping subset download."), 
                 break)
          stop(result)
        }
        
        # Check downloaded subset request contains data: if it contains the following message instead, abort function.
        if(regexpr("Server is busy handling other requests in queue. Please try your subset order later.We apologize 
                for the inconvenience", result$subset[[1]][1]) != -1) {
          stop("Server is busy handling other requests in queue. Please try your subset order later.")
        }
        
        # All MODIS data for a given product band now retrieved and stored in subsets.
        subsets <- as.vector(c(subsets, result$subset[[1]]))  
        rm(result)
      } # End of loop that iterates subset request for each product band.
      
      # Check that there is no missing data in the download & log download status accordingly.
      if(length(subsets) != (length(date.res) * length(Bands))){
        # Add missing data status for this time-series to lat.long for the download summary file & print warning message.
        ifelse(StartDate, lat.long[i,6] <- "Missing data in subset: try downloading again", 
               lat.long[i,5] <- "Missing data in subset: try downloading again")
        print(paste("There is missing information in the subset downloaded for time-series ", lat.long$ID[i], 
                    ". See subset download file.", sep=""))
      } else {
        # Add successful status for this time-series to lat.long for the download summary file.
        ifelse(StartDate, lat.long[i,6] <- "Successful download", lat.long[i,5] <- "Successful download")
      }
      
      # Write an ascii file with all dates for each band at a given location into the working directory.
      if(!Transect){ write(subsets, file=paste(SaveDir, "/", lat.long[i,1], "_", Product, ".asc", sep=""), sep="") }
      if(Transect){
        if(i == 1){ write(subsets, file=paste(SaveDir, "/", lat.long[i,1], "_", Product, ".asc", sep=""), sep="") }
        if(i != 1){ write(subsets, file=paste(SaveDir, "/", lat.long[i,1], "_", Product, ".asc", sep=""), sep="", append=TRUE) }
      }
      if(i == nrow(lat.long)) { print("Full subset download complete. Writing the subset download file...") }
    }
    return(lat.long)
}