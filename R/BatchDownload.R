BatchDownload <-
function(lat.long, start.date, end.date, MODIS.start, MODIS.end, Bands, Products, Size, StartDate, Transect, SaveDir)
{
    # DEFINE
    NCOL_SERVER_RES <- 10

    # Split band names into sets for different products.
    which.bands <- lapply(Products, function(x) which(Bands %in% GetBands(x)))

    # Loop set up to make request and write a subset file for each location.
    for(i in 1:nrow(lat.long))
    {
      # Retrieve the list of date codes to be requested and organise them in batches of time series's of length 10.
      dates <- lapply(Products, function(x) GetDates(lat.long$lat[i], lat.long$long[i], x))

      # Check that time-series fall within date range of MODIS data.
      if(any((start.date$year + 1900) < 2000 & (end.date$year + 1900) < 2000)){
        stop("Time-series found that falls entirely outside the range of available MODIS dates.")
      }
      if(any((start.date$year + 1900) > max(unlist(dates)) & (end.date$year + 1900) > max(unlist(dates)))){
        stop("Time-series found that falls entirely outside the range of available MODIS dates.")
      }
      if(any((end.date$year + 1900) < 2000) | any((end.date$year + 1900) > max(unlist(dates)))){
        stop("Some dates have been found that are beyond the range of MODIS observations available for download.")
      }
      if(any((start.date$year + 1900) < 2000) | any((start.date$year + 1900) > max(unlist(dates)))){
        warning("Dates found beyond range of MODIS observations. Downloading from earliest date.", immediate. = TRUE)
      }

      ##### Initialise objects that will store downloaded data.
      # Find the start date and end date specific for each subset.
      start.dates <- lapply(dates, function(x) which(x >= MODIS.start[i]))
      end.dates <- lapply(dates, function(x) which(x >= MODIS.end[i]))
      # Extract the string of time-steps by snipping end.dates off the end.
      date.res <- mapply(function(x, y) x[which(!x %in% y)], x = start.dates, y = end.dates, SIMPLIFY = FALSE)
      allProblemDates <- c() # will store any empty dates that come up, so they can be returned to the user

      subsets <- mapply(function(x, y) rep(NA, length = (length(x) * length(y))), x = which.bands, y = date.res, SIMPLIFY = FALSE)
      subsets.length <- length(unlist(subsets))
      #####

      cat("Getting subset for location ", i, " of ", nrow(lat.long), "...\n", sep = "")

      for(prod in 1:length(Products)){

        # Organise relevant MODIS dates into batches of 10. Web service getsubset function will only take 10 at a time.
        # Fill up any remaining rows in the final column to avoid data recycling.
        ifelse((length(date.res[[prod]]) %% NCOL_SERVER_RES) == 0,
               date.list <- matrix(dates[[prod]][date.res[[prod]]], nrow = NCOL_SERVER_RES),
               date.list <- matrix(c(dates[[prod]][date.res[[prod]]], rep(NA, NCOL_SERVER_RES - (length(date.res[[prod]]) %% NCOL_SERVER_RES))),
                                   nrow = NCOL_SERVER_RES))

        # Set bands for this product.
        bands <- Bands[which.bands[[prod]]]

        # Loop subset request for each band specified, storing each run into subsets object.
        for(n in 1:length(bands)){

          if(ncol(date.list) > 1){
            # Above statement stops (ncol(date.list)-1)=0 occurring in the loop (i.e. ask for the 0th column of dates).
            for(x in 1:(ncol(date.list) - 1)){

              # getsubset function return object of ModisData class, with a subset slot that only allows 10 elements
              # (i.e. 10 dates), looped until all requested dates have been retrieved.
              # Retrieve the batch of MODIS data and store in result
              result <- try(GetSubset(lat.long$lat[i], lat.long$long[i], Products[prod], bands[n],
                            date.list[1,x], date.list[NCOL_SERVER_RES,x], Size[1], Size[2]))

              if(!is.list(result)) stop("Downloading from the web service is currently not working. Please try again later.")

              if(length(strsplit(as.character(result$subset[[1]][1]), ",")[[1]]) == 5){
                stop("Downloading from the web service is currently not working. Please try again later.")
              }

              busy <- FALSE
              if(class(result) != "try-error"){
                busy <- grepl("Server is busy handling other requests", result$subset[1])
                if(busy) cat("The server is busy handling other requests...\n")
              }

              # Check data downloaded. If not, wait 30 secs and try again until successful or function times out.
              if(class(result) == "try-error" || is.na(result) || busy){
                timer <- 1
                while(timer <= 10){
                  cat("Connection to the MODIS Web Service failed: trying again in 30secs...attempt", timer, "\n")
                  Sys.sleep(30)

                  result <- try(GetSubset(lat.long$lat[i], lat.long$long[i], Products[prod], bands[n],
                                          date.list[1,x], date.list[NCOL_SERVER_RES,x], Size[1], Size[2]))

                  if(!is.list(result)) stop("Downloading from the web service is currently not working. Please try again later.")

                  if(length(strsplit(as.character(result$subset[[1]][1]), ",")[[1]]) == 5){
                    stop("Downloading from the web service is currently not working. Please try again later.")
                  }

                  timer <- timer + 1
                  ifelse(class(result) == "try-error" || is.na(result) || busy, next, break)
                }
                ifelse(class(result) == "try-error" || is.na(result) || busy,
                       cat("Connection to the MODIS Web Service failed: timed out after 10 failed attempts...stopping download.\n"),
                       break)
                stop(result)
              }

              # Store retrieved data in subsets. If more than 10 time-steps are requested, this runs until the final
              # column, which is downloaded after this loop.
              result <- with(result, paste(nrow, ncol, xll, yll, pixelsize, subset[[1]], sep = ','))

              # Check whether result contains the expected number of dates. If not, find missing dates, add NA placemark, and print warning.
              if(length(result) < sum(!is.na(date.list[ ,x]))){
                resultDates <- sapply(result, function(x) strsplit(x, ',')[[1]][8], USE.NAMES=FALSE)
                whichProblemDates <- which(!(date.list[ ,x] %in% resultDates))
                problemDates <- date.list[whichProblemDates,x]
                allProblemDates <- c(allProblemDates,problemDates)
                result <- replace(rep(NA,sum(!is.na(date.list[ ,x]))), date.list[ ,x] %in% resultDates, result)

                warning("There is no data for some requested dates:\n",
                        "Latitude = ",lat.long$lat[i],"\n",
                        "Longitude = ",lat.long$long[i],"\n",
                        "Product = ",Products[prod],"\n",
                        "Band = ",Bands[n],"\n",
                        "Dates = ",problemDates,"\n",
                        call.=FALSE, immediate.=TRUE)
              }

              subsets[[prod]][(((n - 1) * length(date.res[[prod]])) + ((x * NCOL_SERVER_RES) - (NCOL_SERVER_RES - 1))):
                        (((n - 1) * length(date.res[[prod]])) + (x * NCOL_SERVER_RES))] <- result

            } # End of loop that reiterates for multiple batches of time-steps if the time-series is > 10 time-steps long.
          }

          #####
          # This will download the last column of dates left (either final column or only column if < 10 dates).
          result <- try(GetSubset(lat.long$lat[i], lat.long$long[i], Products[prod], bands[n], date.list[1,ncol(date.list)],
                                  date.list[max(which(!is.na(date.list[ ,ncol(date.list)]))),ncol(date.list)], Size[1], Size[2]))

          if(!is.list(result)) stop("Downloading from the web service is currently not working. Please try again later.")

          if(length(strsplit(as.character(result$subset[[1]][1]), ",")[[1]]) == 5){
            stop("Downloading from the web service is currently not working. Please try again later.")
          }

          busy <- FALSE
          if(class(result) != "try-error"){
            busy <- grepl("Server is busy handling other requests", result$subset[1])
            if(busy) cat("The server is busy handling other requests...\n")
          }

          if(class(result) == "try-error" || is.na(result) || busy){
            timer <- 1
            while(timer <= 10){
              cat("Connection to the MODIS Web Service failed: trying again in 30secs...attempt", timer, "\n")
              Sys.sleep(30)

              result <- try(GetSubset(lat.long$lat[i], lat.long$long[i], Products[prod], bands[n], date.list[1,ncol(date.list)],
                                      date.list[max(which(!is.na(date.list[ ,ncol(date.list)]))),ncol(date.list)], Size[1], Size[2]))

              if(!is.list(result)) stop("Downloading from the web service is currently not working. Please try again later.")

              if(length(strsplit(as.character(result$subset[[1]][1]), ",")[[1]]) == 5){
                stop("Downloading from the web service is currently not working. Please try again later.")
              }

              timer <- timer + 1
              ifelse(class(result) == "try-error" || is.na(result) || busy, next, break)
            }

            ifelse(class(result) == "try-error" || is.na(result) || busy,
                   cat("Connection to the MODIS Web Service failed: timed out after 10 failed attempts...stopping download.\n"),
                   break)
            stop(result)
          }


          # Check downloaded subset request contains data: if it contains the following message instead, abort function.
          if(regexpr("Server is busy handling other requests in queue", result$subset[[1]][1]) != -1){
            stop("Server is busy handling other requests in queue. Please try your subset order later.")
          }

          # All MODIS data for a given product band now retrieved and stored in subsets.
          result <- with(result, paste(nrow, ncol, xll, yll, pixelsize, subset[[1]], sep = ','))

          # Check whether result contains the expected number of dates. If not, find missing dates, add NA placemark, and print warning.
          if(length(result) < sum(!is.na(date.list[ ,ncol(date.list)]))){
            resultDates <- sapply(result, function(x) strsplit(x, ',')[[1]][8], USE.NAMES=FALSE)
            whichProblemDates <- which(!(date.list[ ,ncol(date.list)] %in% resultDates))
            problemDates <- date.list[whichProblemDates,ncol(date.list)]
            allProblemDates <- c(allProblemDates,problemDates)
            result <- replace(rep(NA,sum(!is.na(date.list[ ,ncol(date.list)]))), date.list[ ,ncol(date.list)] %in% resultDates, result)

            warning("There is no data for some requested dates:\n",
                    "Latitude = ",lat.long$lat[i],"\n",
                    "Longitude = ",lat.long$long[i],"\n",
                    "Product = ",Products[prod],"\n",
                    "Band = ",Bands[n],"\n",
                    "Dates = ",problemDates,"\n",
                    call.=FALSE, immediate.=TRUE)
          }

          subsets[[prod]][(((n - 1) * length(date.res[[prod]])) + (((ncol(date.list) - 1) * NCOL_SERVER_RES) + 1)):
                    (((n - 1) * length(date.res[[prod]])) + length(date.res[[prod]]))] <- result

          # Check whether any dates in subset are empty and store their subset info for future use.
          whichBandN <- (((n-1)*length(date.res[[prod]]))+1) : (n*length(date.res[[prod]]))
          if(any(emptySubsets <- sapply(subsets[[prod]][whichBandN], function(x) grepl("character(0)",x,fixed=TRUE)))){
            problemDates <- dates[[prod]][date.res[[prod]][which(emptySubsets)]]
            allProblemDates <- c(allProblemDates,problemDates)
            warning("There is no data for some requested dates:\n",
                    "Latitude = ",lat.long$lat[i],"\n",
                    "Longitude = ",lat.long$long[i],"\n",
                    "Product = ",Products[prod],"\n",
                    "Band = ",Bands[n],"\n",
                    "Dates = ",problemDates,"\n",
                    call.=FALSE, immediate.=TRUE)
          }

        } # End of loop for each band.
      } # End of loop for each product.

      subsets <- do.call("c", subsets)

      ##### Check that there is no missing data in the download & log download status accordingly.
      if(length(subsets) != subsets.length | any(is.na(subsets))){
        lat.long$Status[i] <- paste("Some dates were missing:", paste(unique(allProblemDates),collapse="; "))
        subsets <- subsets[!is.na(subsets)]
      } else {
        lat.long$Status[i] <- "Successful download"
      }

      if("," %in% substr(subsets, nchar(subsets), nchar(subsets))){
        lat.long$Status[i] <- "Missing data in subset: try downloading again"
        cat("Missing information for time-series ", lat.long$SubsetID[i], ". See subset download file.\n", sep = "")
      } else {
        lat.long$Status[i] <- "Successful download"
      }
      #####

      # Remove any empty subsets
      if(any(problemDates <- grep("character(0)", subsets, fixed=TRUE))){
        allProblemDates <- c(allProblemDates,problemDates)
        subsets <- subsets[-problemDates]
        lat.long$Status[i] <- paste("Some dates were missing:", paste(unique(allProblemDates),collapse="; "))
      }

      # Write an ascii file with all dates for each band at a given location into the working directory.
      prods <- paste(Products, collapse = "_")

      if(!Transect) write(subsets, file = file.path(SaveDir, paste(lat.long$SubsetID[i], "___", prods, ".asc", sep = "")), sep = "")
      if(Transect){
        if(i == 1) write(subsets, file = file.path(SaveDir, paste(lat.long$SubsetID[i], "___", prods, ".asc", sep = "")), sep = "")
        if(i != 1) write(subsets, file = file.path(SaveDir, paste(lat.long$SubsetID[i], "___", prods, ".asc", sep = "")), sep = "", append = TRUE)
      }

      if(i == nrow(lat.long)) cat("Full subset download complete. Writing the subset download file...\n")
    }
    return(lat.long)
}