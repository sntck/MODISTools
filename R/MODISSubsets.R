MODISSubsets <-
function(LoadDat, FileSep = NULL, Product, Bands, Size, SaveDir = ".", StartDate = FALSE, TimeSeriesLength = 0, Transect = FALSE)
{
    # Load data of locations; external data file, or an R object.
    if(!is.object(LoadDat) & !is.character(LoadDat)) stop("LoadDat must be an object in R or a file path character string.")
    if(is.object(LoadDat)) dat <- data.frame(LoadDat) 
    if(is.character(LoadDat)){
      if(!file.exists(LoadDat)) stop("Character string input for LoadDat does not resemble an existing file path.")
      if(is.null(FileSep)) stop("To load a file as input, you must also specify its delimiter (FileSep).")
      dat <- read.delim(LoadDat, sep = FileSep) 
    }
    
    #####
    # Check lat and long data frame columns are named "lat" and "long" as necessary.
    if(!any(names(dat) == "lat") & !any(names(dat) == "long")){
      stop("Could not find columns for latitude and longitude in your data set. Must be named 'lat' and 'long'.")
    }   
    # Check lats and longs are valid.
    if(abs(dat$lat) > 90 || abs(dat$long) > 180) stop("Detected some lats or longs beyond the range of valid coordinates.")
      
    # Check for missing lat/long data
    if(any(is.na(dat$lat) != is.na(dat$long))) stop("There are locations with incomplete coordinates.")
    
    # Check that the input data set contains dates, named end.date.
    if(!any(names(dat) == "end.date")) stop("Dates for time series must be included and named 'end.date'.")
    
    # Now that incomplete coordinates have been checked for, check also that each coordinate has date information.
    if(any(is.na(dat$lat) != is.na(dat$end.date))) stop("Not all coordinates have a corresponding date.")
    
    # Check SaveDir matches an existing directory.
    if(!file.exists(SaveDir)) stop("Input for SaveDir does not resemble an existing file path.")
    
    # Check StartDate is logial.
    if(!is.logical(StartDate)) stop("StartDate must be logical.")
    
    # Set of stop-if-nots to run if StartDate == TRUE.
    if(StartDate){
      # Check that the input data set contains start dates, named start.date.
      if(!any(names(dat) == "start.date")) stop("StartDate=TRUE, but 'start.date' not found in the data set.")
      # Check that each coordinate has start date information.
      if(any(is.na(dat$lat) != is.na(dat$start.date))) stop("Not all coordinates have a corresponding start date.")
    }
    
    if(!StartDate){
      # Check TimeSeriesLength is correctly inputted.
      if(!is.numeric(TimeSeriesLength)) stop("TimeSeriesLength should be numeric class.")
      
      if(length(TimeSeriesLength) != 1) stop("TimeSeriesLength must be one numeric element.")
      
      if(abs(TimeSeriesLength[1] - round(TimeSeriesLength[1])) > .Machine$double.eps^0.5){
        stop("TimeSeriesLength must be a positive integer.")
      }
      if(TimeSeriesLength <= 0) stop("TimeSeriesLength must be a positive integer.")
    }
    #####
    
    # Find all unique time-series wanted, for each unique location.
    Start <- rep(StartDate, length(dat$lat[!is.na(dat$lat)]))
    ifelse(Start,
           lat.long <- unique(cbind(lat = dat$lat[!is.na(dat$lat)], long = dat$long[!is.na(dat$lat)],
                                    end.date = dat$end.date[!is.na(dat$lat)], start.date = dat$start.date[!is.na(dat$lat)])), 
           lat.long <- unique(cbind(lat = dat$lat[!is.na(dat$lat)], long = dat$long[!is.na(dat$lat)], 
                                    end.date = dat$end.date[!is.na(dat$lat)])))
    cat("Found", nrow(lat.long), "unique time-series to download.")
    
    ##### Year or posixt date format?
    Year <- FALSE
    POSIXt <- FALSE
    
    char.compatible <- as.character(lat.long[ ,3])
    if(!is.character(char.compatible) | all(is.na(char.compatible)) & any(nchar(char.compatible) != 4)) POSIXt <- TRUE
    
    posix.compatible <- try(as.POSIXlt(lat.long[ ,3]), silent = TRUE)
    if(class(posix.compatible) == "try-error") Year <- TRUE
    
    if(!Year & !POSIXt) stop("Date information in LoadDat is not recognised as years or as POSIXt format.")
    if(Year & POSIXt) stop("Date information in LoadDat is recognised as both year and POSIXt formats.")
    
    # Take date information for each time-series, in 'year' or 'posixt', and turn them into MODIS date codes (Julian).
    if(Year){
      if(StartDate){
        start.date <- strptime(paste(lat.long[ ,4], "-01-01", sep = ""), "%Y-%m-%d")
        char.compatible <- as.character(lat.long[ ,4])
        if(!is.character(char.compatible) | all(is.na(char.compatible))) stop("Year date format: start.date not all compatible.")
        if(any(nchar(lat.long[ ,4]) != 4)) stop("start.date is not matching year format - dates should have 4 numeric characters.")
      }
      if(!StartDate) start.date <- strptime(paste(lat.long[ ,3] - TimeSeriesLength, "-01-01", sep = ""), "%Y-%m-%d")
      
      # Put start and end dates in POSIXlt format.
      end.date <- strptime(paste(lat.long[ ,3], "-12-31", sep = ""), "%Y-%m-%d")
      start.day <- start.date$yday
      start.day[nchar(start.day) == 2] <- paste(0, start.day[nchar(start.day) == 2], sep = "")
      start.day[nchar(start.day) == 1] <- paste(0, 0, start.day[nchar(start.day) == 1], sep = "")
      end.day <- end.date$yday
      end.day[nchar(end.day) == 2] <- paste(0, end.day[nchar(end.day) == 2], sep = "")
      end.day[nchar(end.day) == 1] <- paste(0, 0, end.day[nchar(end.day) == 1], sep = "")
      
      # Write dates into format compatible with MODIS date IDs (Julian format: YYYYDDD).
      MODIS.start <- paste("A", substr(start.date, 1, 4), start.day, sep = "")
      MODIS.end <- paste("A", substr(end.date, 1, 4), end.day, sep = "")
    }

    if(POSIXt){
      end.date <- strptime(lat.long[ ,3], "%Y-%m-%d")     
      
      if(StartDate){
        posix.compatible <- try(as.POSIXlt(lat.long[ ,4]), silent = TRUE)
        if(class(posix.compatible) == "try-error") stop("POSIX date format detected, but start.date may not all be POSIXt format.")
        start.date <- strptime(lat.long[ ,4], "%Y-%m-%d")
      }
      if(!StartDate) start.date <- strptime(paste((end.date$year + 1900) - TimeSeriesLength, "-01-01", sep = ""), "%Y-%m-%d")
      
      start.day <- start.date$yday
      start.day[nchar(start.day) == 2] <- paste(0, start.day[nchar(start.day) == 2], sep = "")
      start.day[nchar(start.day) == 1] <- paste(0, 0, start.day[nchar(start.day) == 1], sep = "")
      end.day <- end.date$yday
      end.day[nchar(end.day) == 2] <- paste(0, end.day[nchar(end.day) == 2], sep = "")
      end.day[nchar(end.day) == 1] <- paste(0, 0, end.day[nchar(end.day) == 1], sep = "")
      
      MODIS.start <- paste("A", substr(start.date, 1, 4), start.day, sep = "")
      MODIS.end <- paste("A", substr(end.date, 1, 4), end.day, sep = "")
    }
    #####
    
    # Create IDs for each time series.
    fmt <- '%.5f'
    ID <- paste("Lat", sprintf(fmt, lat.long[ ,1]), "Lon", sprintf(fmt, lat.long[ ,2]), "Start", start.date, "End", end.date, sep = "")
    lat.long <- data.frame(SubsetID = ID, lat.long, Status = rep(NA, nrow(lat.long)))
    
    #####
    # If the Product input does not match any product codes in the list output from GetProducts(), stop with error.
    if(!all(Product %in% GetProducts())) stop("Not every Product input matches available products (?GetProducts).")
    
    # If the Bands input does not match with the Product input, stop with error.
    avail.bands <- unlist(lapply(Product, function(x) GetBands(x)))
    band.test <- any(lapply(Bands, function(x) any(x %in% avail.bands)) == FALSE)
    if(band.test) stop("At least one Bands input does not match the product names entered (?GetBands).")
    
    # If Size is not two dimensions or not integers, stop with error.
    if(!is.numeric(Size)) stop("Size should be numeric class. Two integers.")
    if(length(Size) != 2) stop("Size input must be a vector of integers, with two elements.")
    if(abs(Size[1] - round(Size[1])) > .Machine$double.eps^0.5 |  abs(Size[2] - round(Size[2])) > .Machine$double.eps^0.5){
      stop("Size input must be integers.")
    }
    
    # Retrieve the list of date codes to be requested and organise them in batches of time series's of length 10.
    dates <- lapply(Product, function(x) GetDates(lat.long[1,2], lat.long[1,3], x))
    
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
    #####

    ##### Retrieve data subsets for each time-series of a set of product bands, saving data for each time series into ASCII files.
    lat.long <- BatchDownload(lat.long = lat.long, dates = dates, MODIS.start = MODIS.start, MODIS.end = MODIS.end, Bands = Bands, 
                              Product = Product, Size = Size, StartDate = StartDate, Transect = Transect, SaveDir = SaveDir)
        
    # Run a second round of downloads for any time-series that incompletely downloaded, and overwrite originals.
    success.check <- lat.long[ ,ncol(lat.long)] != "Successful download"
    if(any(success.check)){
      cat("Some subsets that were downloaded were incomplete. Retrying download again for these time-series...")
      
      lat.long[success.check, ] <- BatchDownload(lat.long = lat.long[success.check, ], dates = dates, MODIS.start = MODIS.start,
                                                 MODIS.end = MODIS.end, Bands = Bands, Product = Product, Size = Size,
                                                 StartDate = StartDate, Transect = Transect, SaveDir = SaveDir)
      
      success.check <- lat.long[ ,ncol(lat.long)] != "Successful download"
      if(any(success.check)) cat("Incomplete downloads were re-tried but incomplete downloads remain. See subset download file.")
    }
    #####
    
    ##### Write a summary file with IDs and unique time-series information .
    if(!Transect){
      write.table(lat.long, file = paste(SaveDir, "/", "Subset Download ", Sys.Date(), ".csv", sep = ""), 
                  col.names = TRUE, row.names = FALSE, sep = ",")
    }
    if(Transect){
      DirList <- list.files(path = SaveDir)
      w.transect <- regexpr("Point", dat$ID[1])
      transect.id <- substr(dat$ID[1], 1, w.transect - 1)
      
      if(!any(DirList == paste(SaveDir, "/", transect.id, "_Subset Download ", Sys.Date(), ".csv", sep = ""))){ 
        write.table(lat.long, file = paste(SaveDir, "/", transect.id, "_Subset Download ", Sys.Date(), ".csv", sep = ""), 
                    col.names = TRUE, row.names = FALSE, sep = ",")
      } else {
        write.table(lat.long, file = paste(SaveDir, "/", transect.id, "_Subset Download ", Sys.Date(), ".csv", sep = ""), 
                    col.names = FALSE, row.names = FALSE, sep = ",", append = TRUE)
      }
    }
    #####
    
    # Print message to confirm downloads are complete and to remind the user to check summary file for any missing data.
    if(!Transect) cat("Done! Check the subset download file for correct subset information and download messages.")
}
