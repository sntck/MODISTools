MODISSubsets <-
function(LoadDat, FileSep=NULL, Product, Bands, Size, SaveDir=".", StartDate=FALSE, TimeSeriesLength=2, Transect=FALSE)
{
    if(!is.object(LoadDat) & !is.character(LoadDat)){
      stop("Data is incorrectly specified. Must either be the name of an object in R, or a file path character string.")
    }
    # Load data of locations; external data file, or an R object.
    if(is.object(LoadDat)) { dat <- data.frame(LoadDat) }
    if(is.character(LoadDat)) {
      if(!file.exists(LoadDat)){
        stop("Character string input for LoadDat argument does not resemble an existing file path.")
      }
      if(is.null(FileSep)){
        stop("Data is a file path. If you want to load a file as input, you must also specify its delimiter (FileSep).")
      }
      dat <- read.delim(LoadDat, sep=FileSep) 
    }
    
    # Check lat and long data frame columns are named "lat" and "long" as necessary.
    if(!any(names(dat) == "lat") & !any(names(dat) == "long")){
      stop("Could not find columns for latitude and longitude in your data set. Must be named 'lat' and 'long'.
           See help for data set requirements.")
    }   
    # Check lats and longs are valid.
    if(abs(dat$lat) > 90 || abs(dat$long) > 180){
      stop("Detected some lats or longs beyond the range of valid coordinates.")
    }  
    # Check for missing lat/long data
    if(any(is.na(dat$lat) != is.na(dat$long))) { 
      stop("Not equal amount of lats and longs: there must be locations with incomplete coordinate information.") 
    }
    
    # Check that the input data set contains dates, named end.date.
    if(!any(names(dat) == "end.date")){
      stop("Could not find date information. Dates for time-series must be included and named 'end.date'. 
           See help for data set requirements.")
    }
    # Now that incomplete coordinates have been checked for, check also that each coordinate has date information.
    if(any(is.na(dat$lat) != is.na(dat$end.date))){
      stop("Not all coordinates have a corresponding date. All time-series must have location and time information.")
    }
    
    # Check SaveDir matches an existing directory.
    if(!file.exists(SaveDir)){
      stop("Character string input for SaveDir argument does not resemble an existing file path.")
    }
    
    # Check StartDate is logial.
    if(!is.logical(StartDate)){
      stop("StartDate confirms whether start dates for time-series are included in the dataset. Must be logical.")
    }
    # Set of stop-if-nots to run if StartDate=TRUE.
    if(StartDate){
      # Check that the input data set contains start dates, named start.date.
      if(!any(names(dat) == "start.date")){
        stop("StartDate=TRUE, but no start date has been found in the data set. Start dates must be named 'start.date'.
             See help for data set requirements.")
      }
      # Check that each coordinate has start date information.
      if(any(is.na(dat$lat) != is.na(dat$start.date))){
        stop("Not all coordinates have a corresponding start date. If start.date is incomplete, consider StartDate=FALSE.")
      }
    }
    if(!StartDate){
      # Check TimeSeriesLength is correctly inputted.
      if(!is.numeric(TimeSeriesLength)){
        stop("TimeSeriesLength should be numeric class.")
      }
      if(length(TimeSeriesLength) != 1){
        stop("TimeSeriesLength must be one numeric element.")
      }
      if(abs(TimeSeriesLength[1] - round(TimeSeriesLength[1])) > .Machine$double.eps^0.5){
        stop("TimeSeriesLength must be a positive integer.")
      }
      if(TimeSeriesLength <= 0){
        stop("TimeSeriesLength must be a positive integer.")
      }
    }
    ##########
    # Find all unique time-series wanted, for each unique location.
    Start <- rep(StartDate, length(dat$lat[!is.na(dat$lat)]))
    ifelse(Start, lat.long <- unique(cbind(lat=dat$lat[!is.na(dat$lat)], long=dat$long[!is.na(dat$lat)],
                                                   end.date=dat$end.date[!is.na(dat$lat)], start.date=dat$start.date[!is.na(dat$lat)])), 
           lat.long <- unique(cbind(lat=dat$lat[!is.na(dat$lat)], long=dat$long[!is.na(dat$lat)], 
                                    end.date=dat$end.date[!is.na(dat$lat)])))
    print(paste("Found ", nrow(lat.long), " unique time-series to download.", sep=""))
    
    # Year or posixt date format?
    Year <- FALSE
    POSIXt <- FALSE
    char.compatible <- as.character(lat.long[ ,3])
    if(!is.character(char.compatible) | all(is.na(char.compatible)) & any(nchar(char.compatible) != 4)){
      POSIXt <- TRUE
    }
    posix.compatible <- try(as.POSIXlt(lat.long[ ,3]), silent=TRUE)
    if(class(posix.compatible) == "try-error"){
      Year <- TRUE
    }
    if(!Year & !POSIXt){
      stop("Date information in LoadDat is not recognised as years or as POSIXt format. Check dates conform to one of these.")
    }
    if(Year & POSIXt){
      stop("Date information in LoadDat is recognised as both year and POSIXt formats. Check dates conform to one of these.")
    }
    
    # Take date information for each time-series, in either 'year' or 'posixt', and turn them into MODIS 
    # date codes (Julian format).
    if(Year) {  
      if(!StartDate){
        start.date <- strptime(paste(lat.long[ ,3] - TimeSeriesLength, "-01-01", sep=""), "%Y-%m-%d")
      } else if(StartDate){
        start.date <- strptime(paste(lat.long[ ,4], "-01-01", sep=""), "%Y-%m-%d")
        char.compatible <- as.character(lat.long[ ,4])
        if(!is.character(char.compatible) | all(is.na(char.compatible))){
          stop("Year date format detected, start.date are not all compatible.")
        }
        if(any(nchar(lat.long[ ,4]) != 4)){
          stop("start.date is not matching year format - dates should have 4 numeric characters.")
        }
      }
      # Put start and end dates in POSIXlt format.
      end.date <- strptime(paste(lat.long[ ,3], "-12-31", sep=""), "%Y-%m-%d")
      start.day <- start.date$yday
      start.day[nchar(start.day) == 2] <- paste(0, start.day[nchar(start.day) == 2], sep="")
      start.day[nchar(start.day) == 1] <- paste(0, 0, start.day[nchar(start.day) == 1], sep="")
      end.day <- end.date$yday
      end.day[nchar(end.day) == 2] <- paste(0, end.day[nchar(end.day) == 2], sep="")
      end.day[nchar(end.day) == 1] <- paste(0, 0, end.day[nchar(end.day) == 1], sep="")
      # Write dates into format compatible with MODIS date IDs (Julian format: YYYYDDD).
      MODIS.start <- paste("A", substr(start.date, 1, 4), start.day, sep="")
      MODIS.end <- paste("A", substr(end.date, 1, 4), end.day, sep="")
    }
    
    if(POSIXt) {
      end.date <- strptime(lat.long[ ,3], "%Y-%m-%d")     
      if(!StartDate){
        start.date <- strptime(paste((end.date$year + 1900) - TimeSeriesLength, "-01-01", sep=""), "%Y-%m-%d")
      } else if(StartDate){
        posix.compatible <- try(as.POSIXlt(lat.long[ ,4]), silent=TRUE)
        if(class(posix.compatible) == "try-error"){
          stop("POSIX date format detected, but start.date are not all unambiguously in standard POSIXt format.
             See ?POSIXt for help.")
        }
        start.date <- strptime(lat.long[ ,4], "%Y-%m-%d")
      }     
      start.day <- start.date$yday
      start.day[nchar(start.day) == 2] <- paste(0, start.day[nchar(start.day) == 2], sep="")
      start.day[nchar(start.day) == 1] <- paste(0, 0, start.day[nchar(start.day) == 1], sep="")
      end.day <- end.date$yday
      end.day[nchar(end.day) == 2] <- paste(0, end.day[nchar(end.day) == 2], sep="")
      end.day[nchar(end.day) == 1] <- paste(0, 0, end.day[nchar(end.day) == 1], sep="")
      MODIS.start <- paste("A", substr(start.date, 1, 4), start.day, sep="")
      MODIS.end <- paste("A", substr(end.date, 1, 4), end.day, sep="")
    }
    # Unique time-series are now extracted from input dataset and organised into a format useful for download.
    #####
    # Code has now identified which subscripts in the larger data file correspond to unique locations,
    # making sure all are considered, so that corresponding information specific to each location 
    # such as date and ID can be easily retrieved.
    fmt <- '%.5f'
    ID <- paste("Lat", sprintf(fmt, lat.long[ ,1]), "Lon", sprintf(fmt, lat.long[ ,2]), "Start", start.date, "End", end.date, sep="")
    lat.long <- data.frame(SubsetID=ID, lat.long, Status=rep(NA, nrow(lat.long)))
    
    ##### Some sanity checks.
    # If the Product input does not match any product codes in the list output from GetProducts(), stop with error.
    if(!any(Product == GetProducts())){
      stop("The product name entered does not match any available products. 
           See GetProducts() for available products.")
    }
    # If the Bands input does not match with the Product input, stop with error.
    band.test <- lapply(Bands, function(x) !any(x == GetBands(Product)))
    if(any(band.test == TRUE)){ 
      stop("At least one band name entered does not match the product name entered. 
           See GetBands() for band names available within each product.")
    }
    # If Size is not two dimensions or are not integers (greater than expected after rounding, with tolerance around
    # computing precision), stop with error.
    if(!is.numeric(Size)){
      stop("Size should be numeric class. Two integers.")
    }
    if(length(Size) != 2){
      stop("Size input must be a vector of integers, with two elements.")
    }
    if(abs(Size[1] - round(Size[1])) > .Machine$double.eps^0.5 | 
                  abs(Size[2] - round(Size[2])) > .Machine$double.eps^0.5){
      stop("Size input must be integers.")
    }
    #####
    
    # Retrieve the list of date codes to be requested and organise them in batches of time series's of length 10.
    dates <- GetDates(lat.long[1,2], lat.long[1,3], Product)
    
    # Run some checks that time-series fall within date range of MODIS data.
    if(any((end.date$year + 1900) < 2000) | any((end.date$year + 1900) > dates[length(dates)])){
      warning("Some dates have been found that are beyond the range of MODIS observations available for download.
              Downloading available dates only.")
    }
    
    if(any((start.date$year + 1900) < 2000) | any((start.date$year + 1900) > dates[length(dates)])){
      warning("Some dates have been found that move beyond the range of MODIS observations available for download. 
              Downloading available dates only.")
    }
    if(any((start.date$year + 1900) < 2000 & (end.date$year + 1900) < 2000)){
      lat.long <- lat.long[-which((start.date$year + 1900) < 2000 & (end.date$year + 1900) < 2000), ]
      warning("Time-series found that falls entirely outside the range of available MODIS dates. Removing these time-series.")
    }
    if(any((start.date$year + 1900) > dates[length(dates)] & (end.date$year + 1900) > dates[length(dates)])){
      lat.long <- lat.long[-which((start.date$year + 1900) > dates[length(dates)] & (end.date$year + 1900) > dates[length(dates)]), ]
      warning("Time-series found that falls entirely outside the range of available MODIS dates. Removing these time-series.")
    }

    # Use the getsubset function as described (http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html) to 
    # retrieve data subsets for each time-series of a set of product bands, at a defined surrounding area, saving 
    # the data for each time-series into separate ascii files in /pixels dir in the working directory.
    lat.long <- BatchDownload(lat.long=lat.long, dates=dates, MODIS.start=MODIS.start, MODIS.end=MODIS.end, Bands=Bands, 
                              Product=Product, Size=Size, StartDate=StartDate, Transect=Transect, SaveDir=SaveDir)
    # End of loop that retrieves data. All downloaded data now saved in ascii files for each time-series.
    
    # Run a second round of downloads for any time-series that incompletely downloaded, and overwrite originals.
    check.count <- 1
    while(check.count <= 2){
      success.check <- lat.long[ ,ncol(lat.long)] != "Successful download"
      if(any(success.check)){
        print("Some subsets that were downloaded were incomplete. Retrying download again for these time-series...")
        lat.long[which(success.check), ] <- BatchDownload(lat.long=lat.long[which(success.check), ], dates=dates,
                                                          MODIS.start=MODIS.start, MODIS.end=MODIS.end, Bands=Bands, 
                                                          Product=Product, Size=Size, StartDate=StartDate, 
                                                          Transect=Transect, SaveDir=SaveDir)
      }
      check.count <- check.count + 1
    }
    success.check <- lat.long[ ,ncol(lat.long)] != "Successful download"
    if(any(success.check)){
      print("Incomplete downloads were found, and downloading for these time-series was re-tried twice,")
      print("but some incomplete downloads remain. See subset download file and try downloading them later.")
    }
    
    # Write a summary file with IDs and unique time-series information .
    if(!Transect){ write.table(lat.long, file=paste(SaveDir, "/", "Subset Download ", Sys.Date(), ".csv", sep=""), 
          col.names=TRUE, row.names=FALSE, sep=",") }
    if(Transect){
      DirList <- list.files(path=SaveDir)
      w.transect <- regexpr("Point", dat$ID[1])
      transect.id <- substr(dat$ID[1], 1, w.transect - 1)
      if(!any(DirList == paste(SaveDir, "/", transect.id, "_Subset Download ", Sys.Date(), ".csv", sep=""))){ 
        write.table(lat.long, file=paste(SaveDir, "/", transect.id, "_Subset Download ", Sys.Date(), ".csv", sep=""), 
              col.names=TRUE, row.names=FALSE, sep=",") 
      }
      if(any(DirList == paste(SaveDir, "/", transect.id, "_Subset Download ", Sys.Date(), ".csv", sep=""))){ 
        write.table(lat.long, file=paste(SaveDir, "/", transect.id, "_Subset Download ", Sys.Date(), ".csv", sep=""), 
              col.names=FALSE, row.names=FALSE, sep=",", append=TRUE) 
      }
    }
    
    # Print message to confirm downloads are complete and to remind the user to check summary file for any missing data.
    if(!Transect){ 
      print("Done! Check the subset download file for correct subset information and any download messages.") 
    }                                                                                    
}