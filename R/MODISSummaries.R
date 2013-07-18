MODISSummaries <-
  function(LoadDat, FileSep=NULL, Dir=".", Product, Band, ValidRange, NoDataFill, ScaleFactor, StartDate=FALSE, QualityScreen=FALSE, QualityBand=NULL, QualityThreshold=NULL, Mean=TRUE, SD=TRUE, Min=TRUE, Max=TRUE, Yield=FALSE, Interpolate=TRUE) 
  { 
    # Load input time-series data file; external data file, or an R object.
    if(is.object(LoadDat)) { details <- data.frame(LoadDat) }
    if(is.character(LoadDat)) {
      if(!file.exists(LoadDat)){
        stop("Character string input for LoadDat argument does not resemble an existing file path.")
      }
      if(is.null(FileSep)){
        stop("LoadDat is a file path. If you want to load a file as input, you must also specify its delimiter (FileSep).")
      }
      details <- read.delim(LoadDat, sep=FileSep) 
    }
    if(!is.object(LoadDat) & !is.character(LoadDat)){
      stop("LoadDat is incorrectly specified. Must either be the name of an object in R, or a file path character string.")
    }
    
    if(!file.exists(Dir)){
      stop("Character string input for Dir argument does not resemble an existing file path.")
    }
    
    # Check valid inputs for the quality screening process.
    if(!is.logical(QualityScreen)){
      stop("QualityScreen argument should be a logical input. See help ?MODISSummaries.")
    }
    if(QualityScreen){
      if(is.null(QualityBand) | is.null(QualityThreshold)){
        stop("QualityScreen=TRUE, but not all optional arguments the QualityCheck function needs are added. See help.")
      }
    }
    
    if(!any(GetBands(Product) == Band)){
      stop("Band does not match with the Product entered.")
    }
    
    # NoDataFill should be one integer.
    if(length(NoDataFill) != 1){
      stop("NoDataFill input must be one integer.")
    }
    if(!is.numeric(NoDataFill)){
      stop("NoDataFill should be numeric class. One integer.")
    }
    if(abs(NoDataFill - round(NoDataFill)) > .Machine$double.eps^0.5){
      stop("NoDataFill input must be one integer.")
    }
    # ValidRange should be a numeric vector, length 2.
    if(length(ValidRange) != 2){
      stop("ValidRange input must be a numeric vector - an upper and lower bound.")
    }
    if(!is.numeric(ValidRange)){
      stop("ValidRange should be numeric class.")
    }
    # ScaleFactor should be numeric, length 1.
    if(length(ScaleFactor) != 1){
      stop("ValidRange input must be a numeric vector - an upper and lower bound.")
    }
    if(!is.numeric(ScaleFactor)){
      stop("ValidRange should be numeric class.")
    }
    
    # Year or posixt date format?
    Year <- FALSE
    POSIXt <- FALSE
    char.compatible <- as.character(details$end.date)
    if(!is.character(char.compatible) | all(is.na(char.compatible)) & any(nchar(details$end.date) != 4)){
      POSIXt <- TRUE
    }
    posix.compatible <- try(as.POSIXlt(details$end.date), silent=TRUE)
    if(class(posix.compatible) == "try-error"){
      Year <- TRUE
    }
    if(!Year & !POSIXt){
      stop("Date information in LoadDat is not recognised as years or as POSIXt format. Check dates conform to one of these.")
    }
    if(Year & POSIXt){
      stop("Date information in LoadDat is recognised as both year and POSIXt formats. Check dates conform to one of these.")
    }
    
    if(StartDate){
      if(!any(names(details) == "start.date")){
        stop("StartDate == TRUE, but no start.date field found in LoadDat.")
      }
    }
    
    # Get a list of all downloaded subset (.asc) files in the data directory.
    filelist <- list.files(path=Dir, pattern=".asc")
    
    # Time-series analysis for each time-series (.asc file) consecutively.
    band.data.by.site <- list(NA)     # Initialise objects to store summarised data.
    band <- c()
    for(counter in 1:length(filelist)){
      # Load selected .asc file into a data frame, name columns and tell user what's being processed.
      ds <- read.csv(paste(Dir, "/", filelist[counter], sep=""), header=FALSE, as.is=TRUE)
      names(ds) <- c("row.id", "land.product.code", "MODIS.acq.date", "where", "MODIS.proc.date", 1:(ncol(ds) - 5))
      print(paste("Processing file ", counter, " of ", length(filelist), "...", sep=""))
      
      ##### Section that uses the files metadata strings [ ,1:5] for each time-series to extract necessary information.
      # Find location information from metadata string attached at the beginning of each downloaded set of pixels (i.e.
      # each time-step in the time-series), using regular expression.        
      # Extract lat and long data from the metadata string.
      wherelong <- regexpr("Lon", ds$where[1])
      whereSamp <- regexpr("Samp", ds$where[1])
      lat <- as.numeric(substr(ds$where[1], 4, wherelong - 1))
      long <- as.numeric(substr(ds$where[1], wherelong + 3, whereSamp - 1))
      
      # Identify which rows in ds correspond to the product band and which are reliability data.
      if(any(grepl(Band, ds$row.id))){
        which.are.band <- grep(Band, ds$row.id)
      } else {
        stop("Cannot find which rows in LoadDat are band data. Make sure the only ascii files in the directory are 
             those downloaded from MODISSubsets.")
      }
      
      if(QualityScreen){
        if(any(grepl(QualityBand, ds$row.id))){
          which.are.reliability <- grep(QualityBand, ds$row.id)
        } else {
          stop("Cannot find which rows in LoadDat are quality data. Download quality data with band data in MODISSubsets
               if you want to check for poor quality data.")
        }
      }
      #####
      
      #  Organise data into matrices containing product band data and another for corresponding reliability data.
      band.time.series <- as.matrix(ds[which.are.band,6:ncol(ds)], nrow=length(which.are.band), ncol=length(6:ncol(ds)))
      if(QualityScreen){
        rel.time.series <- as.matrix(ds[which.are.reliability,6:ncol(ds)], nrow=length(which.are.reliability), ncol=length(6:ncol(ds))) 
      }
      
      # Screen the pixel values in band.time.series: any pixels whose value correspond to NoDataFill, or whose
      # corresponding pixel in rel.time.series is below QualityThreshold, will be replaced with NA so they
      # are not included in time-series analysis.
      if(QualityScreen){
        band.time.series <- QualityCheck(Data=band.time.series, QualityScores=rel.time.series, 
                                         Band=Band, NoDataFill=NoDataFill, QualityBand=QualityBand,
                                         Product=Product, QualityThreshold=QualityThreshold)
      } else if(!QualityScreen){
        band.time.series <- matrix(
          ifelse(band.time.series != NoDataFill, band.time.series, NA),
          nrow=length(which.are.band))
      }
      
      # Final check, that band values all fall within the ValidRange (as defined for given MODIS product band).
      if(any(!(band.time.series >= ValidRange[1] && band.time.series <= ValidRange[2]), na.rm=TRUE)) { 
        stop("Some values fall outside the valid range, after no fill values should have been removed.") 
      }
      
      # Extract year and day from the metadata and make POSIXlt dates (YYYY-MM-DD), ready for time-series analysis.
      ds$Year <- as.numeric(substr(ds$MODIS.acq.date, 2, 5))
      ds$Day <- as.numeric(substr(ds$MODIS.acq.date, 6, 8))
      ds$date <- strptime(paste(ds$Year, "-", ds$Day, sep=""), "%Y-%j")
      
      ########## Interpolation
      # Initialise objects for various summaries.
      mean.band <- rep(NA, ncol(band.time.series))
      sd.band <- rep(NA, ncol(band.time.series))
      band.yield <- rep(NA, ncol(band.time.series))
      nofill <- rep(NA, ncol(band.time.series))
      poorquality <- rep(NA, ncol(band.time.series))
      band.min <- rep(NA, ncol(band.time.series))
      band.max <- rep(NA, ncol(band.time.series))
      
      # Run time-series analysis for the ith pixel.
      for(i in 1:ncol(band.time.series)) {
        # Minimum and maximum band values observed.
        minobsband <- min(as.numeric(band.time.series[ ,i]) * ScaleFactor, na.rm=TRUE)    
        maxobsband <- max(as.numeric(band.time.series[ ,i]) * ScaleFactor, na.rm=TRUE)
        
        # Assess the quality of data at this time-step by counting the number of data left after screening, and use this
        # assessment to decide how to proceed with analysis for each time-step.
        if(QualityScreen){
          data.quality <- sum(!is.na(band.time.series[ ,i]))
        } else if(!QualityScreen){
          data.quality <- 2
        }
        
        if(data.quality >= 2) {
          # Linearly interpolate between screened data points, for each pixel, over time (daily from first to last date).
          sout <- approx(x=ds$date[which.are.band], y=as.numeric(band.time.series[ ,i]) * ScaleFactor, method="linear", 
                         n=((max(ds$date[!is.na(band.time.series[ ,i])]) - min(ds$date[!is.na(band.time.series[ ,i])])) - 1))
          
          # Carry out all the relevant summary analyses, set by options in the function call.
          if(Yield){ band.yield[i] <- (sum(sout$y) - minobsband * length(sout$x)) / length(sout$x) }    # (((365*length(years))-16)*365) = average annual yield  (i.e. work out daily yield * 365).
          if(Mean){ 
            if(Interpolate){
              mean.band[i] <- mean(sout$y)
            } else if(!Interpolate){
              mean.band[i] <- mean(as.numeric(band.time.series[ ,i]) * ScaleFactor, na.rm=TRUE)
            }
          }
          if(SD){
            if(Interpolate){
              sd.band[i] <- sd(sout$y)
            } else if(!Interpolate){
              sd.band[i] <- sd(as.numeric(band.time.series[ ,i]) * ScaleFactor, na.rm=TRUE)
            }  
          }
        }
        if(data.quality == 1){
          warning("Only single data point that passed the quality screen: cannot summarise", immediate.=TRUE)
          if(Mean){ mean.band[i] <- mean(as.numeric(band.time.series[ ,i]) * ScaleFactor, na.rm=TRUE) } 
        }
        if(data.quality == 0){
          warning("No reliable data for this pixel", immediate.=TRUE)
        }
        
        # Complete final optional summaries, irrespective of data quality.
        if(Min){ band.min[i] <- minobsband }
        if(Max){ band.max[i] <- maxobsband }
        nofill[i] <- paste(round((sum(ds[ ,i + 5] == NoDataFill) / length(band.time.series[ ,i])) * 100, 2), "% (",
                             sum(ds[ ,i + 5] == NoDataFill), "/", length(band.time.series[ ,i]), ")", sep="")
        if(QualityScreen){
          poorquality[i] <- paste(round((sum(rel.time.series[ ,i] > QualityThreshold) / length(rel.time.series[ ,i])) * 100, 2),
                                  "% (", sum(rel.time.series[ ,i] > QualityThreshold), "/", length(rel.time.series[ ,i]), ")", sep="")
        } else if(!QualityScreen){
          poorquality[i] <- NA
        }

      } # End of loop for time-series summary analysis for each pixel.
      
      # Extract ID for this .asc file time-series so it can be included in final summary output.
      where.id <- regexpr("_", filelist[counter])
      id <- rep(substr(filelist[counter], 1, where.id - 1), length(mean.band))
      
      # Compile time-series information and relevant summaries data into a final output listed by-sites (.asc files).
      band.data.by.site[[counter]] <- data.frame(ID=id, lat=rep(lat,length(mean.band)), long=rep(long,length(mean.band)),
                                                 start.date=rep(min(ds$date),length(mean.band)), end.date=rep(max(ds$date),length(mean.band)),
                                                 min.band=band.min, max.band=band.max, mean.band=mean.band, sd.band=sd.band, band.yield=band.yield, 
                                                 no.fill.data=nofill, poor.quality.data=poorquality)
      # Extract mean band values.
      band <- rbind(band,mean.band)
    } # End of loop that reitrates time-series summary for each .asc file.
    
    # Write output summary file by appending summary data from all files, producing one file of summary stats output.
    print("Writing summaries and collecting data...")
    write.table(band.data.by.site[[1]], file=paste(Dir, "/", "MODIS Summary ", Sys.Date(), ".csv", sep=""),
                sep=",", row.names=FALSE)
    if(length(filelist) > 1){ 
      for(i in 2:length(filelist)){ 
        write.table(band.data.by.site[[i]], file=paste(Dir, "/", "MODIS Summary ", Sys.Date(), ".csv", sep=""), 
                      sep=",", append=TRUE, row.names=FALSE, col.names=FALSE) 
      } 
    }
    
    # Following code will append the mean (time-averaged) band values for each pixel, for each time-series, to the
    # original input dataset (details) to produce one file that contains all necessary information.
    w.lat <- regexpr("Lat", filelist)
    w.lon <- regexpr("Lon", filelist)
    w.start <- regexpr("Start", filelist)
    w.end <- regexpr("End", filelist)
    w.finish <- regexpr("_", filelist)
    lats <- as.numeric(substr(filelist, w.lat + 3, w.lon - 1))
    lons <- as.numeric(substr(filelist, w.lon + 3, w.start - 1))
    e.dates <- strptime(substr(filelist, w.end + 3, w.finish - 1), "%Y-%m-%d")
    res <- data.frame(details, band.pixels=matrix(NA, nrow=nrow(details), ncol=ncol(band)))
    
    if(Year){
      e.dates <- as.numeric(e.dates$year + 1900)
      if(StartDate){
        s.dates <- as.numeric(strptime(substr(filelist, w.start + 5, w.end - 1), "%Y-%m-%d")$year + 1900)
        ID.match <- data.frame(lat=lats, long=lons, end.date=e.dates, start.date=s.dates)
      } else if(!StartDate){
        ID.match <- data.frame(lat=lats, long=lons, end.date=e.dates)
      }
    } else if(POSIXt){
      if(StartDate){
        s.dates <- strptime(substr(filelist, w.start + 5, w.end - 1), "%Y-%m-%d")
        ID.match <- data.frame(lat=lats, long=lons, end.date=e.dates, start.date=s.dates)
      } else if(!StartDate){
        ID.match <- data.frame(lat=lats, long=lons, end.date=e.dates)
      }
    }
    
    
    # Use FindID for each row of ID.match, to add the right band subscripts to the right details subscripts.
    for(i in 1:nrow(ID.match)){
      match.subscripts <- FindID(ID.match[i, ], details)
      if(all(match.subscripts != "No matches found.")){
        for(x in 1:length(match.subscripts)){
          res[match.subscripts[x],(ncol(details) + 1):ncol(res)] <- band[i, ]
        }
      }
    } 
    any(is.na(res[ ,(ncol(details) + 1):ncol(res)]))
    
    # Write the final appended dataset to a csv file, ready for use, in Dir.
    write.table(res, file=paste(Dir, "/", "MODIS Data ", Sys.Date(), ".csv", sep=""), sep=",", col.names=TRUE, row.names=FALSE)
    print(paste("Done! Check the 'MODIS Summary' and 'MODIS Data' output files.", sep=""))  
  }