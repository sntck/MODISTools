MODISSummaries <-
function(LoadDat, FileSep = NULL, Dir = ".", Product, Band, ValidRange, NoDataFill, ScaleFactor, StartDate = FALSE,
         QualityScreen = FALSE, QualityBand = NULL, QualityThreshold = NULL, Mean = TRUE, SD = TRUE, Min = TRUE, Max = TRUE,
         Yield = FALSE, Interpolate = TRUE) 
{ 
    # Load input time-series data file; external data file, or an R object.
    if(!is.object(LoadDat) & !is.character(LoadDat)) stop("LoadDat must be an object in R or a file path character string.")
    if(is.object(LoadDat)) details <- data.frame(LoadDat)
    if(is.character(LoadDat)){
      if(!file.exists(LoadDat)) stop("Character string input for LoadDat does not resemble an existing file path.")
      if(is.null(FileSep)) stop("To load a file as input, you must also specify its delimiter (FileSep).")
      details <- read.delim(LoadDat, sep = FileSep) 
    }
    
    if(!file.exists(Dir)) stop("Character string input for Dir argument does not resemble an existing file path.")
    
    # Check valid inputs for the quality screening process.
    if(!is.logical(QualityScreen)) stop("QualityScreen argument should be a logical input. See help ?MODISSummaries.")
    if(QualityScreen){
      if(is.null(QualityBand) | is.null(QualityThreshold)) stop("QualityBand and QualityThreshold not specified.")
    }
    
    if(!any(Band == GetBands(Product))) stop("No Band input matches with the Product input.")
    
    # NoDataFill should be one integer.
    if(length(NoDataFill) != 1) stop("NoDataFill input must be one integer.")
    if(!is.numeric(NoDataFill)) stop("NoDataFill should be numeric class. One integer.")
    if(abs(NoDataFill - round(NoDataFill)) > .Machine$double.eps^0.5) stop("NoDataFill input must be one integer.")
    
    # ValidRange should be a numeric vector, length 2.
    if(length(ValidRange) != 2) stop("ValidRange input must be a numeric vector - an upper and lower bound.")
    if(!is.numeric(ValidRange)) stop("ValidRange should be numeric class.")
    
    # ScaleFactor should be numeric, length 1.
    if(length(ScaleFactor) != 1) stop("ScaleFactor input must be one numeric element.")
    if(!is.numeric(ScaleFactor)) stop("ScaleFactor should be numeric class.")
    
    # Year or posixt date format?
    Year <- FALSE
    POSIXt <- FALSE
    char.compatible <- as.character(details$end.date)
    if(!is.character(char.compatible) | all(is.na(char.compatible)) & any(nchar(details$end.date) != 4)) POSIXt <- TRUE
    
    posix.compatible <- try(as.POSIXlt(details$end.date), silent=TRUE)
    if(class(posix.compatible) == "try-error") Year <- TRUE
    
    if(!Year & !POSIXt) stop("Date informDate information in LoadDat is not recognised as years or as POSIXt format.")
    if(Year & POSIXt) stop("Date information in LoadDat is recognised as both year and POSIXt formats.")
    
    if(StartDate & !any(names(details) == "start.date")) stop("StartDate == TRUE, but no start.date field found in LoadDat.")
    
    # Get a list of all downloaded subset (.asc) files in the data directory.
    file.list <- list.files(path = Dir, pattern = paste(Product, ".*asc$", sep = ""))
    if(length(file.list) == 0) stop("Found no MODIS data files in Dir that match the request.")
    
    size.test <- sapply(file.list, function(x) ncol(read.csv(x)[1, ]) - 5)
    if(!all(size.test == size.test[1])) stop("The number of pixels (Size) in subsets identified are not all the same.")
    
    # Time-series analysis for each time-series (.asc file) consecutively.
    band.data.site <- lapply(size.test, function(x) matrix(nrow = x, ncol = 12))
    band <- matrix(nrow = length(file.list), ncol = size.test[1])
    
    for(counter in 1:length(file.list)){
      
      print(paste("Processing file ", counter, " of ", length(file.list), "...", sep = ""))
      
      # Load selected .asc file into a data frame, name columns and tell user what's being processed.
      ds <- read.csv(paste(Dir, "/", file.list[counter], sep = ""), header = FALSE, as.is = TRUE)
      names(ds) <- c("row.id", "land.product.code", "MODIS.acq.date", "where", "MODIS.proc.date", 1:(ncol(ds) - 5))
      
      ##### Section that uses the files metadata strings [ ,1:5] for each time-series to extract necessary information.
      wherelong <- regexpr("Lon", ds$where[1])
      whereSamp <- regexpr("Samp", ds$where[1])
      lat <- as.numeric(substr(ds$where[1], 4, wherelong - 1))
      long <- as.numeric(substr(ds$where[1], wherelong + 3, whereSamp - 1))
      
      # Identify which rows in ds correspond to the product band and which are reliability data.
      ifelse(any(grepl(Band, ds$row.id)),
             which.band <- grep(Band, ds$row.id),
             stop("Cannot find band data in LoadDat. Make sure ASCII files in the directory are from MODISSubsets."))
      if(QualityScreen){
        ifelse(any(grepl(QualityBand, ds$row.id)),
               which.rel <- grep(QualityBand, ds$row.id),
               stop("Cannot find quality data in LoadDat. Download quality data with band data in MODISSubsets."))
      }
      #####
      
      #  Organise data into matrices containing product band data and another for corresponding reliability data.
      band.time.series <- as.matrix(ds[which.band,6:ncol(ds)], nrow = length(which.band), ncol = length(6:ncol(ds)))
      if(QualityScreen) rel.time.series <- as.matrix(ds[which.rel,6:ncol(ds)], nrow = length(which.rel), ncol = length(6:ncol(ds)))
      
      # Screen pixels in band.time.series: any NoDataFill or pixels < QualityThreshold, will be replaced with NA.
      ifelse(QualityScreen,
             band.time.series <- QualityCheck(Data = band.time.series, QualityScores = rel.time.series, Band = Band,
                                              NoDataFill = NoDataFill, QualityBand = QualityBand, Product = Product,
                                              QualityThreshold = QualityThreshold),
             band.time.series <- matrix(ifelse(band.time.series != NoDataFill, band.time.series, NA), nrow = length(which.band)))
      
      # Final check, that band values all fall within the ValidRange (as defined for given MODIS product band).
      if(any(!(band.time.series >= ValidRange[1] && band.time.series <= ValidRange[2]), na.rm = TRUE)){ 
        stop("Some values fall outside the valid range, after no fill values should have been removed.") 
      }
      
      # Extract year and day from the metadata and make POSIXlt dates (YYYY-MM-DD), ready for time-series analysis.
      ds$Year <- as.numeric(substr(ds$MODIS.acq.date, 2, 5))
      ds$Day <- as.numeric(substr(ds$MODIS.acq.date, 6, 8))
      ds$date <- strptime(paste(ds$Year, "-", ds$Day, sep = ""), "%Y-%j")
      
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
      for(i in 1:ncol(band.time.series)){
        
        # Minimum and maximum band values observed.
        minobsband <- min(as.numeric(band.time.series[ ,i]) * ScaleFactor, na.rm = TRUE)    
        maxobsband <- max(as.numeric(band.time.series[ ,i]) * ScaleFactor, na.rm = TRUE)
        
        # Assess quality of data at this time-step, counting data left after screening, to decide how to proceed with analysis.
        ifelse(QualityScreen,
               data.quality <- sum(!is.na(band.time.series[ ,i])),
               data.quality <- 2)
        
        if(data.quality >= 2){
          # Linearly interpolate between screened data points, for each pixel, over time (daily from first to last date).
          sout <- approx(x = ds$date[which.band], y = as.numeric(band.time.series[ ,i]) * ScaleFactor, method = "linear", 
                         n = ((max(ds$date[!is.na(band.time.series[ ,i])]) - min(ds$date[!is.na(band.time.series[ ,i])])) - 1))
          
          # Carry out all the relevant summary analyses, set by options in the function call.
          # (((365*length(years))-16)*365) = average annual yield  (i.e. work out daily yield * 365).
          if(Yield) band.yield[i] <- (sum(sout$y) - minobsband * length(sout$x)) / length(sout$x)
          if(Mean){
            ifelse(Interpolate,
                   mean.band[i] <- mean(sout$y),
                   mean.band[i] <- mean(as.numeric(band.time.series[ ,i]) * ScaleFactor, na.rm = TRUE))
          }
          if(SD){
            ifelse(Interpolate,
                   sd.band[i] <- sd(sout$y),
                   sd.band[i] <- sd(as.numeric(band.time.series[ ,i]) * ScaleFactor, na.rm = TRUE))  
          }
        }
        
        if(data.quality == 1){
          warning("Only single data point that passed the quality screen: cannot summarise", immediate. = TRUE)
          if(Mean) mean.band[i] <- mean(as.numeric(band.time.series[ ,i]) * ScaleFactor, na.rm = TRUE)
        }
        
        if(data.quality == 0) warning("No reliable data for this pixel", immediate. = TRUE)
        
        # Complete final optional summaries, irrespective of data quality.
        if(Min) band.min[i] <- minobsband
        if(Max) band.max[i] <- maxobsband
        
        nofill[i] <- paste(round((sum(ds[ ,i + 5] == NoDataFill) / length(band.time.series[ ,i])) * 100, 2), "% (",
                           sum(ds[ ,i + 5] == NoDataFill), "/", length(band.time.series[ ,i]), ")", sep = "")
        
        ifelse(QualityScreen,
               poorquality[i] <- paste(round((sum(rel.time.series[ ,i] > QualityThreshold) / length(rel.time.series[ ,i])) * 100, 2),
                                       "% (", sum(rel.time.series[ ,i] > QualityThreshold), "/", length(rel.time.series[ ,i]), ")",
                                       sep = ""),
               poorquality[i] <- NA)
        
      } # End of loop for time-series summary analysis for each pixel.
      
      # Extract ID for this .asc file time-series so it can be included in final summary output.
      where.id <- regexpr("_", file.list[counter])
      id <- rep(substr(file.list[counter], 1, where.id - 1), length(mean.band))
      
      # Compile time-series information and relevant summaries data into a final output listed by-sites (.asc files).
      band.data.site[[counter]] <- 
        data.frame(ID = id, lat = rep(lat,length(mean.band)), long = rep(long,length(mean.band)),
                   start.date = rep(min(ds$date), length(mean.band)), end.date = rep(max(ds$date), length(mean.band)),
                   min.band = band.min, max.band = band.max, mean.band = mean.band, sd.band = sd.band, band.yield = band.yield, 
                   no.fill.data = nofill, poor.quality.data = poorquality)
      
      # Extract mean band values.
      band[counter, ] <- mean.band
      
    } # End of loop that reitrates time-series summary for each .asc file.
    
    # Append the summaries for each pixel, for each time-series, to the original input dataset (details).
    ifelse(StartDate,
           ID.match <- data.frame(unique(cbind(lat = details$lat[!is.na(details$lat)], long = details$long[!is.na(details$lat)],
                                               end.date = details$end.date[!is.na(details$lat)],
                                               start.date = details$start.date[!is.na(details$lat)]))),
           ID.match <- data.frame(unique(cbind(lat = details$lat[!is.na(details$lat)], long = details$long[!is.na(details$lat)],
                                               end.date = details$end.date[!is.na(details$lat)]))))
    
    if(nrow(ID.match) != nrow(band)) stop("Differing number of unique locations found between LoadDat and ASCII subsets.")
    
    res <- data.frame(details, band.pixels = matrix(NA, nrow = nrow(details), ncol = ncol(band)))
    
    # Use FindID for each row of ID.match, to add the right band subscripts to the right details subscripts.
    for(i in 1:nrow(ID.match)){
      match.subscripts <- FindID(ID.match[i, ], details)
      if(all(match.subscripts != "No matches found.")){
        for(x in 1:length(match.subscripts)) res[match.subscripts[x],(ncol(details) + 1):ncol(res)] <- band[i, ]
      }
    }
    
    # Write output summary file by appending summary data from all files, producing one file of summary stats output.
    write.table(band.data.site[[1]], file = paste(Dir, "/", "MODIS Summary ", Sys.Date(), ".csv", sep = ""),
                sep = ",", row.names = FALSE)
    if(length(file.list) > 1){ 
      for(i in 2:length(file.list)){ 
        write.table(band.data.site[[i]], file = paste(Dir, "/", "MODIS Summary ", Sys.Date(), ".csv", sep = ""), 
                    sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE) 
      } 
    }
    
    # Write the final appended dataset to a csv file, ready for use, in Dir.
    write.table(res, file = paste(Dir, "/", "MODIS Data ", Sys.Date(), ".csv", sep = ""),
                sep = ",", col.names = TRUE, row.names = FALSE)
    
    print(paste("Done! Check the 'MODIS Summary' and 'MODIS Data' output files.", sep = ""))  
}