# =============================================================================
# A class definition for a call to the MODISSubsets function.
# Each instantiated "ModisRequest" object is a unique request for MODIS data.
# The class includes all necessary data and methods to request subsets.
# =============================================================================

ModisRequest <- R6Class("ModisRequest",
    inherit = .ModisMethods,
    public = list(
      ##### Public fields
      inputData = NA,
      products  = NA,
      bands     = NA,
      size      = NA,
      saveDir   = NA,
      transect  = FALSE,
      bandList  = NA,
      dateList  = NA,

      ##### Public methods
      initialize = function(...)
      {
        ## Assign the variable list of arguments to objects within this scope.
        args <- do.call(c, list(...))
        for(name in names(args)) assign(name, args[[name]])

        ## Check if any necessary arguments are missing.
        if(any(missing(LoadDat), missing(Products), missing(Bands), missing(Size)))
          stop("Input was missing: LoadDat, Products, Bands, and Size are all required.")

        ## Populate the object fields upon initialisation.
        self$inputData <- LoadDat
        self$products  <- Products
        self$bands     <- Bands
        self$size      <- Size

        if("SaveDir" %in% names(optionalInput))
        {
          self$saveDir <- normalizePath(optionalInput$SaveDir) ## Creates absolute path.
        } else {
          cat("SaveDir not specified: default is the working directory.\n")
          self$saveDir <- getwd()
        }

        if("Transect" %in% names(optionalInput)) self$transect <- optionalInput$Transect

        ## If optional variable names were supplied, make these now conform with what is expected.
        if(any("LatColName" %in% names(optionalInput)))
        {
          tryCatch(stopifnot(any(LatColName %in% names(self$inputData))),
                   error = function(e) stop(simpleError("LatColName supplied does not match any variable name in LoadDat.")))
          names(self$inputData)[names(self$inputData) == LatColName] <- "lat"
        }
        if(any("LongColName" %in% names(optionalInput)))
        {
          tryCatch(stopifnot(any(LongColName %in% names(self$inputData))),
                   error = function(e) stop(simpleError("LongColName supplied does not match any variable name in LoadDat.")))
          names(self$inputData)[names(self$inputData) == LongColName] <- "long"
        }
        if(any("StartDateColName" %in% names(optionalInput)))
        {
          tryCatch(stopifnot(any(StartDateColName %in% names(self$inputData))),
                   error = function(e) stop(simpleError("StartDateColName supplied does not match any variable name in LoadDat.")))
          names(self$inputData)[names(self$inputData) == StartDateColName] <- "start.date"
        }
        if(any("EndDateColName" %in% names(optionalInput)))
        {
          tryCatch(stopifnot(any(EndDateColName %in% names(self$inputData))),
                   error = function(e) stop(simpleError("EndDateColName supplied does not match any variable name in LoadDat.")))
          names(self$inputData)[names(self$inputData) == EndDateColName] <- "end.date"
        }

        ## Organise bands into tidy data.frame with product names that will simplify downloading later.
        self$bandList <- lapply(self$products, function(x) data.frame(product = I(x), band = I(self$getBands(x))))
        self$bandList <- Reduce(rbind, self$bandList)
        self$bandList <- subset(self$bandList, band %in% self$bands)
        ## Order bandList using the user input so that output data files intuitively reflect the user function call.
        ## Group by product then organise bands within products.
        orderAsUserInput <- with(self$bandList,
                                 order(sapply(product, function(product) which(self$products == product)),
                                       sapply(band, function(band) which(self$bands == band))))
        self$bandList <- self$bandList[orderAsUserInput, ]

        ## Store available dates for each product in a list: a list because length of dates can vary among products.
        self$dateList <- lapply(self$products, function(x) self$getDates(self$inputData$lat[1], self$inputData$long[1], x))
        names(self$dateList) <- self$products

        ## Print the opening text output detailing the MODISSubsets function call.
        private$requestMessage()
      },
      ##
      validateInput = function()
      {
        ## Check self$inputData is coercible to data.frame.
        tryCatch(self$inputData <- data.frame(self$inputData),
                 error = function(e) stop(simpleError("LoadDat is not coercible to a data.frame.")))

        ## Check self$inputData contains all the necessary variables.
        tryCatch(stopifnot("lat" %in% names(self$inputData),
                           "long" %in% names(self$inputData),
                           "start.date" %in% names(self$inputData),
                           "end.date" %in% names(self$inputData)),
                 error = function(e) stop(simpleError("LoadDat does not have all required variables: lat, long, start.date, end.date.")))

        ## Check the latitude and longitude input values are valid -- ignoring NAs here.
        tryCatch(stopifnot(abs(na.omit(self$inputData$lat)) <= 90,
                           abs(na.omit(self$inputData$long)) <= 180),
                 error = function(e) stop(simpleError("Some latitude or longitude values are beyond the range of valid values.")))

        ## Check no coordinates supplied are partially incomplete.
        tryCatch(stopifnot(all(is.na(self$inputData$lat) == is.na(self$inputData$long))),
                 error = function(e) stop(simpleError("Some coordinates are partially incomplete.")))

        ## Now coordinates are complete, check each one has required dates.
        tryCatch(stopifnot(all(is.na(self$inputData$lat) == is.na(self$inputData$start.date)),
                           all(is.na(self$inputData$lat) == is.na(self$inputData$end.date))),
                 error = function(e) stop(simpleError("Some coordinates do not have complete start and end dates.")))

        ## Check Size input was valid: two integers 0 <= size <= 100.
        tryCatch(stopifnot(is.numeric(self$size),
                           all(abs(self$size - round(self$size)) < .Machine$double.eps^0.5),
                           length(self$size) == 2,
                           all(0 <= self$size & self$size <= 100)),
                 error = function(e) stop(simpleError("Size input invalid: should be two integers between 0 and 100.")))

        ## Check the Products input was a valid member of the MODIS products list.
        productList <- c("MCD12Q1",   "MCD12Q2", "MCD43A1",    "MCD43A2", "MCD43A4", "MOD09A1", "MOD11A2", "MOD13Q1", "MOD15A2",
                         "MOD15A2GFS","MOD16A2", "MOD17A2_51", "MOD17A3", "MYD09A1", "MYD11A2", "MYD13Q1", "MYD15A2")
        tryCatch(stopifnot(all(self$products %in% productList)),
                 error = function(e) stop(simpleError("A named Product is not a MODIS product available through the web service.")))

        ## Check the Bands input are valid given the Products input.
        tryCatch(stopifnot(all(self$bands %in% self$bandList$band),          ## Are all input Bands found in available bands?
                           all(self$products %in% self$bandList$product)),   ## Are all input Products used to get Bands?
                 error = function(e) stop(simpleError("Bands and Products input do not fully match.")))

        ## Check SaveDir matches an existing directory.
        tryCatch(stopifnot(file.exists(self$saveDir)),
                 error = function(e) stop(simpleError("Input for SaveDir does not resemble an existing file path.")))
      },
      ##
      subsetClean = function()
      {
        ## Take input dataset and remove all rows that are incomplete or duplicated time series.
        self$inputData <- subset(self$inputData, !is.na(lat) | !is.na(long) | !is.na(start.date) | !is.na(end.date))
        self$inputData <- subset(self$inputData, !duplicated(data.frame(lat, long, start.date, end.date)))

        cat("Found",nrow(self$inputData),"unique time series to download.\n")

        private$createID()

        ## Create variable that will store the status of each subset's download success.
        self$inputData$Status <- NA
      },
      ##
      createModisDates = function()
      {
        switch(private$checkDateFormat(),
               "posix" = {
                 startDates <- as.Date(self$inputData$start.date)
                 endDates   <- as.Date(self$inputData$end.date)
               },
               "year" = {
                 startDates <- as.Date(paste0(self$inputData$start.date,"-01-01"))
                 endDates   <- as.Date(paste0(self$inputData$end.date,"-12-31"))
               })

        ## Format days information for MODIS-style dates (A + YYYYDDD).
        modisDates <- data.frame(start = I(self$posixToModisDates(startDates)),
                                 end   = I(self$posixToModisDates(endDates)))

        ## Check start and end dates fall within the available range.
        tryCatch(stopifnot(all(startDates >= min(self$modisToPosixDates(unlist(self$dateList)))),
                           all(endDates <= max(self$modisToPosixDates(unlist(self$dateList))))),
                 error = function(e) stop(simpleError("Some dates requested fall outside the available range.")))

        ## Return dates in MODIS format.
        return(modisDates)
      },
      ##
      prepareDatesForDownload = function(start, end)
      {
        ## For each product, remove dates beyond the requested range.
        self$dateList <- lapply(self$products, function(product)
        {
          dates <- subset(self$dateList[[product]],
                          start <= self$dateList[[product]] & self$dateList[[product]] <= end)

          ## Find time-series length modulo maxRequestLength and create vector of NAs to fill a matrix.
          matrixFiller <- rep(NA, private$maxRequestLength - (length(dates) %% private$maxRequestLength))
          return(matrix(c(dates, matrixFiller), nrow = private$maxRequestLength))
        })
      },
      ##
      posixToModisDates = function(dates)
      {
        days <- as.POSIXlt(dates)$yday + 1 ## +1 because Jan 1 is day 0.
        days[nchar(days) == 2] <- paste0(0, days[nchar(days) == 2])
        days[nchar(days) == 1] <- paste0(0, 0, days[nchar(days) == 1])
        return(paste0("A", substr(dates, 1, 4), days))
      },
      ##
      modisToPosixDates = function(dates)
      {
        years <- substr(dates, 2, 5)
        days  <- substr(dates, 6, 8)
        return(as.Date(paste0(years, "-01-01")) + (as.numeric(days) - 1)) ## -1 because Jan 1 is day 0.
      },
      ##
      createEmptySubset = function()
      {
        ## Create empty list to store the subset: each list element is a product, with length time-series*bands.
        mapply(function(subsetDates, subsetProduct)
        {
          numBands <- nrow(subset(self$bandList, product == subsetProduct))
          rep(NA, length = sum(!is.na(subsetDates)) * numBands)
        },
        subsetDates = self$dateList, subsetProduct = self$products, SIMPLIFY = FALSE)
      },
      ##
      subsetDownload = function(subset, subsetID)
      {
        ## For each subset loop over all the data bands to download.
        for(i in 1:nrow(self$bandList))
        {
          ## Iterate subset download for each batch of dates in the time series from request$dateList.
          download <- private$batchDownload(subset = subsetID, dataType = i)

          ## Check the subset was successfully downloaded. Did try catch an error? Do any subsets contain erroneous strings?
          private$validateDownload(download)

          ## Tidy the subset download into a data.frame and store in subsets.
          download <- Reduce(rbind, download)

          ## Collapse download into a vector of strings, where each string contains all data for one date in a time series.
          subsetStrings <- with(download, paste(nrow, ncol, xll, yll, pixelsize, unlist(subset), sep = ','))

          ## Write subsetStrings (data for the ith band) to the object containing the whole subset.
          subsetEntries <- seq(min(which(is.na(subset[[self$bandList$product[i]]]))), length.out = length(subsetStrings))
          subset[[self$bandList$product[i]]][subsetEntries] <- subsetStrings
        }

        ## Return a simplified data structure of subsets to prepare for writing to disk.
        return(Reduce(c, subset))
      },
      ##
      checkDownloadSuccess = function(subset, subsetID)
      {
        ## If there are any NAs remaining in subset or a string ends in a comma, then the download was incomplete.
        if(any(is.na(subset)) | any(substr(subset, nchar(subset), nchar(subset)) == ','))
        {
          cat("Missing information for",self$inputData$ID[subsetID],"time series. Retrying download...\n")

          ## Wipe subset clean and then retry the download.
          subset <- self$createEmptySubset()
          subset <- self$subsetDownload(subset, subsetID = subsetID)

          ## Check whether download is still incomplete and if it is, print a message then continue.
          ## If any dates are empty, remove them, print a warning and record the dates in the download log.
          if(any(substr(subset, nchar(subset), nchar(subset)) == ',')){
            cat("The incomplete download was retried but remains incomplete. See subset download file.\n")
            self$inputData$Status[subsetID] <- "Missing data in subset: try downloading again"
          } else if(any(is.na(subset))){
            subsetDates <- sapply(subset, function(x) strsplit(x, ',')[[1]][self$whereIsDate], USE.NAMES=FALSE)
            problemDates <- unlist(self$dateList)[!(unlist(self$dateList) %in% subsetDates)]

            warning("There is no data for some requested dates:\n",
                    "Subset ID = ",self$inputData$ID[subsetID],"\n",
                    "Latitude = ",self$inputData$lat[subsetID],"\n",
                    "Longitude = ",self$inputData$long[subsetID],"\n",
                    "Dates = ",problemDates,"\n",
                    call.=FALSE, immediate.=TRUE)

            subset <- subset[!is.na(subset)]
            self$inputData$Status[subsetID] <- paste("Some dates were missing:", paste(problemDates, collapse="; "))
          } else {
            cat("The incomplete download was re-tried and has been successful.\n")
          }
        } else {
          self$inputData$Status[subsetID] <- "Successful download"
        }
      },
      ##
      writeSummaryFile = function()
      {
        runTime <- as.POSIXlt(Sys.time())
        timeStamp <- paste(as.Date(runTime),
                           with(runTime, paste(paste0('h',hour), paste0('m',min), paste0('s',round(sec, digits=0)), sep = '-')),
                           sep = '_')

        if(!self$transect){
          write.table(self$inputData, file = file.path(self$saveDir, paste0("SubsetDownload_",timeStamp,".csv")),
                      col.names = TRUE, row.names = FALSE, sep = ',')
        } else {
          endOfTransectID <- regexpr("Point", self$inputData$ID[1])
          transectID <- substr(self$inputData$ID[1], 1, endOfTransectID-1)

          write.table(self$inputData, file = file.path(self$saveDir, paste0(transectID,"_SubsetDownload_",timeStamp,".csv")),
                      col.names = FALSE, row.names = FALSE, sep = ',', append = TRUE)
        }
      }
    ),

    private = list(
      ##### Private fields
      maxRequestLength = 10,     ## The maximum time-series length that a web service request can handle at one time.
      numSubsetMetadataCols = 5, ## Number of variables in downloaded subsets that are metadata before downloaded data begins.

      ##### Private methods
      requestMessage = function() cat("Files will be written to ",self$saveDir,".\n", sep=''),
      ##
      createID = function()
      {
        ## Check whether a variable resembling unique IDs already exists. If not, make one.
        whichVarAreIDs <- which(grepl("ID", names(self$inputData)))[1]
        numberOfUniqueIDs <- length(unique(self$inputData[ ,whichVarAreIDs]))

        if(numberOfUniqueIDs == nrow(self$inputData))
        {
          cat("Unique subset IDs found in variable: ",names(self$inputData)[whichVarAreIDs],".\n", sep='')

          ## Three underscores used as placemarker to find ID in strings, so cannot exist within IDs.
          if(any(grepl("___", self$inputData[whichVarAreIDs]))) stop("IDs cannot contain '___'.")

          ## Create new ID variable, at first column in the data.frame, named subsetID for convenience.
          self$inputData <- data.frame(subsetID = self$inputData[ ,whichVarAreIDs], self$inputData)
        } else {
          cat("No unique subset ID variable found so one will be created.\n")

          newID <- with(self$inputData, paste0("Lat",sprintf('%.5f',lat),"Lon",sprintf('%.5f',long),"Start",start.date,"End",end.date))
          self$inputData <- data.frame(subsetID = newID, self$inputData)
        }
      },
      ##
      checkDateFormat = function()
      {
        ## Were start.date and end.date supplied as years or dates coercible to POSIX/C99 construct?
        year <- FALSE
        posix <- FALSE

        allDates <- c(self$inputData$start.date, self$inputData$end.date)
        posixCompatible <- try(as.POSIXlt(allDates), silent = TRUE)

        if("try-error" %in% class(posixCompatible))
        {
          yearCompatible <- try(as.numeric(allDates), silent = TRUE)
          ifelse("try-error" %in% class(yearCompatible) | any(nchar(allDates) != 4),
                 stop("Dates in LoadDat are recognised in neither year or POSIXt format."),
                 year <- TRUE)
        } else {
          posix <- TRUE
        }

        ## Return either "year" or "posix", which will be passed to switch() in modis dates method.
        if(posix)     return("posix")
        else if(year) return("year")
      },
      ##
      batchDownload = function(subset, dataType)
      {
        ## Wrapper for self$getSubset() to run for >10 dates, as restricted by the web service method.
        subsetDates <- self$dateList[[self$bandList$product[dataType]]]
        try(apply(subsetDates, 2, function(dates)
        {
          self$getSubset(self$inputData$lat[subset], self$inputData$long[subset],
                         self$bandList$product[dataType], self$bandList$band[dataType],
                         min(dates, na.rm = TRUE), max(dates, na.rm = TRUE),
                         self$size[1], self$size[2])
        }))
      },
      ##
      validateDownload = function(download)
      {
        ## Check download ran without error and retry if it did.
        subsets <- unlist(download)[grepl("subset", names(unlist(download)))]
        tryCatch(stopifnot(is.list(download),
                           all(sapply(strsplit(subsets,','), length) > private$numSubsetMetadataCols)),
                 error = function(e) stop(simpleError("Download failed: either input was invalid or web service is busy.")))

        busy <- any(grepl("Server is busy handling other requests", subsets))
        #if(class(download) != "try-error" | busy)
      }
    )
)