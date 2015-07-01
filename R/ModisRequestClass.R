# =============================================================================
# An object class definition for a call to the MODISSubsets function.
# Each instantiated "ModisRequest" object is a unique request for MODIS data.
# The class include all necessary data to request data.
# Private methods used internally by MODISSubsets are also included.
# The class is created using the R6 class system.
# Author: Sean Tuck
# Last updated: 2015-06-28
# =============================================================================

# Load package for R6 class system
library(R6)

ModisRequest <- R6Class("ModisRequest",
    public = list(
      ##### Public fields
      inputData = NA,
      products = NA,
      bands = NA,
      size = NA,
      saveDir = NA,
      transect = FALSE,
      dateList = NA,

      ##### Public methods
      initialize = function(...)
      {
        ## Assign the variable list of arguments to objects within this scope.
        args <- do.call(c, list(...))
        for(i in 1:length(args)) assign(names(args)[i], args[[i]])

        ## Check if any necessary arguments are missing.
        if(any(missing(LoadDat), missing(Products), missing(Bands), missing(Size)))
          stop("Input was missing: LoadDat, Products, Bands, and Size are all required.")

        ## Populate the object fields upon initialisation
        self$inputData <- LoadDat
        self$products  <- Products
        self$bands     <- Bands
        self$size      <- Size

        if("SaveDir" %in% names(optionalInput))
        {
          self$saveDir <- normalizePath(optionalInput$SaveDir) ## Creates absolute path
        } else {
          cat("SaveDir not specified: default is the working directory.\n")
          self$saveDir <- getwd()
        }

        if("Transect" %in% names(optionalInput)) self$transect <- optionalInput$Transect

        ## Print the opening text output detailing the MODISSubsets function call
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
                 error = function(e) stop(simpleError("LoadDat does not contain all required variables: lat, long, start.date, end.date.")))

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
        productList <- c("MCD12Q1","MCD12Q2","MCD43A1","MCD43A2","MCD43A4","MOD09A1","MOD11A2","MOD13Q1","MOD15A2",
                         "MOD15A2GFS","MOD16A2","MOD17A2_51","MOD17A3","MYD09A1","MYD11A2","MYD13Q1","MYD15A2")
        tryCatch(stopifnot(all(self$products %in% productList)),
                 error = function(e) stop(simpleError("A named Product is not a MODIS product available through the web service.")))

        ## Check the Bands input are valid given the Products input.
        bandList <- do.call(rbind, lapply(self$products, function(x) data.frame(product = I(x), band = I(GetBands(x)))))
        bandList <- bandList[bandList$band %in% self$bands, ]           ## From list of relevant bands, extract ones in input.
        tryCatch(stopifnot(all(self$bands %in% bandList$band),          ## Are all input Bands found in available bands?
                           all(self$products %in% bandList$product)),   ## Are all input Products used to get Bands?
                 error = function(e) stop(simpleError("Bands and Products input do not fully match.")))

        ## Check SaveDir matches an existing directory.
        tryCatch(stopifnot(file.exists(self$saveDir)),
                 error = function(e) stop(simpleError("Input for SaveDir does not resemble an existing file path.")))
      },
      ##
      subsetClean = function()
      {
        ## Take input dataset and remove all rows that are incomplete or duplicated time series.
        self$inputData <- subset(self$inputData,
                                 !is.na(lat) | !is.na(long) | !is.na(start.date) | !is.na(end.date))
        self$inputData <- subset(self$inputData,
                                 !duplicated(data.frame(lat, long, start.date, end.date)))

        cat("Found", nrow(self$inputData), "unique time series to download.\n")

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
                 startDates <- as.Date(paste0(self$inputData$start.date, "-01-01"))
                 endDates   <- as.Date(paste0(self$inputData$end.date, "-12-31"))
               })

        ## Format days information for MODIS-style dates (A + YYYYDDD).
        modisDates <- data.frame(start = I(private$posixToModisDates(startDates)),
                                 end   = I(private$posixToModisDates(endDates)))

        ## Store available dates then check start and end dates fall within the available range.
        self$dateList <- lapply(self$products,
                                function(x) GetDates(self$inputData$lat[1], self$inputData$long[1], x))

        tryCatch(stopifnot(all(startDates >= min(private$modisToPosixDates(unlist(self$dateList)))),
                           all(endDates <= max(private$modisToPosixDates(unlist(self$dateList))))),
                 error = function(e) stop(simpleError("Some dates requested fall outside the available range.")))

        ## Return dates in MODIS format.
        return(modisDates)
      }
    ),

    private = list(
      ##### Private methods
      requestMessage = function() cat("Files will be written to ", self$saveDir, ".\n", sep = ''),
      ##
      createID = function()
      {
        ## Check whether a variable resembling unique IDs already exists. If not, make one.
        whichVarAreIDs <- which(grepl("ID", names(self$inputData)))
        whichVarAreIDs <- if(length(whichVarAreIDs) > 1) whichVarAreIDs[1] ## If several variables resemble IDs use the first one.
        numberOfUniqueIDs <- length(unique(self$inputData[ ,whichVarAreIDs]))

        if(numberOfUniqueIDs == nrow(self$inputData))
        {
          cat("Unique subset IDs found in variable: ", names(self$inputData)[whichVarAreIDs], ".\n", sep = '')

          ## Three underscores used as placemarker to find ID in strings, so cannot exist within IDs.
          if(any(grepl("___", self$inputData[whichVarAreIDs]))) stop("IDs can not contain '___'.")

          ## Create new ID variable, at first column in the data.frame, named subsetID for convenience.
          self$inputData <- data.frame(subsetID = self$inputData[ ,whichVarAreIDs], self$inputData)
        } else {
          cat("No unique subset ID variable found so one will be created.\n")

          newID <- with(self$inputData,
                        paste0("Lat", sprintf('%.5f', lat), "Lon", sprintf('%.5f', long),
                               "Start", start.date,         "End", end.date))
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

        if(class(posixCompatible) != "try-error")
        {
          posix <- TRUE
        } else {
          yearCompatible <- try(as.numeric(allDates), silent = TRUE)
          ifelse(class(yearCompatible) != "try-error" & all(nchar(allDates) == 4),
                 year <- TRUE,
                 stop("Dates in LoadDat are recognised in neither year or POSIXt format."))
        }

        ## Return either "year" or "posix", which will be passed to switch() in modis dates method.
        if(posix)     return("posix")
        else if(year) return("year")
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
      }
      #checkDownloadSuccess = function()
      #                       {
      #                           return(TRUE)
      #                       },
      #writeSummaryFile = function()
      #                   {
      #                       return(TRUE)
      #                   }
    )
)
