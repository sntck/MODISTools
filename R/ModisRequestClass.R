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
          cat("SaveDir input missing. Files will be written to the working directory.\n")
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

        cat("Found", nrow(self$inputData), "unique time-series to download.\n")
      }
    ),

    private = list(
      ##### Private methods
      requestMessage = function() cat("Files downloaded will be written to ", self$saveDir, ".\n", sep = '')#,
      #createModisDates = function()
      #                   {
      #                       return(TRUE)
      #                   },
      #createUniqueID = function()
      #                 {
      #                     return(TRUE)
      #                 },
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
