MODISSubsets <-
function(LoadDat, Products, Bands, Size, StartDate = FALSE, TimeSeriesLength = 0, ...)
{
    ## Retrieve list of optional arguments.
    optionalInput <- list(...)

    ## Instantiate an object of class ModisRequest using input arguments from MODISSubsets call.
    request <- ModisRequest$new(mget(ls()))

    ## Perform defensive checks on input.
    request$validateInput()

    ## Clean input dataset so that each time series is complete and unique.
    request$subsetClean()

    ##### Year or posixt date format?
    Year <- FALSE
    POSIXt <- FALSE

    posix.compatible <- try(as.POSIXlt(request$inputData$end.date), silent = TRUE)

    if(any(class(request$inputData$end.date) == "POSIXt") | all(class(posix.compatible) != "try-error")) POSIXt <- TRUE
    if(all(is.numeric(request$inputData$end.date) & nchar(request$inputData$end.date) == 4) &
         any(class(posix.compatible) == "try-error")) Year <- TRUE

    if(!Year & !POSIXt) stop("Date information in LoadDat is not recognised as years or as POSIXt format.")
    if(Year & POSIXt) stop("Date information in LoadDat is recognised as both year and POSIXt formats.")

    # Take date information for each time-series, in 'year' or 'posixt', and turn them into MODIS date codes (Julian).
    if(Year){
      if(StartDate){
        start.year.fail <- any(!is.numeric(request$inputData$start.date) | nchar(request$inputData$start.date) != 4)
        if(start.year.fail) stop("end.date identified as year dates, but start.date does not match.")

        start.date <- strptime(paste(request$inputData$start.date, "-01-01", sep = ""), "%Y-%m-%d")
      }
      if(!StartDate) start.date <- strptime(paste(request$inputData$end.date - TimeSeriesLength, "-01-01", sep = ""), "%Y-%m-%d")

      # Put start and end dates in POSIXlt format.
      end.date <- strptime(paste(request$inputData$end.date, "-12-31", sep = ""), "%Y-%m-%d")
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
      end.date <- strptime(request$inputData$end.date, "%Y-%m-%d")

      if(StartDate){
        start.posix.fail <- any(class(try(as.POSIXlt(request$inputData$end.date), silent = TRUE)) == "try-error")
        if(start.posix.fail) stop("end.date identified as POSIXt dates, but start.date does not match.")

        start.date <- strptime(request$inputData$start.date, "%Y-%m-%d")
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
    # Retrieve the list of date codes to be requested and organise them in batches of time series's of length 10.
    dates <- lapply(request$products, function(x) GetDates(request$inputData$lat[1], request$inputData$long[1], x))

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
    request$inputData <- BatchDownload(lat.long = request$inputData, dates = dates, MODIS.start = MODIS.start, MODIS.end = MODIS.end, Bands = request$bands,
                              Products = request$products, Size = request$size, StartDate = StartDate, Transect = request$transect, SaveDir = request$saveDir)

    # Run a second round of downloads for any time-series that incompletely downloaded, and overwrite originals.
    success.check <- request$inputData$Status != "Successful download"
    if(any(success.check)){
      cat("Some subsets that were downloaded were incomplete. Retrying download again for these time-series...\n")

      request$inputData[success.check, ] <- BatchDownload(lat.long = request$inputData[success.check, ], dates = dates, MODIS.start = MODIS.start,
                                                 MODIS.end = MODIS.end, Bands = request$bands, Products = request$products, Size = request$size,
                                                 StartDate = StartDate, Transect = request$transect, SaveDir = request$saveDir)

      success.check <- request$inputData$Status != "Successful download"
      if(any(success.check)) cat("Incomplete downloads were re-tried but incomplete downloads remain. See subset download file.\n")
    }
    #####

    ##### Write a summary file with IDs and unique time-series information.
    date <- as.POSIXlt(Sys.time())
    file.date <- paste(as.Date(date),
                       paste(paste0("h", date$hour), paste0("m", date$min), paste0("s", round(date$sec, digits=0)), sep = "-"),
                       sep = "_")
    if(!request$transect){
      write.table(request$inputData, file = file.path(request$saveDir, paste0("SubsetDownload_", file.date, ".csv")),
                  col.names = TRUE, row.names = FALSE, sep = ",")
    }
    if(request$transect){
      DirList <- list.files(path = request$saveDir)
      w.transect <- regexpr("Point", request$inputData$ID[1])
      transect.id <- substr(request$inputData$ID[1], 1, w.transect - 1)

      if(!any(DirList == file.path(request$saveDir, paste0(transect.id, "_SubsetDownload_", file.date, ".csv")))){
        write.table(request$inputData, file = file.path(request$saveDir, paste0(transect.id, "_SubsetDownload_", file.date, ".csv")),
                    col.names = TRUE, row.names = FALSE, sep = ",")
      } else {
        write.table(request$inputData, file = file.path(request$saveDir, paste0(transect.id, "_SubsetDownload_", file.date, ".csv")),
                    col.names = FALSE, row.names = FALSE, sep = ",", append = TRUE)
      }
    }
    #####

    # Print message to confirm downloads are complete and to remind the user to check summary file for any missing data.
    if(!request$transect) cat("Done! Check the subset download file for correct subset information and download messages.\n")
}