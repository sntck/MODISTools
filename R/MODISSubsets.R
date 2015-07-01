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

    ## Use dates in request$inputData to create dates in MODIS format that will be passed to web service methods.
    modisDates <- request$createModisDates()

    ##### Retrieve data subsets for each time-series of a set of product bands, saving data for each time series into ASCII files.
    request$inputData <- BatchDownload(lat.long = request$inputData, dates = request$dateList,
                                       MODIS.start = modisDates$start, MODIS.end = modisDates$end,
                                       Bands = request$bands, Products = request$products,
                                       Size = request$size, StartDate = StartDate,
                                       Transect = request$transect, SaveDir = request$saveDir)

    # Run a second round of downloads for any time-series that incompletely downloaded, and overwrite originals.
    success.check <- request$inputData$Status != "Successful download"
    if(any(success.check)){
      cat("Some subsets that were downloaded were incomplete. Retrying download again for these time-series...\n")

      request$inputData[success.check, ] <- BatchDownload(lat.long = request$inputData[success.check, ], dates = request$dateList,
                                                          MODIS.start = modisDates$start, MODIS.end = modisDates$end,
                                                          Bands = request$bands, Products = request$products,
                                                          Size = request$size, StartDate = StartDate, Transect = request$transect,
                                                          SaveDir = request$saveDir)

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