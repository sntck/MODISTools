MODISSubsets <-
function(LoadDat, Products, Bands, Size, ...)
{
    ## Retrieve list of optional arguments.
    optionalInput <- list(...)

    ## Instantiate an object of class ModisRequest using input arguments from MODISSubsets call.
    request <- ModisRequest$new(mget(ls()))

    ## Perform defensive checks on input.
    request$validateInput()

    ## Clean input dataset so that each time series is complete and unique.
    request$subsetClean()

    ## Check whether any subsets have already been downloaded before beginning subset request.
    newSubsets <- UpdateSubsets(request$inputData, request$saveDir, StartDate = TRUE) #Remove StartDate once OOP dev complete
    if(nrow(newSubsets) < nrow(request$inputData))
    {
      cat(nrow(request$inputData)-nrow(newSubsets)," previously downloaded subsets were found in ",request$saveDir,"\n",
          "These subsets will be ignored during this request.\n",
          "The number of unique subsets to download is now ",nrow(newSubsets),".\n", sep='')
      request$inputData <- newSubsets
    }

    ## Use dates in request$inputData to create dates in MODIS format that will be passed to web service methods.
    modisDates <- request$createModisDates()

    ## Loop the downloading over each subset.
    for(i in 1:nrow(request$inputData))
    {
      cat("Getting subset for location ",i," of ",nrow(request$inputData),"...\n", sep='')

      ## Extract the available dates within the requested range and organise for download.
      request$dateList <- request$prepareDatesForDownload(start = modisDates$start, end = modisDates$end)
      names(request$dateList) <- request$products

      ## Create list object that will store subset downloads. Each list element is a product, with length time-series*bands.
      subset <- mapply(function(subsetDates, subsetProduct)
      {
        numBands <- nrow(subset(request$bandList, product == subsetProduct))
        rep(NA, length = sum(!is.na(subsetDates)) * numBands)
      },
      subsetDates = request$dateList, subsetProduct = request$products, SIMPLIFY = FALSE)

      ## Get time series of all MODIS data bands for this subset.
      subset <- request$subsetDownload(subset, subsetID = i)

      ## Check if any data are missing, log download status accordingly, and retry download if necessary.
      request$checkDownloadSuccess(subset, subsetID = i)

      fileName <- paste0(request$inputData$subsetID[i],"___",paste(request$products, collapse='_'),".asc")
      write(subset, file = file.path(request$saveDir, fileName), sep = '', append = TRUE)
    }

    ## Write a summary file with IDs and unique time-series information.
    request$writeSummaryFile()

    ## Print a message to confirm downloads are complete and a reminder to check the summary file for any missing data.
    if(!request$transect) cat("Done! Check the subset download file to verify subset information and download messages.\n")
}