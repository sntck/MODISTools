UpdateSubsets <-
function(LoadDat, StartDate = FALSE, Dir = ".")
{
	if(StartDate) details <- LoadDat[!duplicated(data.frame(LoadDat$lat, LoadDat$long, LoadDat$end.date, LoadDat$start.date)), ]
  if(!StartDate) details <- LoadDat[!duplicated(data.frame(LoadDat$lat, LoadDat$long, LoadDat$end.date)), ]
	cat("Found", nrow(details), "unique time-series in original file\n")

	# Year or posixt date format?
	Year <- FALSE
	POSIXt <- FALSE
	posix.compatible <- try(as.POSIXlt(details$end.date), silent = TRUE)
	if(any(class(details$end.date) == "POSIXt") | all(class(posix.compatible) != "try-error")) POSIXt <- TRUE
	if(all(is.numeric(details$end.date) & nchar(details$end.date) == 4) &
	     any(class(posix.compatible) == "try-error")) Year <- TRUE
	if(!Year & !POSIXt) stop("Date information in LoadDat is not recognised as years or as POSIXt format.")
	if(Year & POSIXt) stop("Date information in LoadDat is recognised as both year and POSIXt formats.")

	if(Year) endyear <- details$end.date
	if(POSIXt) endyear <- as.numeric(format(details$end.date, "%Y"))
	if(StartDate){
		if(Year) startyear <- details$start.date
		if(POSIXt) startyear <- as.numeric(format(details$start.date, "%Y"))
	}

	#ID <- ifelse(any(names(details) == "ID"), TRUE, FALSE)
	#fmt <- '%.5f'
	#if(StartDate){
  #  if(ID){
  #    ## Check that all author-given IDs will be unique for each unique time-series, and check that they won't cause issues with product information
  #    n.unique <- length(unique(details$ID)) == nrow(details)
  #    if(!n.unique){
  #      cat('Number of IDs is not unique.\n')
  #      details$ID <- paste("Lat", sprintf(fmt, details$lat), "Lon", sprintf(fmt, details$long), "Start", startyear, "End", endyear, sep = "")
  #    }
  #  } else {
  #    details$ID <- paste("Lat", sprintf(fmt, details$lat), "Lon", sprintf(fmt, details$long), "Start", startyear, "End", endyear, sep = "")
  #  }
	#}
	#if(!StartDate){
  #  if(ID){
  #    ## Check that all author-given IDs will be unique for each unique time-series, and check that they won't cause issues with product information
  #    n.unique <- length(unique(details$ID)) == nrow(details)
  #    if(!n.unique){
  #      details$ID <- paste("Lat", sprintf(fmt, details$lat), "Lon", sprintf(fmt, details$long), "End", endyear, sep = "")
  #    }
  #  } else {
  #    details$ID <- paste("Lat", sprintf(fmt, details$lat), "Lon", sprintf(fmt, details$long), "End", endyear, sep = "")
	#	}
	#}

	filelist <- list.files(path = Dir, pattern = ".asc")
	cat("Found", length(filelist), "subsets previously downloaded\n")
	whichSubsetsDownloaded <- c()

  for(file in filelist)
  {
    dataFile <- read.csv(file.path(Dir, file), as.is = TRUE, header = FALSE)

    dataLat <- substr(dataFile[1,9],
                      regexpr("Lat", dataFile[1,9])+3,
                      regexpr("Lon", dataFile[1,9])-1)
    dataLong <- substr(dataFile[1,9],
                       regexpr("Lon", dataFile[1,9])+3,
                       regexpr("Samp", dataFile[1,9])-1)

    startModisDate <- dataFile[1,8]
    endModisDate <- dataFile[nrow(dataFile),8]

    startYears <- substr(startModisDate, 2, 5)
    startDays  <- substr(startModisDate, 6, 8)
    startPosixDate <- as.Date(paste0(startYears, "-01-01")) + (as.numeric(startDays) - 1)

    endYears <- substr(endModisDate, 2, 5)
    endDays  <- substr(endModisDate, 6, 8)
    endPosixDate <- as.Date(paste0(endYears, "-01-01")) + (as.numeric(endDays) - 1)

    if(Year)
    {
      subsetMetadata <- data.frame(lat = as.numeric(dataLat),
                                   long = as.numeric(dataLong),
                                   start.date = startYears,
                                   end.date = endYears)

      whichSubsetsDownloaded <- c(whichSubsetsDownloaded,
                                  with(subsetMetadata,
                                       which(sprintf("%.5f", lat) == sprintf("%.5f", details$lat) &
                                             sprintf("%.5f", long) == sprintf("%.5f", details$long) &
                                             start.date == details$start.date &
                                             end.date == details$end.date)))
    }
    if(POSIXt)
    {
      subsetMetadata <- data.frame(lat = as.numeric(dataLat),
                                   long = as.numeric(dataLong),
                                   start.date = startPosixDate,
                                   end.date = endPosixDate)

      ## Find the interval length for the downloaded data band, to set the
      ## flexibility allowed when matching subset dates.
      secondDate <- dataFile[2,8]
      secondYears <- substr(secondDate, 2, 5)
      secondDays  <- substr(secondDate, 6, 8)
      secondDate <- as.Date(paste0(secondYears, "-01-01")) + (as.numeric(secondDays) - 1)
      intervalLength <- as.numeric(secondDate - startPosixDate)

      whichSubsetsDownloaded <- c(whichSubsetsDownloaded,
                                  with(subsetMetadata,
                                       which(sprintf("%.5f", lat) == sprintf("%.5f", details$lat) &
                                             sprintf("%.5f", long) == sprintf("%.5f", details$long) &
                                             (as.Date(details$start.date) <= start.date & start.date < as.Date(details$start.date)+intervalLength) &
                                             (end.date <= as.Date(details$end.date) & as.Date(details$start.date) < end.date+intervalLength))))
    }
  }

	return(details[-whichSubsetsDownloaded, ])
}