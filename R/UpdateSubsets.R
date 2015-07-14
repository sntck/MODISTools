UpdateSubsets <-
function(LoadDat, Dir = '.', StartDate = TRUE)
{
	details <- LoadDat[!duplicated(data.frame(LoadDat$lat, LoadDat$long, LoadDat$end.date, LoadDat$start.date)), ]
	cat("Found", nrow(details), "unique time-series in original file\n")

	## year or posixt date format?
	year <- FALSE
	posix <- FALSE
	posixCompatible <- try(as.POSIXlt(details$end.date), silent = TRUE)
	if(any(class(details$end.date) == "POSIXlt") | all(class(posixCompatible) != "try-error")) posix <- TRUE
	if(all(is.numeric(details$end.date) & nchar(details$end.date) == 4) & any(class(posixCompatible) == "try-error")) year <- TRUE

  if(!year & !posix)
    stop("Date information in LoadDat is not recognised as years or as posix format.")
	if(year & posix)
    stop("Date information in LoadDat is recognised as both year and posix formats.")

	if(year) endYear <- details$end.date
	if(posix) endYear <- as.numeric(format(details$end.date, "%Y"))
	if(StartDate){
		if(year) startYear <- details$start.date
		if(posix) startYear <- as.numeric(format(details$start.date, "%Y"))
	}

	## Check that all author-given IDs are unique for each subset.
	if("ID" %in% names(details)){
	  allSubsetsAreUnique <- length(unique(details$ID)) == nrow(details)
	  if(!allSubsetsAreUnique){
	    cat("Not all subset IDs are unique.\n")
	    details$ID <- paste0("Lat", sprintf("%.5f", details$lat), "Lon", sprintf("%.5f", details$long), "Start", startYear, "End", endYear)
	  }
	} else {
	  details$ID <- paste0("Lat", sprintf("%.5f", details$lat), "Lon", sprintf("%.5f", details$long), "Start", startYear, "End", endYear)
	}

	fileList <- list.files(path = Dir, pattern = ".asc")
	cat("Found", length(fileList), "subsets previously downloaded\n")

	whereAreIds <- regexpr("___", fileList)
	downloadedSubsetIds <- substr(fileList, 1, whereAreIds - 1)

	revisedSubsets <- details[which(!(details$ID %in% downloadedSubsetIds)), ]
	return(revisedSubsets)
}