UpdateSubsets <- function(LoadDat, StartDate = FALSE, Dir = ".")
{

	if(StartDate) details <- LoadDat[!duplicated(data.frame(LoadDat$lat, LoadDat$long, LoadDat$end.date, LoadDat$start.date)), ]
    if(!StartDate) details <- LoadDat[!duplicated(data.frame(LoadDat$lat, LoadDat$long, LoadDat$end.date)), ]
	cat("Found", nrow(details), "unique time-series in original file\n") 
	
	
	## If date of LoadDat is only the year
	if(nchar(details$end.date[1]) == 4){ endyear <- details$end.date}
	## If date of LoadDat is in POSIX format
	if(nchar(details$end.date[1]) != 4){ endyear <- as.numeric(format(details$end.date, "%Y"))}
	
	if(StartDate){
		if(nchar(details$start.date) == 4){startyear <- details$start.date}
		if(nchar(details$start.date) != 4){startyear <- as.numeric(format(details$start.date, "%Y"))}
	}
			
	if(!StartDate) all.subsets <- paste(details$lat, details$long, endyear)
	if(StartDate) all.subsets <- paste(details$lat, details$long, startyear, endyear)
	
	filelist <- list.files(path = Dir, pattern = ".asc")
	cat("Found", length(filelist), "subsets previously downloaded\n")
	downloaded <- c()
	
	for(count in 1:length(filelist)){
    
		ds <- read.csv(filelist[count], header = FALSE)
		names(ds) <- c("row.id", "land.product.code", "MODIS.acq.date", "where", "MODIS.proc.date", 1:(ncol(ds) - 5))
    
		wS <- regexpr("Samp", ds$where[1]) ## number of rows according to number of pixels
		wlong <- regexpr("Lon", ds$where[1])
    
    lat <- as.numeric(substr(ds$where[1], 4, wlong - 1)) 
    long <- as.numeric(substr(ds$where[1], wlong + 3, wS - 1))
    dsyear <- as.numeric(substr(ds$MODIS.acq.date, 2, 5))
    
    dsendyear <- max(dsyear, na.rm = TRUE)
    if(StartDate) dsstartyear <- min(dsyear, na.rm = TRUE)
    
    if(!StartDate) downloaded[count] <- paste(lat, long, dsendyear)
    if(StartDate) downloaded[count] <- paste(lat, long, dsstartyear, dsendyear)
    
	} ## End of filelist count.
	
	revised.subsets <- LoadDat[which(all.subsets %in% downloaded == FALSE), ]
	
	return(revised.subsets)
	
}