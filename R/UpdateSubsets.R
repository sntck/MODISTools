UpdateSubsets <-
function(LoadDat, Dir = ".")
{

	end.date <- LoadDat$end.date
	
	ifelse(class(end.date) == "POSIXlt",
         year <- end.date$year + 1900,
         year <- end.date)
	
	all.subsets <- unique(paste(LoadDat$lat, LoadDat$long, year))
	
	filelist <- list.files(path = Dir, pattern = ".asc")
	downloaded <- c()
	
	for(count in 1:length(filelist)){
    
		ds <- read.csv(filelist[count], header = FALSE)
		names(ds) <- c("row.id", "land.product.code", "MODIS.acq.date", "where", "MODIS.proc.date", 1:(ncol(ds) - 5))
    
		wS <- regexpr("Samp", ds$where[1]) ## number of rows according to number of pixels
		wlong <- regexpr("Lon", ds$where[1])
    
    lat <- as.numeric(substr(ds$where[1], 4, wlong - 1)) 
    long <- as.numeric(substr(ds$where[1], wlong + 3, wS - 1))
    dsyear <- as.numeric(substr(ds$MODIS.acq.date, 2, 5))
    
    endyear <- max(dsyear)
    downloaded[count] <- paste(lat, long, endyear)
    
	} ## End of filelist count.
	
	revised.subsets <- LoadDat[which(all.subsets %in% downloaded == FALSE), ]
	
	return(revised.subsets)
	
}