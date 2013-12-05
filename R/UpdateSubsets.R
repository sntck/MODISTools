## 15th August 2013
## Helen Phillips

## Working direction is where original pixels were downloaded
## LoadDat would need a lat, long, end.date column.



UpdateSubsets <- function(LoadDat, Dir = ".")
{
	
	## if end.date is in POSIXlt format
	
	end.date <- LoadDat$end.date
	
	if(class(end.date) == "POSIXlt"){
		year <- end.date$year + 1900
	}else{year <- end.date }
	
	all.subsets <- unique(paste(LoadDat$lat, LoadDat$long, year))
	
	filelist <- list.files(path = Dir, pattern = ".asc") ## list of MODIS pixels already downloaded
	downloaded <- c()
	
	for (count in 1:length(filelist)) ## for each file collects lat, long and end year of pixels downloaded
	{
		ds <- read.csv(filelist[count], header = FALSE)
		names(ds) <- c("row.id", "land.product.code", "MODIS.acq.date", "where", "MODIS.proc.date", 1:(ncol(ds) - 5))
		wS <- regexpr("Samp", ds$where[1]) ## number of rows according to number of pixels
		wlong <- regexpr("Lon", ds$where[1])
    	lat <- as.numeric(substr(ds$where[1], 4, wlong - 1)) 
    	long <- as.numeric(substr(ds$where[1], wlong + 3, wS - 1))
    	dsyear <- as.numeric(substr(ds$MODIS.acq.date, 2, 5))
    
    	endyear <- max(dsyear)
    	downloaded[count] <- paste(lat, long, endyear)
	} ## end of filelist count
	
	revised.subsets <- LoadDat[which(all.subsets %in% downloaded == FALSE),] ## creates a subset of the LoadDat which has not already been downloaded
	
	return(revised.subsets)
	
} ## end of function