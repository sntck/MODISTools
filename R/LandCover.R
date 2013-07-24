LandCover <- 
function(Dir=".", Band)
{ 
  # Define land cover classes for each lc band,
  LC_CLASS <- list(
    Land_Cover_Type_1 = c("Water"=0, "Evergreen Needleleaf forest"=1, "Evergreen Broadleaf forest"=2, "Deciduous Needleleaf forest"=3,
                          "Deciduous Broadleaf forest"=4, "Mixed forest"=5, "Closed shrublands"=6, "Open shrublands"=7, "Woody savannas"=8,
                          "Savannas"=9, "Grasslands"=10, "Permanent wetlands"=11, "Croplands"=12, "Urban & built-up"=13, "Cropland/Natural vegetation mosaic"=14,
                          "Snow & ice"=15, "Barren/Sparsely vegetated"=16, "Unclassified"=254, "NoDataFill"=255),
    Land_Cover_Type_2 = c("Water"=0, "Evergreen Needleleaf forest"=1, "Evergreen Broadleaf forest"=2, "Deciduous Needleleaf forest"=3,
                          "Deciduous Broadleaf forest"=4, "Mixed forest"=5, "Closed shrublands"=6, "Open shrublands"=7, "Woody savannas"=8,
                          "Savannas"=9, "Grasslands"=10, "Croplands"=12, "Urban & built-up"=13, "Barren/Sparsely vegetated"=16, "Unclassified"=254, "NoDataFill"=255),
    Land_Cover_Type_3 = c("Water"=0, "Grasses/Cereal crops"=1, "Shrubs"=2, "Broadleaf crops"=3, "Savanna"=4, "Evergreen Broadleaf forest"=5,
                          "Deciduous Broadleaf forest"=6, "Evergreen Needleleaf forest"=7, "Deciduous Needleleaf forest"=8, "Non-vegetated"=9,
                          "Urban"=10, "Unclassified"=254, "NoDataFill"=255),
    Land_Cover_Type_4 = c("Water"=0, "Evergreen Needleleaf forest"=1, "Evergreen Broadleaf forest"=2, "Deciduous Needleleaf forest"=3,
                          "Deciduous Broadleaf forest"=4, "Annual Broadleaf vegetation"=5, "Annual grass vegetation"=6,
                          "Non-vegetated land"=7, "Urban"=8, "Unclassified"=254, "NoDataFill"=255),
    Land_Cover_Type_5 = c("Water"=0, "Evergreen Needleleaf forest"=1, "Evergreen Broadleaf forest"=2, "Deciduous Needleleaf forest"=3,
                          "Deciduous Broadleaf forest"=4, "Shrub"=5, "Grass"=6, "Cereal crop"=7, "Broadleaf crop"=8, "Urban & built-up"=9,
                          "Snow & ice"=10, "Barren/Sparsely vegetated"=11, "Unclassified"=254, "NoDataFill"=255)
  )
  #####
  
  if(!file.exists(Dir)){
    stop("Character string input for Dir argument does not resemble an existing file path.")
  }
  
  file.list <- list.files(pattern=".asc", path=Dir)
  if(length(file.list) == 0){
    stop("Found no ascii files in Dir. The function wants downloaded MODIS Land Cover .asc files.")
  }
  if(any(!grepl("MCD12Q1", file.list))){
    print("Some .asc files found in Dir that weren't MODIS downloads of MCD12Q1 product. Selecting only ascii files from MCD12Q1 downloads.")
    file.list <- file.list[which(grepl("MCD12Q1", file.list))]
  }
  if(!any(GetBands("MCD12Q1") == Band)){
    stop("This function is for land cover data. Downloaded files should be for the MCD12Q1 product.
         Band specified is not for this product.")
  }
  #####
  
  lc.type.set <- LC_CLASS[[which(names(LC_CLASS) == Band)]]
  NoDataFill <- unname(lc.type.set["NoDataFill"])
  # Check that band values all fall within the ValidRange (as defined for given MODIS product band).
  ValidRange <- unname(lc.type.set)
  
  lc.summary <- list(NA)
  for(i in 1:length(file.list)){
    lc.subset <- read.csv(paste(Dir, "/", file.list[i], sep=""), header=FALSE, as.is=TRUE)
    names(lc.subset) <- c("row.id", "land.product.code", "MODIS.acq.date", "where", "MODIS.proc.date", 
                          1:(ncol(lc.subset) - 5))
    print(paste("Processing file ", i, " of ", length(file.list), "...", sep=""))
    
    where.long <- regexpr("Lon", lc.subset$where[1])
    where.samp <- regexpr("Samp", lc.subset$where[1])
    where.land <- regexpr("Land", lc.subset$row.id)
    lat <- as.numeric(substr(lc.subset$where[1], 4, where.long - 1))
    long <- as.numeric(substr(lc.subset$where[1], where.long + 3, where.samp - 1))
    band.codes <- substr(lc.subset$row.id, where.land, nchar(lc.subset$row.id))
    
    if(any(grepl(Band, lc.subset$row.id))){
      which.are.band <- which(band.codes == Band)
    } else {
      stop("Cannot find which rows in LoadDat are band data. Make sure the only ascii files in the directory are 
             those downloaded from MODISSubsets.")
    }
    
    lc.tiles <- as.matrix(lc.subset[which.are.band,6:ncol(lc.subset)], nrow=length(which.are.band), ncol=length(6:ncol(lc.subset)))

    if(!all(lc.tiles %in% ValidRange)){
      stop("Some values fall outside the valid range for the data band specified.") 
    }
    
    # Screen the pixel values in lc.tiles: any pixels whose value correspond to NoDataFill, or whose
    # corresponding pixel in qc.tiles is below QualityThreshold, will be replaced with NA so they aren't included.
    lc.tiles <- matrix(ifelse(lc.tiles != NoDataFill, lc.tiles, NA), nrow=length(which.are.band))
    
    # Extract year and day from the metadata and make POSIXlt dates (YYYY-MM-DD), ready for time-series analysis.
    lc.subset$Year <- as.numeric(substr(lc.subset$MODIS.acq.date, 2, 5))
    lc.subset$Day <- as.numeric(substr(lc.subset$MODIS.acq.date, 6, 8))
    lc.subset$date <- strptime(paste(lc.subset$Year, "-", lc.subset$Day, sep=""), "%Y-%j")
    
    # Initialise objects to store landscape summaries
    lc.mode.class <- rep(NA, nrow(lc.tiles))
    lc.richness <- rep(NA, nrow(lc.tiles))
    simp.even <- rep(NA, nrow(lc.tiles))
    simp.d <- rep(NA, nrow(lc.tiles))
    no.fill <- rep(NA, nrow(lc.tiles))
    poor.quality <- rep(NA, nrow(lc.tiles))
    
    for(x in 1:nrow(lc.tiles)){
      # Calculate mode - most frequent lc class
      lc.freq <- table(lc.tiles[x, ])
      lc.freq <- lc.freq / ncol(lc.tiles)
      lc.freq <- sum(lc.freq^2)
      # Calculate Simpson's D diversity index 
      simp.d[x] <- 1 / lc.freq
      
      lc.mode <- which.max(table(lc.tiles[x, ]))
      lc.mode.class[x] <- names(which(lc.type.set == lc.mode))     
      # Calculate landscape richness
      lc.richness[x] <- length(table(lc.tiles[x, ]))   
          
      # Calculate Simpson's measure of evenness
      simp.even[x] <- simp.d[x] / lc.richness[x]
      
      no.fill[x] <- paste(round((sum(lc.subset[x,6:ncol(lc.subset)] == NoDataFill) / length(lc.tiles[x, ])) * 100, 2), "% (",
                         sum(lc.subset[x,6:ncol(lc.subset)] == NoDataFill), "/", length(lc.tiles[x, ]), ")", sep="") 
    } # End of loop that summaries tiles at each time-step, for the ith ascii file.
    
    # Compile summaries into a table.
    lc.summary[[i]] <- data.frame(lat=lat, long=long, date=lc.subset$date[which(band.codes == Band)],
                                  modis.band=Band, most.common=lc.mode.class, richness=lc.richness, simpsons.d=simp.d,
                                  simpsons.evenness=simp.even, no.data.fill=no.fill)
  } # End of loop that reiterates for each ascii file.
  
  # Write output summary file by appending summary data from all files, producing one file of summary output.
  write.table(lc.summary[[1]], file=paste(Dir, "/", "MODIS Land Cover Summary ", Sys.Date(), ".csv", sep=""),
              sep=",", row.names=FALSE)
  if(length(file.list) > 1){ 
    for(i in 2:length(file.list)){ 
      write.table(lc.summary[[i]], file=paste(Dir, "/", "MODIS Land Cover Summary ", Sys.Date(), ".csv", sep=""), 
                  sep=",", append=TRUE, row.names=FALSE, col.names=FALSE) 
    } 
  }
  print(paste("Done! Check the 'MODIS Land Cover Summary' output file.", sep=""))
}