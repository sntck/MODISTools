MODISTransects <-
  function(LoadData, LoadMethod='object' | 'ext.file', FileSep=NULL, Product, Bands, Size=c(), SaveDir="./", StartDate=FALSE, TimeSeriesLength=2, DateFormat='year' | 'posixt')
  {
    # Define:  
    # Data are gridded in equal-area tiles in a sinusoidal projection. Each tile consists of a 1200x1200 km data 
    # array (MODPRJ_TILE_KM) of pixels at a finer resolution (see http://modis-land.gsfc.nasa.gov/MODLAND_grid.html).                          
    MODPRJ_TILE_M = 1200 * 1000                      # Define rounded width of each MODIS prj grid tile in metres.
    NUM_TILES = 36                                   # No. of tiles spanning horizontally along the MODIS mapping grid.
    MODPRJ_EXTENT_X = 20015109.354 + 20015109.354    # Absolute horizontal extent of grid (-20015109.354,20015109.354). 
    LONG_EQTR_M = 111.2 * 1000                       # A degree at equator is 111.2km in distance.
    
    # Load data of locations; external data file, or an R object.
    if(LoadMethod == 'object') { dat<- data.frame(LoadData) }
    if(LoadMethod == 'ext.file') { dat<- read.delim(LoadData, sep=FileSep) }
    
    # Load up Web Service Description Language and set up SOAP-client interface
    ornlMODIS = processWSDL('http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.wsdl')
    ornlMODISFuncs = genSOAPClientInterface(operations=ornlMODIS@operations[[1]], def=ornlMODIS)
    
    #####
    # Actual width of each tile in the MODIS projection (in metres).
    tile.width<- MODPRJ_EXTENT_X / NUM_TILES
    # Identify resolution of requested product band and use it to work out actual cell size from projection info.
    w.res<- regexpr('m_', Bands[1])
    res<- as.numeric(substr(Bands[1], 1, w.res-1))
    # Work out actual width of each pixel in the MODIS projection.
    cell.size<- tile.width / (MODPRJ_TILE_M/res)
    #####
    
    # Find all unique transects to download pixels for.
    t.dat<- dat[duplicated(dat$transect) == FALSE,]
    print(paste('Found ',nrow(t.dat),' transects. Downloading time-series sets for each transect...',sep=''))
    
    # Loop that reiterates download for each transect.
    for(i in 1:nrow(t.dat)) {
      
      # Find the distance, in decimal degrees between the start and end of the transect.
      delta.lat<- t.dat$end.lat[i]-t.dat$start.lat[i]
      delta.long<- round(t.dat$end.long[i]-t.dat$start.long[i], digits=5)
      # Work out how far in metres is the latitudinal difference between start and end locations.
      lat.metres<- delta.lat * LONG_EQTR_M
      # Determine the curvature angle from the latitude so the distance between one longitude at the transect location
      # can be calculated. The distance between longitudes depends on what latitude you are at. The latitude used to work
      # this out here is the middle of the transect, but unless the transect spans large latitudinal difference, it
      # doesn't matter.
      lat.rad<- median(c(t.dat$start.lat[i], t.dat$end.lat[i]))*(pi/180)
      # Work out how far in metres is the longitudinal difference between start and end locations.
      long.metres<- delta.long * (LONG_ETR_M * cos(lat.rad))
      # Work out the actual length of the transect.
      transect<- sqrt((lat.metres^2)+(long.metres^2))
      
      # Work out how many points can be equally spaced (i.e. how many pixels) between the start and end coordinates.
      num.points<- transect/cell.size
      # Work out the lat and long distances of the equally spaced points.
      lat.increment<- round(delta.lat/num.points, digits=5)
      long.increment<- round(delta.long/num.points, digits=5)
      lat<- t.dat$start.lat[i]
      long<- round(t.dat$start.long[i], digits=5)
      
      # Take the start lat and long and interpolate a new lat & long, using lat & long increments, until the end lat &
      # long. The following if statements decide how to interpolate the equally spaced coordinates, depending on the 
      # position of the start lat and long values relative to the end values (+ve or -ve difference).
      # Produces vector of lats and long for all pixels along transect for time-series information in MODISSubsets.
      if(lat.increment > 0) {
        if(long.increment > 0) {                         
          while(lat[length(lat)] <= (t.dat$end.lat[i]-lat.increment) & long[length(long)] <= 
                  (round(t.dat$end.long[i], digits=5)-long.increment)) {
            lat<- c(lat, round(lat[length(lat)] + lat.increment, digits=5))
            long<- c(long, round(long[length(long)] + long.increment, digits=5))
          }
        } else {
          while(lat[length(lat)] <= (t.dat$end.lat[i]-lat.increment) & long[length(long)] >= 
                  (round(t.dat$end.long[i], digits=5)-long.increment)) {
            lat<- c(lat, round(lat[length(lat)] + lat.increment, digits=5))
            long<- c(long, round(long[length(long)] + long.increment, digits=5))
          }
        }
      } else { 
        if(long.increment > 0) {
          while(lat[length(lat)] >= (t.dat$end.lat[i]-lat.increment) & long[length(long)] <= 
                  (round(t.dat$end.long[i], digits=5)-long.increment)) {
            lat<- c(lat, round(lat[length(lat)] + lat.increment, digits=5))
            long<- c(long, round(long[length(long)] + long.increment, digits=5))
          }
        } else {
          while(lat[length(lat)] >= (t.dat$end.lat[i]-lat.increment) & long[length(long)] >= 
                  (round(t.dat$end.long[i], digits=5)-long.increment)) {
            lat<- c(lat, round(lat[length(lat)] + lat.increment, digits=5))        
            long<- c(long, round(long[length(long)] + long.increment, digits=5))   
          }
        }
      } # End of if statements that correctly interpolate points along transect line.
      
      # Write vector of end dates & IDs of each transect point to be used for time-series information in MODISSubsets.
      end.date<- rep(t.dat$end.date[i], length(lat))
      ID<- paste('Transect',t.dat$transect[i],'Point',1:length(lat), sep='')
      
      # Organise time-series information, with new by-transect IDs for each pixel, for input into MODISSubsets call
      # with optional start date as well as end date.
      if(StartDate == TRUE) {
        start.date<- rep(t.dat$start.date[i], length(lat))                            
        t.subset<- data.frame(ID, lat, long, start.date, end.date)
      } else {
        t.subset<- data.frame(ID, lat, long, end.date)
      }     
      
      #####
      # Do some error checking of pixels in transect before requesting data in MODISSubsets function call.
      t.point<- list(NA)
      xll<- c()
      yll<- c()
      # Get the MODIS Projection coordinates for the pixel that each interpolated transect increment falls within.
      for(n in 1:nrow(t.subset)) {
        t.point[[n]]<- ornlMODISFuncs@functions$getsubset(t.subset$lat[n], t.subset$long[n], 'MOD13Q1', '250m_16_days_EVI', 'A2000049', 'A2000049', 0, 0)
        xll[n]<- t.point[[n]]@xllcorner
        yll[n]<- t.point[[n]]@yllcorner  
      }
      
      # Check which pixels have the same x or y as the previous pixel.
      check.equal.x<- signif(xll[1:length(xll)-1], digits=6) == signif(xll[2:length(xll)], digits=6)
      check.equal.y<- signif(yll[1:length(yll)-1], digits=5) == signif(yll[2:length(yll)], digits=5) 
      # From remaining pixels, check if they are +/- 1 pixel width (i.e. adjacent pixel) away.
      check.new.x<- ifelse(xll[which(check.equal.x == FALSE)] < xll[which(check.equal.x == FALSE)+1], 
                           round(xll[which(check.equal.x == FALSE)]) == round(xll[which(check.equal.x == FALSE)+1]-cell.size), 
                           round(xll[which(check.equal.x == FALSE)]) == round(xll[which(check.equal.x == FALSE)+1]+cell.size))
      check.new.y<- ifelse(yll[which(check.equal.y == FALSE)] < yll[which(check.equal.y == FALSE)+1], 
                           round(yll[which(check.equal.y == FALSE)]) == round(yll[which(check.equal.y == FALSE)+1]-cell.size), 
                           round(yll[which(check.equal.y == FALSE)]) == round(yll[which(check.equal.y == FALSE)+1]+cell.size))    
      
      # Check if there are any remaining pixels whose distance from previous pixel doesn't meet above criteria.
      if(any(check.new.x == FALSE) | any(check.new.y == FALSE) == TRUE) {
        # Check if this unaccounted for distance between pixels is small enough that it can be attributed to rounding
        # error or MODIS projection location uncertainty.
        check.error.x<- ifelse(xll[which(check.equal.x == FALSE)] < xll[which(check.equal.x == FALSE)+1], 
                               signif(xll[which(check.equal.x == FALSE)+1]-xll[which(check.equal.x == FALSE)],digits=3) == signif(cell.size,digits=3), 
                               signif(xll[which(check.equal.x == FALSE)+1]-xll[which(check.equal.x == FALSE)],digits=3) == -signif(cell.size,digits=3))    
        check.error.y<- ifelse(yll[which(check.equal.y == FALSE)] < yll[which(check.equal.y == FALSE)+1], 
                               signif(yll[which(check.equal.y == FALSE)+1]-yll[which(check.equal.y == FALSE)],digits=3) == signif(cell.size,digits=3), 
                               signif(yll[which(check.equal.y == FALSE)+1]-yll[which(check.equal.y == FALSE)],digits=3) == -signif(cell.size,digits=3)) 
        # If differences between pixel is greater than would be expected from checks, then abort the function call
        # and produce an error message stating there are gaps in this transect.
        if(any(check.error.x == FALSE) | any(check.error.y == FALSE) == TRUE){ stop('Error: Gap in transect pixels') } 
      }
      
      # Transect pixels found, checked, and time-series information organised. Now run MODISSubsets to retrieve subset
      # for this transect.
      MODISSubsets(LoadDat=t.subset,LoadMethod='object',Product=Product,Bands=Bands,Size=Size,SaveDir=SaveDir,StartDate=StartDate,TimeSeriesLength=TimeSeriesLength,DateFormat=DateFormat,Transect=TRUE)   # Write files for each pixel picked up along the transect line
    } # End of loop that reiterates download for each transect.
  }