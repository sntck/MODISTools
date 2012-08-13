MODISTransects <-
function(LoadData, LoadMethod='object' | 'ext.file', FileSep=NULL, Product, Bands, Size=c(), SaveDir=NULL, StartDate=FALSE, TimeSeriesLength=2, DateFormat='year' | 'posixt', WriteSummary=FALSE)
{
  if(LoadMethod == 'object') { t.dat<- data.frame(LoadData) }   # Load data of locations; external data file, or an R object.
  if(LoadMethod == 'ext.file') { t.dat<- read.delim(LoadData, sep=FileSep) }
  ornlMODIS = processWSDL('http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.wsdl')
  ornlMODISFuncs = genSOAPClientInterface(operations=ornlMODIS@operations[[1]], def=ornlMODIS)
  Tile.width<- (20015109.354+20015109.354)/36                   # Actual width of each tile in the MODIS projection.
  w.res<- regexpr('m_', Bands[1])
  res<- as.numeric(substr(Bands[1], 1, w.res-1))                # Identify resolution and work out actual cell size from projection info accordingly.
  cell.size<- Tile.width/(1200*(1000/res))                      # Works out actual width of each pixel in the MODIS projection.
  for(i in 1:nrow(t.dat)) {                                     # Loop that repeats for each transect.
    delta.lat<- t.dat$end.lat[i]-t.dat$start.lat[i]
    delta.long<- round(t.dat$end.long[i]-t.dat$start.long[i], digits=5)
    lat.metres<- delta.lat*(111.2*1000)                         # Works out how far in metres is the longitudinal difference between start and end locations.
    lat.rad<- median(c(t.dat$start.lat[i], t.dat$end.lat[i]))*(pi/180)         # Takes latitude from middle of two transect points to determine the curvature angle for calculating distance between 1 long at transect location.
    long.metres<- delta.long*(111.2*1000)*cos(lat.rad)          # Works out how far in metres is the longitudinal difference between start and end locations.
    transect<- sqrt((lat.metres^2)+(long.metres^2))             # Works out the actual length of the transect.
    num.points<- transect/cell.size                             # Works out how many points can be equally spaced between the start and end coordinates.
    lat.increment<- round(delta.lat/num.points, digits=5)       # Works out the lat distance of the equally spaced points.
    long.increment<- round(delta.long/num.points, digits=5)     # Works out the long distance if the equally spaced points.
    lat<- t.dat$start.lat[i]
    long<- round(t.dat$start.long[i], digits=5)
    if(lat.increment > 0) {                            # Takes the start lat and long and interpolates a new lat and long equally spaced until the end lat. The following if statements decide                                                  # and long is reached or approached without going past.
      if(long.increment > 0) {                         # how to interpolate the equally spaced coordinates, depending on the position of the start lat and long values relative to the end values.
        while(lat[length(lat)] <= (t.dat$end.lat[i]-lat.increment) & long[length(long)] <= (round(t.dat$end.long[i], digits=5)-long.increment)) {
            lat<- c(lat, round(lat[length(lat)] + lat.increment, digits=5))
            long<- c(long, round(long[length(long)] + long.increment, digits=5))
        }
      } else {
        while(lat[length(lat)] <= (t.dat$end.lat[i]-lat.increment) & long[length(long)] >= (round(t.dat$end.long[i], digits=5)-long.increment)) {
            lat<- c(lat, round(lat[length(lat)] + lat.increment, digits=5))
            long<- c(long, round(long[length(long)] + long.increment, digits=5))
        }
      }
    } else { 
      if(long.increment > 0) {
        while(lat[length(lat)] >= (t.dat$end.lat[i]-lat.increment) & long[length(long)] <= (round(t.dat$end.long[i], digits=5)-long.increment)) {
            lat<- c(lat, round(lat[length(lat)] + lat.increment, digits=5))
            long<- c(long, round(long[length(long)] + long.increment, digits=5))
        }
      } else {
        while(lat[length(lat)] >= (t.dat$end.lat[i]-lat.increment) & long[length(long)] >= (round(t.dat$end.long[i], digits=5)-long.increment)) {
            lat<- c(lat, round(lat[length(lat)] + lat.increment, digits=5))        
            long<- c(long, round(long[length(long)] + long.increment, digits=5))   
        }
      }
    }       
    end.date<- rep(t.dat$end.date[i], length(lat))
    ID<- paste('Transect',t.dat$transect[i],'Point',1:length(lat), sep='')         # Writes the ID of each transect point to be used in the filename.
    if(StartDate == TRUE) {
      start.date<- rep(t.dat$start.date[i], length(lat))                             # Start date and end date for the transect study, repeated for each interpolated transect increment.
      t.subset<- data.frame(ID, lat, long, start.date, end.date)
    } else {
      t.subset<- data.frame(ID, lat, long, end.date)
    }                                   
    t.point<- list(NA)
    xll<- c()
    yll<- c()
    for(n in 1:nrow(t.subset)) {    # Gets the MODIS Projection coordinates for the pixel that each interpolated transect increment falls within.
        t.point[[n]]<- ornlMODISFuncs@functions$getsubset(t.subset$lat[n], t.subset$long[n], 'MOD13Q1', '250m_16_days_EVI', 'A2000049', 'A2000049', 0, 0)
        xll[n]<- t.point[[n]]@xllcorner
        yll[n]<- t.point[[n]]@yllcorner  
    }
    check.equal.x<- signif(xll[1:length(xll)-1], digits=6) == signif(xll[2:length(xll)], digits=6)    # Checks which pixels have the same x or y as the previous pixel.
    check.equal.y<- signif(yll[1:length(yll)-1], digits=5) == signif(yll[2:length(yll)], digits=5)    # From remaining pixels checks if they are +- 1 pixel width (i.e. adjacent pixel) away.
    check.new.x<- ifelse(xll[which(check.equal.x == FALSE)] < xll[which(check.equal.x == FALSE)+1], round(xll[which(check.equal.x == FALSE)]) == round(xll[which(check.equal.x == FALSE)+1]-cell.size), round(xll[which(check.equal.x == FALSE)]) == round(xll[which(check.equal.x == FALSE)+1]+cell.size))
    check.new.y<- ifelse(yll[which(check.equal.y == FALSE)] < yll[which(check.equal.y == FALSE)+1], round(yll[which(check.equal.y == FALSE)]) == round(yll[which(check.equal.y == FALSE)+1]-cell.size), round(yll[which(check.equal.y == FALSE)]) == round(yll[which(check.equal.y == FALSE)+1]+cell.size))    
    if(any(check.new.x == FALSE) | any(check.new.y == FALSE) == TRUE) {   # Check if there are any remaining pixels whose distance from previous does not meet either of above criteria.                                                                  
      check.error.x<- ifelse(xll[which(check.equal.x == FALSE)] < xll[which(check.equal.x == FALSE)+1], signif(xll[which(check.equal.x == FALSE)+1]-xll[which(check.equal.x == FALSE)],digits=3) == signif(cell.size,digits=3), signif(xll[which(check.equal.x == FALSE)+1]-xll[which(check.equal.x == FALSE)],digits=3) == -signif(cell.size,digits=3))    
      check.error.y<- ifelse(yll[which(check.equal.y == FALSE)] < yll[which(check.equal.y == FALSE)+1], signif(yll[which(check.equal.y == FALSE)+1]-yll[which(check.equal.y == FALSE)],digits=3) == signif(cell.size,digits=3), signif(yll[which(check.equal.y == FALSE)+1]-yll[which(check.equal.y == FALSE)],digits=3) == -signif(cell.size,digits=3))    
      if(any(check.error.x == FALSE) | any(check.error.y == FALSE) == TRUE){ stop('Error: Gap in transect pixels') } # Checked if this unaccounted for distance is small enough that it can be attributed to rounding error or MODIS prj location inaccuracy.
    }                                                                                                                # If differences between pixel is greater than would be expected from pixel
    MODISSubsets(LoadDat=t.subset,LoadMethod='object',Product=Product,Bands=Bands,Size=Size,SaveDir=SaveDir,StartDate=StartDate,TimeSeriesLength=TimeSeriesLength,DateFormat=DateFormat,WriteSummary=WriteSummary,Transect=TRUE)   # Write files for each pixel picked up along the transect line
  }
}

