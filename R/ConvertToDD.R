ConvertToDD <-
function(XY, FileSep=NULL, LatColName, LongColName, DegFormat= 'DM' | 'DMS')
{
  if(is.object(XY)) { XY<- data.frame(XY) }
  if(is.character(XY)) {
    if(FileSep == NULL){
      stop("Data is a file path. if you want to load a file as input, you must also specify it's delimiter (FileSep).")
    }
    XY<- read.delim(XY, sep=FileSep) 
  }
  if(!is.object(XY) & !is.character(XY)){
    stop("Data is incorrectly specified. Must either be the name of an object in R, or a file path character string.")
  }
  DMS.lat<- as.character(XY[,which(names(XY) == LatColName)])
  DMS.long<- as.character(XY[,which(names(XY) == LongColName)])  
  DD.lat<- c()
  DD.long<- c()
  S.lat<- c()
  S.long<- c()
  
  D.point.lat<- regexpr("([^0-9][0-9]{1,2}[^0-9])", DMS.lat)
  if(DegFormat == 'DMS') {      # If original format is degrees minutes seconds
    D.lat<- as.numeric(substr(DMS.lat, 1, D.point.lat-1))
    M.lat<- as.numeric(substr(DMS.lat, D.point.lat+1, D.point.lat+attr(D.point.lat, "match.length")-2))
    for(i in 1:length(DMS.lat)){
      if(substr(DMS.lat[i], nchar(DMS.lat[i]), nchar(DMS.lat[i])) == 'N') {
        S.lat[i]<- as.numeric(substr(DMS.lat[i], D.point.lat[i]+attr(D.point.lat, "match.length"), nchar(DMS.lat[i])-2)) 
      } else {
        if(substr(DMS.lat[i], nchar(DMS.lat[i]), nchar(DMS.lat[i])) == 'S') { 
          D.lat[i]<- -D.lat[i]
          S.lat[i]<- as.numeric(substr(DMS.lat[i], D.point.lat[i]+attr(D.point.lat, "match.length"), nchar(DMS.lat[i])-2)) 
        } else {
          S.lat[i]<- as.numeric(substr(DMS.lat[i], D.point.lat[i]+attr(D.point.lat, "match.length"), nchar(DMS.lat[i])-1))
        }
      }
      
      # Calculate decimal degrees.
      if(D.lat[i] >= 0){
        DD.lat[i]<- (D.lat[i]) + ((M.lat[i])/60) + ((S.lat[i])/3600)
      } else {
        DD.lat[i]<- -((S.lat[i])/3600) - ((M.lat[i])/60) + (D.lat[i])
      }
      if(substr(DMS.lat[i], nchar(DMS.lat[i]), nchar(DMS.lat[i])) == 'S' & D.lat[i] == 0){ 
        DD.lat[i]<- -DD.lat[i] 
      }
    }     
  } 
  
  if(DegFormat == 'DM') {     # If original format is degrees decimal minutes
    D.lat<- as.numeric(substr(DMS.lat, 1, D.point.lat-1))
    M.lat<- c()
    for(i in 1:length(DMS.lat)){
      if(substr(DMS.lat[i], nchar(DMS.lat[i]), nchar(DMS.lat[i])) == 'N') { 
        M.lat[i]<- as.numeric(substr(DMS.lat[i], D.point.lat[i]+1, nchar(DMS.lat[i])-2)) 
      } else {
        if(substr(DMS.lat[i], nchar(DMS.lat[i]), nchar(DMS.lat[i])) == 'S') { 
           M.lat[i]<- as.numeric(substr(DMS.lat[i], D.point.lat[i]+1, nchar(DMS.lat[i])-2))
           D.lat[i]<- -D.lat[i] 
        } else {
          M.lat[i]<- as.numeric(substr(DMS.lat[i], D.point.lat[i]+1, nchar(DMS.lat[i])-1))
        }
      }
      
      # Calculate decimal degrees.
      if(D.lat[i] >= 0){
        DD.lat[i]<- (D.lat[i]) + ((M.lat[i])/60)
      } else {
        DD.lat[i]<- -((M.lat[i])/60) + (D.lat[i])
      }
      if(substr(DMS.lat[i], nchar(DMS.lat[i]), nchar(DMS.lat[i])) == 'S' & D.lat[i] == 0){ 
        DD.lat[i]<- -DD.lat[i] 
      }                      
    }     
  }
  
  # Checks that lat answers are going to be sensible before returning result.
  if(any(abs(D.lat) > 90)){
    print("Invalid degrees of latitude entries:")
    print(XY[which(abs(D.lat) > 90), ])
    stop("Range of valid degrees is from -90 to 90.")
  }
  if(any(M.lat < 0 & M.lat > 60)){
    print("Invalid minutes entries:")
    print(XY[which(M.lat > 0 & M.lat < 60), ])
    stop("Range of valid minutes is from 0 to 60.")
  }
  if(any(S.lat < 0 & S.lat > 60)){
    print("Invalid seconds entries:")
    print(XY[which(S.lat > 0 & S.lat < 60), ])
    stop("Range of valid seconds is from 0 to 60.")
  }
  #
  
  D.point.long<- regexpr("([^0-9][0-9]{1,2}[^0-9])", DMS.long)
  if(DegFormat == 'DMS') {      # If original format is degrees minutes seconds
    D.long<- as.numeric(substr(DMS.long, 1, D.point.long-1))
    M.long<- as.numeric(substr(DMS.long, D.point.long+1, D.point.long+attr(D.point.long, "match.length")-2))
    for(i in 1:length(DMS.long)){
      if(substr(DMS.long[i], nchar(DMS.long[i]), nchar(DMS.long[i])) == 'E') { 
        S.long[i]<- as.numeric(substr(DMS.long[i], D.point.long[i]+attr(D.point.long, "match.length"), nchar(DMS.long[i])-2)) 
      } else {
        if(substr(DMS.long[i], nchar(DMS.long[i]), nchar(DMS.long[i])) == 'W') { 
          S.long[i]<- as.numeric(substr(DMS.long[i], D.point.long[i]+attr(D.point.long, "match.length"), nchar(DMS.long[i])-2))
          D.long[i]<- -D.long[i] 
        } else {
          S.long[i]<- as.numeric(substr(DMS.long[i], D.point.long[i]+attr(D.point.long, "match.length"), nchar(DMS.long[i])-1))
        }
      }
      
      # Calculate decimal degrees.
      if(D.long[i] >= 0){
        DD.long[i]<- (D.long[i]) + ((M.long[i])/60) + ((S.long[i])/3600)
      } else {
        DD.long[i]<- -((S.long[i])/3600) - ((M.long[i])/60) + (D.long[i])
      }
      if(substr(DMS.long[i], nchar(DMS.long[i]), nchar(DMS.long[i])) == 'W' & D.long[i] == 0){ 
        DD.long[i]<- -DD.long[i] 
      }
    }   
  } 
  
  if(DegFormat == 'DM') {     # If original format is degrees decimal minutes
    D.long<- as.numeric(substr(DMS.long, 1, D.point.long-1))
    M.long<- c()
    for(i in 1:length(DMS.long)){
      if(substr(DMS.long[i], nchar(DMS.long[i]), nchar(DMS.long[i])) == 'E') {   
        M.long[i]<- as.numeric(substr(DMS.long[i], D.point.long[i]+1, nchar(DMS.long[i])-2))
      } else {
        if(substr(DMS.long[i], nchar(DMS.long[i]), nchar(DMS.long[i])) == 'W') {   
          M.long[i]<- as.numeric(substr(DMS.long[i], D.point.long[i]+1, nchar(DMS.long[i])-2))
          D.long[i]<- -D.long[i] 
        } else {
          M.long[i]<- as.numeric(substr(DMS.long[i], D.point.long[i]+1, nchar(DMS.long[i])-1))
        }
      }
      
      # Calculate decimal degrees.
      if(D.long[i] >= 0){
        DD.long[i]<- (D.long[i]) + ((M.long[i])/60)
      } else {
        DD.long[i]<- -((M.long[i])/60) + (D.long[i])
      }
      if(substr(DMS.long[i], nchar(DMS.long[i]), nchar(DMS.long[i])) == 'W' & D.long[i] == 0){ 
        DD.long[i]<- -DD.long[i] 
      }                   
    }  
  }
  
  # Checks that long answers are going to be sensible before returning result.
  if(any(abs(D.long) > 180)){
    print("Invalid degrees of longitude entries:")
    print(XY[which(abs(D.long) > 180), ])
    stop("Range of valid degrees longitude is from -180 to 180.")
  }
  if(any(M.long < 0 & M.long > 60)){
    print("Invalid minutes entries:")
    print(XY[which(M.long > 0 & M.long < 60), ])
    stop("Range of valid minutes is from 0 to 60.")
  }
  if(any(S.long < 0 & S.long > 60)){
    print("Invalid seconds entries:")
    print(XY[which(S.long > 0 & S.long < 60), ])
    stop("Range of valid seconds is from 0 to 60.")
  }
  
  # Final checks that -90 <= decimal lat <= 90 and -180 <= decimal long <= 180, and then return the result.
  lat.res.check<- all(abs(DD.lat) <= 90)
  long.res.check<- all(abs(DD.long) <= 180)
  if(lat.res.check == FALSE & long.res.check == FALSE){
    stop("It appears an invalid answer has been calculated. Check for values just beyond the valid ranges of lat and long.")
  } else {
    return(cbind(DD.lat, DD.long))
  }
}