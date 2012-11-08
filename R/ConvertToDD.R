ConvertToDD <-
function(XY, FileSep=NULL, LatColName, LongColName, LoadMethod= 'object' | 'ext.file', DegFormat= 'DM' | 'DMS', DSign)
{
  if(LoadMethod == 'object') { dat<- data.frame(XY) }
  if(LoadMethod == 'ext.file') { dat<- read.delim(XY, sep=FileSep) }
  DMS.lat<- as.character(dat[,which(names(dat) == LatColName)])
  DMS.long<- as.character(dat[,which(names(dat) == LongColName)])  
  lat.res<- c()
  long.res<- c()
  DD.lat<- c()
  DD.long<- c()
  S.lat<- c()
  S.long<- c()
  
  D.point.lat<- regexpr(DSign, DMS.lat)
  if(DegFormat == 'DMS') {      # If original format is degrees minutes seconds
    D.lat<- as.numeric(substr(DMS.lat, 1, D.point.lat-1))
    M.lat<- as.numeric(substr(DMS.lat, D.point.lat+1, D.point.lat+2))
    for(i in 1:length(DMS.lat)){
      if(substr(DMS.lat[i], nchar(DMS.lat[i]), nchar(DMS.lat[i])) == 'N') {
        S.lat[i]<- as.numeric(substr(DMS.lat[i], D.point.lat[i]+4, nchar(DMS.lat[i])-2)) 
      } else {
        if(substr(DMS.lat[i], nchar(DMS.lat[i]), nchar(DMS.lat[i])) == 'S') { 
          D.lat[i]<- -D.lat[i]
          S.lat[i]<- as.numeric(substr(DMS.lat[i], D.point.lat[i]+4, nchar(DMS.lat[i])-2)) 
        } else {
          S.lat[i]<- as.numeric(substr(DMS.lat[i], D.point.lat[i]+4, nchar(DMS.lat[i])-1))
        }
      }
      if(D.lat[i] >= 0){
        DD.lat[i]<- (D.lat[i]) + ((M.lat[i])/60) + ((S.lat[i])/3600)
      } else {
        DD.lat[i]<- -((S.lat[i])/3600) - ((M.lat[i])/60) + (D.lat[i])
      }
      if(substr(DMS.lat[i], nchar(DMS.lat[i]), nchar(DMS.lat[i])) == 'W' && D.lat[i] == 0){ DD.lat[i]<- -DD.lat[i] }
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
      if(D.lat[i] >= 0){
        DD.lat[i]<- (D.lat[i]) + ((M.lat[i])/60)
      } else {
        DD.lat[i]<- -((M.lat[i])/60) + (D.lat[i])
      }
      if(substr(DMS.lat[i], nchar(DMS.lat[i]), nchar(DMS.lat[i])) == 'W' && D.lat[i] == 0){ DD.lat[i]<- -DD.lat[i] }                      
    }     
  } 
  
  
  D.point.long<- regexpr(DSign, DMS.long)
  if(DegFormat == 'DMS') {      # If original format is degrees minutes seconds
    D.long<- as.numeric(substr(DMS.long, 1, D.point.long-1))
    M.long<- as.numeric(substr(DMS.long, D.point.long+1, D.point.long+2))
    for(i in 1:length(DMS.long)){
      if(substr(DMS.long[i], nchar(DMS.long[i]), nchar(DMS.long[i])) == 'E') { 
        S.long[i]<- as.numeric(substr(DMS.long[i], D.point.long[i]+4, nchar(DMS.long[i])-2)) 
      } else {
        if(substr(DMS.long[i], nchar(DMS.long[i]), nchar(DMS.long[i])) == 'W') { 
          S.long[i]<- as.numeric(substr(DMS.long[i], D.point.long[i]+4, nchar(DMS.long[i])-2))
          D.long[i]<- -D.long[i] 
        } else {
          S.long[i]<- as.numeric(substr(DMS.long[i], D.point.long[i]+4, nchar(DMS.long[i])-1))
        }
      }
      if(D.long[i] >= 0){
        DD.long[i]<- (D.long[i]) + ((M.long[i])/60) + ((S.long[i])/3600)
      } else {
        DD.long[i]<- -((S.long[i])/3600) - ((M.long[i])/60) + (D.long[i])
      }
      if(substr(DMS.long[i], nchar(DMS.long[i]), nchar(DMS.long[i])) == 'W' && D.long[i] == 0){ DD.long[i]<- -DD.long[i] }
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
      if(D.long[i] >= 0){
        DD.long[i]<- (D.long[i]) + ((M.long[i])/60)
      } else {
        DD.long[i]<- -((M.long[i])/60) + (D.long[i])
      }
      if(substr(DMS.long[i], nchar(DMS.long[i]), nchar(DMS.long[i])) == 'W' && D.long[i] == 0){ DD.long[i]<- -DD.long[i] }                      
    }  
  }
  return(cbind(DD.lat, DD.long))
}