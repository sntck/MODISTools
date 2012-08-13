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
  D.point.lat<- regexpr(DSign, DMS.lat)
  if(DegFormat == 'DMS') {      # If original format is degrees minutes seconds
    D.lat<- as.numeric(substr(DMS.lat, 1, D.point.lat-1))
    M.lat<- as.numeric(substr(DMS.lat, D.point.lat+1, D.point.lat+2))
    if(substr(DMS.lat[1], nchar(DMS.lat), nchar(DMS.lat)) == 'N') {
      S.lat<- as.numeric(substr(DMS.lat, D.point.lat+4, nchar(DMS.lat)-2)) 
    } else {
      if(substr(DMS.lat[1], nchar(DMS.lat), nchar(DMS.lat)) == 'S') { 
        D.lat<- -D.lat
        S.lat<- as.numeric(substr(DMS.lat, D.point.lat+4, nchar(DMS.lat)-2)) 
      } else {
        S.lat<- as.numeric(substr(DMS.lat, D.point.lat+4, nchar(DMS.lat)-1))
      }
    } 
    nas<- is.na(D.lat) 
    calc.set<- (D.lat[which(!is.na(D.lat))]) >= 0
    lat.res[which(calc.set == TRUE)]<- (D.lat[which(!is.na(D.lat))]) + ((M.lat[which(!is.na(D.lat))])/60) + ((S.lat[which(!is.na(D.lat))])/3600)
    lat.res[which(calc.set == FALSE)]<- -((S.lat[which(!is.na(D.lat))])/3600) - ((M.lat[which(!is.na(D.lat))])/60) + (D.lat[which(!is.na(D.lat))])
    if(substr(DMS.lat[1], nchar(DMS.lat), nchar(DMS.lat)) == 'S'){ lat.res[which(D.lat == 0)]<- -lat.res[which(D.lat == 0)] }                      
    DD.lat[nas == FALSE]<- lat.res
    DD.lat[nas == TRUE]<- NA    
  } 
  if(DegFormat == 'DM') {     # If original format is degrees decimal minutes
    D.lat<- as.numeric(substr(DMS.lat, 1, D.point.lat-1))
    if(substr(DMS.lat[1], nchar(DMS.lat), nchar(DMS.lat)) == 'N') { 
      M.lat<- as.numeric(substr(DMS.lat, D.point.lat+1, nchar(DMS.lat)-2)) 
    } else {
      if(substr(DMS.lat[1], nchar(DMS.lat), nchar(DMS.lat)) == 'S') { 
         M.lat<- as.numeric(substr(DMS.lat, D.point.lat+1, nchar(DMS.lat)-2))
         D.lat<- -D.lat 
      } else {
        M.lat<- as.numeric(substr(DMS.lat, D.point.lat+1, nchar(DMS.lat)-1))
      }
    } 
    nas<- is.na(D.lat) 
    calc.set<- (D.lat[which(!is.na(D.lat))]) >= 0
    lat.res[which(calc.set == TRUE)]<- (D.lat[which(!is.na(D.lat))]) + ((M.lat[which(!is.na(D.lat))])/60)
    lat.res[which(calc.set == FALSE)]<- -((M.lat[which(!is.na(D.lat))])/60) + (D.lat[which(!is.na(D.lat))])
    if(substr(DMS.lat[1], nchar(DMS.lat), nchar(DMS.lat)) == 'S'){ lat.res[which(D.lat == 0)]<- -lat.res[which(D.lat == 0)] }                      
    DD.lat[nas == FALSE]<- lat.res
    DD.lat[nas == TRUE]<- NA      
  } 
  D.point.long<- regexpr(DSign, DMS.long)
  if(DegFormat == 'DMS') {      # If original format is degrees minutes seconds
    D.long<- as.numeric(substr(DMS.long, 1, D.point.long-1))
    M.long<- as.numeric(substr(DMS.long, D.point.long+1, D.point.long+2))
    if(substr(DMS.long[1], nchar(DMS.long), nchar(DMS.long)) == 'E') { 
      S.long<- as.numeric(substr(DMS.long, D.point.long+4, nchar(DMS.long)-2)) 
    } else {
      if(substr(DMS.long[1], nchar(DMS.long), nchar(DMS.long)) == 'W') { 
        S.long<- as.numeric(substr(DMS.long, D.point.long+4, nchar(DMS.long)-2))
        D.long<- -D.long 
      } else {
        S.long<- as.numeric(substr(DMS.long, D.point.long+4, nchar(DMS.long)-1))
      }
    }
    nas<- is.na(D.long) 
    calc.set<- (D.long[which(!is.na(D.long))]) >= 0
    long.res[which(calc.set == TRUE)]<- (D.long[which(!is.na(D.long))]) + ((M.long[which(!is.na(D.long))])/60) + ((S.long[which(!is.na(D.long))])/3600)
    long.res[which(calc.set == FALSE)]<- -((S.long[which(!is.na(D.long))])/3600) - ((M.long[which(!is.na(D.long))])/60) + (D.long[which(!is.na(D.long))])
    if(substr(DMS.long[1], nchar(DMS.long), nchar(DMS.long)) == 'W'){ long.res[which(D.long == 0)]<- -long.res[which(D.long == 0)] }                      
    DD.long[nas == FALSE]<- long.res
    DD.long[nas == TRUE]<- NA   
  } 
  if(DegFormat == 'DM') {     # If original format is degrees decimal minutes
    D.long<- as.numeric(substr(DMS.long, 1, D.point.long-1))
    if(substr(DMS.long[1], nchar(DMS.long), nchar(DMS.long)) == 'E') {   
      M.long<- as.numeric(substr(DMS.long, D.point.long+1, nchar(DMS.long)-2))
    } else {
      if(substr(DMS.long[1], nchar(DMS.long), nchar(DMS.long)) == 'W') {   
        M.long<- as.numeric(substr(DMS.long, D.point.long+1, nchar(DMS.long)-2))
        D.long<- -D.long 
      } else {
        M.long<- as.numeric(substr(DMS.long, D.point.long+1, nchar(DMS.long)-1))
      }
    }
    nas<- is.na(D.long) 
    calc.set<- (D.long[which(!is.na(D.long))]) >= 0
    long.res[which(calc.set == TRUE)]<- (D.long[which(!is.na(D.long))]) + ((M.long[which(!is.na(D.long))])/60)
    long.res[which(calc.set == FALSE)]<- -((M.long[which(!is.na(D.long))])/60) + (D.long[which(!is.na(D.long))])
    if(substr(DMS.long[1], nchar(DMS.long), nchar(DMS.long)) == 'W'){ long.res[which(D.long == 0)]<- -long.res[which(D.long == 0)] }                      
    DD.long[nas == FALSE]<- long.res
    DD.long[nas == TRUE]<- NA   
  }
  return(cbind(DD.lat, DD.long))
}

