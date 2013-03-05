## Function to find the coordinates from a focal pixel when given a distance and an angle.
## Version 2  5th March 2013  Helen R. Phillips (with help from Isabel Fenton)

## Input file should have the following columns;
## start.lat
## start.long
## distance should be in metres
## angle can either be degrees or radians

EndCoordinates <- function(LoadDat, LoadMethod = 'object'|'ext.file', FileSep = NULL, Distance = 1000, Angle = 90, AngleUnits = 'radians'|'degrees', Dir = ".", FileName = "Transect Coordinates")
{
    if(LoadMethod == 'object') { x<- data.frame(LoadDat) }
    if(LoadMethod == 'ext.file') { x<- read.delim(LoadDat, sep=FileSep) }
    
    if(AngleUnits == 'radians'){
    	if(Angle > (2*pi)){stop('Not sensible radian values. Did you mean degrees?')}
    }
  
    if(AngleUnits == 'degrees'){
    	if(Angle > 360){stop('Not sensible degrees values. Check input.')}
    }
    
    if (AngleUnits == 'radians'){ angle.rad <- Angle }
    if (AngleUnits == 'degrees'){ angle.rad <- Angle/(180/pi) }
	lat.rad <- x$start.lat/(180/pi)
	delta.lat.metres <- round(Distance * cos(angle.rad))
	delta.long.metres <- round(Distance * sin(angle.rad))
	delta.lat.degrees <- delta.lat.metres/(111.2 * 1000)
	delta.long.degrees <- delta.long.metres/((111.2 * 1000) * cos(lat.rad))
	end.lat <- x$start.lat + delta.lat.degrees
	end.long <- x$start.long + delta.long.degrees
	x <- cbind(x, end.lat, end.long)
	
	if (Dir == "."){
			write.csv(x, file = paste(FileName, Distance, "m", Angle, AngleUnits, Sys.Date(),".csv"), row.names=FALSE)
	}else{
			write.csv(x, file = paste(Dir, FileName, Distance, "m", Angle, AngleUnits, Sys.Date(),".csv"), row.names=FALSE)
		}
}