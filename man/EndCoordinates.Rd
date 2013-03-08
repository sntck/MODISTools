\name{EndCoordinates}
\alias{EndCoordinates}
\title{Calculate end location of a transect}
\description{Function that uses a set of locations in decimal degrees to calculate new locations, at a specified distance and angle away, that can form the start and end coordinates for transects.}
\usage{EndCoordinates(LoadDat, LoadMethod = 'object'|'ext.file', FileSep = NULL, 
		Distance = 1000, Angle = 90, AngleUnits = 'radians'|'degrees', Dir = ".", 
		FileName = "Transect Coordinates")}
\arguments{
  \item{LoadDat}{Input data; an object or external file to be read in, containing a set of locations to be used for calculating new end coordinates.}
  \item{LoadMethod}{Character string; method of data input. If XY is a text file LoadMethod='ext.file', if LoadMethod is an object method='object'.}
  \item{FileSep}{If LoadMethod='ext.file', choose the field separator character.}
  \item{Distance}{Numeric; distance, in metres, that the end coordinates should be from the focal coordinates.}
  \item{Angle}{Numeric; angle, in either degrees or radians, that the end coordinates should be from the focal coordinates.}
  \item{AngleUnits}{Character String; the unit the angle is given in, either 'radians' or 'degrees'.}
  \item{Dir}{Character string; an optional argument to specify the subdirectory where the output file should be saved. Default Dir='.' writes file to the working directory.}
  \item{FileName}{Character string; an optional argument to specify the name given to the output file.  Default is "Transect Coordinates".}
}
\details{Input dataset, LoadDat, should contain separate columns for latitudes and longitudes, named 'start.lat' and 'start.long' respectively.  Coordinates should be in decimal degrees.}
\value{A new csv file containing the new end coordinate data appended to the original data.}   
\author{Helen Phillips}
\seealso{\code{\link[MODISTools:MODISTransects]{MODISTransects}} \code{\link[MODISTools:ConvertToDD]{ConvertToDD}}}
\examples{data(EndCoordinatesExample)
	EndCoordinates(LoadDat=EndCoordinatesExample, LoadMethod='object', 
	Distance = 2000, Angle = 90, AngleUnits = 'degrees')
}