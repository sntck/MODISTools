\name{ConvertToDD}
\alias{ConvertToDD}
\title{Degrees Minutes Seconds/Decimal Minutes - Decimal Degrees Converter}
\description{Takes a set of coordinates in either degrees minutes seconds or degrees decimal minutes, and converts them to decimal degrees format (dd.ddddd; -dd.ddddd).
  The function is flexible to take the input coordinates in several different formats.}
\usage{ConvertToDD(XY, FileSep=NULL, LatColName, LongColName, 
LoadMethod = 'object' | 'ext.file', DegFormat = 'DM' | 'DMS', DSign)}
\arguments{
  \item{XY}{Input dataset; either an object already in the workspace, or a file to be read in.}
  \item{FileSep}{If method="ext.file", choose the field separator character.}
  \item{LatColName}{Character string; the name of the column in xy containing latitude data.}
  \item{LongColName}{Character string; the name of the column in xy containing longitude data.}
  \item{LoadMethod}{Character string; method of location data input. If load.dat is a text file method = "ext.file", if load.dat is an object method = "object".}
  \item{DegFormat}{Character string; deg.format = "DM" | "DMS". DM selects degrees decimal minutes-decimal degrees conversion. DMS selects degrees minutes seconds-decimal degrees conversion.}
  \item{DSign}{A one character symbol, that is used to denote degrees in the input data frame.}
}
\details{There is flexibility in the layout of each degrees input format. For example, N-S/E-W can be described by positive-negative coordinates or by using respective letters at the end of each coordinate. The layout, however, must be consistent throughout the input dataset.
    The standard notation used for minutes and seconds is inappropriate as they are used to identify character strings. These must be changed to any other one-character length notation before being input.
    Coordinates should not contain spaces.}
\value{Output is a data frame of same dimensions as input, with each value converted to decimal degrees, named "DD.lat" and "DD.long".}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{
data(ConvertExample)

ConvertToDD(XY=ConvertExample, LatColName='lat', LongColName='long', 
    LoadMethod='object', DegFormat='DMS', DSign='d')}