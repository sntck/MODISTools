\name{ConvertToDD}
\alias{ConvertToDD}
\title{Degrees Minutes Seconds/Decimal Minutes - Decimal Degrees Converter}
\description{Takes a set of coordinates in either degrees minutes seconds or degrees decimal minutes, and converts them to decimal degrees format (##.#####; -##.#####). The function is flexible to take the input coordinates in several different formats.
}
\usage{ConvertToDD(XY, FileSep=NULL, LatColName, LongColName)
}
\arguments{
  \item{XY}{Input dataset; either the name of an object already in the workspace, or a file to be read in by specifying its file path as a character string.}
  \item{FileSep}{If XY is a character string that corresponds to a file path, choose the delimiter character for that file (e.g. "," for comma separated).}
  \item{LatColName}{Character string; the name of the column in XY containing latitude data.}
  \item{LongColName}{Character string; the name of the column in XY containing longitude data.}
}
\details{There is flexibility in the layout of each degrees input format. For example, N-S/E-W can be described by positive-negative coordinates or by using respective letters at the end of each coordinate. For coordinates in degrees minutes seconds to be converted requires three numbers - degrees, minutes, and seconds - each separated by a single non-numeric character (e.g. 51d24'51.106"N 0d38'56.018"W). For coordinates in degrees minutes to be converted requires just two numbers - degrees and minutes (e.g. 51d24.106'N 0d38.018'W). Coordinates should not contain spaces in between the numbers. The degrees format does not have to be consistent throughout the data set.}
\value{Output is a data frame of same dimensions as input, with each value converted to decimal degrees, named "DD.lat" and "DD.long".}
\author{Sean Tuck}
\seealso{\code{\link[MODISTools:MODISSubsets]{MODISSubsets}}}
\examples{
data(ConvertExample)
ConvertToDD(XY=ConvertExample, LatColName="lat", LongColName="long")}