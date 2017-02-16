\name{MODISGrid}
\alias{MODISGrid}
\title{Create GIS ASCII files from downloaded MODIS data files
}
\description{Looks for ASCII files of downloaded MODIS data in a directory and creates new ASCII raster grid files that allow data downloaded using MODISTools to connect to a GIS environment. The new raster grid files can be converted into raster images and loaded into a GIS environment where further data processing or spatial analysis can take place.
}
\usage{MODISGrid(Dir = ".", DirName = "MODIS_GRID", SubDir = TRUE, NoDataValues)
}
\arguments{
  \item{Dir}{Character; file path to the directory where downloaded ASCII files to be processed are located and the output is written: default \code{Dir = "."} extracts files from the working directory.
  }
  \item{DirName}{Character; the name of the directory (a subdirectory found in directory specified as \code{Dir}) where the new ASCII raster grids will be written. If the directory does not already exist, it will be written.
  }
  \item{SubDir}{Logical; if TRUE, all ASCII raster grids created from one MODIS data file will be collected under a subdirectory, within \code{DirName}, that has the same name as the MODIS data file from which it was created. If FALSE all ASCII raster grids will simply be written to \code{DirName}, with no nested file structure.
  }
  \item{NoDataValues}{Named list; the no data values for all the data bands contained in the files. Should be specified as a list of vectors: the names for the list elements should be the product codes and each element should contain a named vector of no data values for bands within that product. For guidance, see the example below. All data bands found in the MODIS data files should be listed in this argument. These values are neccessary to identify pixels with missing data. The correct no data values for the data bands of interest can be found at on the ORNL LPDAAC website.
  }
}
\details{A raster grid file is created for each grid found in a MODIS data file (i.e. a new file is written for each date that occurs in a time series). In other words, where the MODIS data file may follow a tile of pixels over time, the raster grids split up each tile to create a new file for that tile at each new date. Therefore, multiple raster grids may be created from one MODIS data file. These files can be stored in one directory or nested in subdirectories for each MODIS data file, using the optional argument \code{SubDir}. By default, the subdirectories are created to retain the spatial structure.

The data are in the MODIS projection system. To combine a raster grid with a projection, the two files must be stored in the same directory with the same file name. Every raster grid needs a respective PRJ file. The projection (PRJ) file for each ASCII raster grid is included in the function output.
}
\value{Multiple ASCII raster grid files, with PRJ files, containing a tile of MODIS pixels at a given date, either all written to one directory or separated into subdirectories for each MODIS data file found. The file names for each raster grid created are composed of the following: "GRID_", the file name of the relevant MODIS data file, the data band that the grid pertains to, and finally the MODIS acquisition date for the grid (in Julian date format).
}
\author{Sean Tuck
}
\seealso{\code{\link{MODISSubsets}}
         \code{\link{MODISSummaries}}
         \code{\link{ExtractTile}}
         }
\examples{
\dontrun{
# dontrun() used because running the example requires internet access.

dat <- EndCoordinatesExample
names(dat) <- c("lat", "long", "start.date", "end.date", "ID")

MODISSubsets(dat, Products = "MOD13Q1", Size = c(1,1), StartDate = TRUE,
             Bands = c("250m_16_days_EVI", "250m_16_days_NDVI", "250m_16_days_pixel_reliability"))

MODISGrid(NoDataValues = list("MOD13Q1" = c("250m_16_days_EVI" = -3000,
                                            "250m_16_days_NDVI" = -3000,
                                            "250m_16_days_pixel_reliability" = -1)))
    }
}
