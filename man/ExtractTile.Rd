\name{ExtractTile}
\alias{ExtractTile}
\title{Extract subset tiles from bigger tiles of MODIS data.}
\description{Input a dataset of MODIS data, comprised of one or many tiles of pixels - a column for each pixel in a tile and a row for each tile - and extract a nested subset from within these tiles.
}
\usage{ExtractTile(Data, Rows, Cols, Grid=FALSE)
}
\arguments{
  \item{Data}{Numeric vector, matrix, or data frame; The input data, containing the large tile(s) that smaller tiles will be extracted from. If a matrix or data frame, each row should represent a distinct tile, whilst each column in a row will be a different pixel within the that tile.
  }
  \item{Rows}{Numeric - two integers; Rows[1] should be the number of rows in the large tile(s) of pixels. Rows[2] is the number of rows you would like either side of the tile(s) central pixel in the small tile(s) output.
  }
  \item{Cols}{Numeric - two integers; Cols[1] should be the number of columns in the large tile(s) of pixels. Cols[2] is the number of columns you would like either side of the tile(s) central pixel in the small tile(s) output.
  }
  \item{Grid}{Logical; if Grid=FALSE, the smaller tile(s) will be in a matrix, like the input. If Grid=TRUE, the output will be an array, with tile(s) presented explicitly. See value.
  }
}
\details{Data should only include MODIS data and not any other metadata for the input tiles (see example). Rows[2] and Cols[2] should equate to a nested subset of Rows[1] and Cols[1]. Rows[1] and Cols[1] specify the dimensions of the tiles, laid out in rows in matrix, and therefore should equate to the dimensions of a matrix that is filled by ncols(Data) data points.
}
\value{The output may have two possible structures, optionally chosen with the Grid argument. If Grid=FALSE, the output will be the same structure as the input - a nxm matrix, where n is the number of tiles and m is the number of pixels in each tile - with the same number of rows but only the number of pixels in the smaller tile(s) for each row. If Grid=TRUE, the output will be an array, with the tiles laid out with pixels in spatial order - a matrix of the subset of pixels, for each row in Data.
}
\author{Sean Tuck}
\examples{
\dontrun{ # dontrun() used because running the example requires internet access.
data(SubsetExample)
MODISSubsets(LoadDat = SubsetExample, Products = "MOD13Q1", 
    Bands = c("250m_16_days_EVI", "250m_16_days_pixel_reliability"), Size = c(1,1), 
    StartDate = FALSE, TimeSeriesLength = 1)
MODISSummaries(LoadDat = SubsetExample, Product = "MOD13Q1", Bands = "250m_16_days_EVI", 
    ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
    StartDate = FALSE, QualityScreen = TRUE, QualityThreshold = 0,
    QualityBand = "250m_16_days_pixel_reliability")

if(sum(grepl("MODIS_Data", list.files())) != 1){
  print("Could not identify 'MODIS_Data' csv output file from MODISSummaries")
} else {
  TileExample <- read.csv(list.files(pattern = "MODIS_Data"))
  TileExample <- TileExample[1,which(grepl("pixel", names(TileExample)))]
  
  dim(TileExample)
  dim(ExtractTile(Data = TileExample, Rows = c(9,2), Cols = c(9,2), Grid = FALSE))
  ExtractTile(Data = TileExample, Rows = c(9,2), Cols = c(9,2), Grid = FALSE)
  
  matrix(TileExample, nrow = 9, ncol = 9, byrow = TRUE)
  ExtractTile(Data = TileExample, Rows = c(9,2), Cols = c(9,2), Grid = TRUE)
}
  }
}