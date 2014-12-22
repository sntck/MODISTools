\name{WritePRJ}
\alias{WritePRJ}
\title{Internal function to write projection files for MODISGrid
}
\description{The function is used internally by MODISGrid to write PRJ files for the MODIS projection system. A file is written for each ASCII raster grid.
}
\usage{
WritePRJ(Path)
}
\arguments{
  \item{Path}{The file path for the MODIS PRJ file that will be written.
  }
}
\value{A PRJ (plain text) file containing the definitions for the MODIS projection system. This PRJ file can be accessed online at \url{http://daac.ornl.gov/MODIS/modis.prj}.
}
\author{Sean Tuck
}
\seealso{\code{\link{MODISGrid}}
}