\name{QualityCheckExample}
\alias{QualityCheckExample}
\docType{data}
\title{Example dataset for QualityCheck}
\description{A dataset containing the MODIS band data, and it's quality control data for the same pixels. The first half rows are band data (EVI data from the MOD13Q1 vegetation indices product), and the second half are pixel reliability data. There are 5 rows for each data type. Each row contains 5 pixels (columns) at that time-step.}
\usage{data(QualityCheckExample)}
\format{
  A data frame with 10 observations on the following 5 variables.
  \describe{
    \item{\code{pixel.1}}{Pixel 1.}
    \item{\code{pixel.2}}{Pixel 2.}
    \item{\code{pixel.3}}{Pixel 3.}
    \item{\code{pixel.4}}{Pixel 4.}
    \item{\code{pixel.5}}{Pixel 5.}
  }
}
\details{The data input to QualityCheck can be a vector or a matrix. It will return an object with the same dimensions. QualityCheck was designed for internal use within MODISSummaries, but can be used separately. When called via MODISSummaries, the data input and it's quality information is expected to be contained in the same ascii files, downloaded using MODISSubsets.}
\source{Arbitrary values within the valid range of EVI data and it's pixel reliability information.}
\keyword{datasets}
