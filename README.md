MODISTools
=============
R package - retrieving & using MODIS data from NASA LPDAAC archive
-------------

Provides a batch method for retrieving subsets of NASA's MODIS [http://modis.gsfc.nasa.gov/](http://modis.gsfc.nasa.gov/) remotely-sensed data and processing them to a format ready for user friendly application in R, such as statistical modelling.

The main purpose of this package is to employ the MODIS SOAP Web Service to remotely interrogate the MODIS archive and locally download the requested datasets. The most important function is `MODISSubsets`, which allows the user to request subsets from a given MODIS product for multiple time-series. Each time-series is defined by a coordinate location (WGS-1984), a specified surrounding extent of pixels, and a start and end date. The relevant MODIS product data is then extracted for these location/time combinations. Automating this as a batch process greatly reduces time, effort, and human error. Alternatively, `MODISTransects` expands upon `MODISSubsets` by extracting MODIS data along a transect, and its surrounding neighbourhood. Downloaded subsets are saved in ascii files, which are then accessed by `MODISSummaries` for computing summary statistics. It also organises downloaded data back with the original input data into a csv file that can be easily used for modelling; this provides efficient storage of data and a transparent process from data collection, to processing, to a form that is ready for final use.

The functions were originally used for downloading vegetation indices data, but have been generalised to provide a package that performs the same functionality for any MODIS data for similar aims. For a list of all MODIS products, see [https://lpdaac.usgs.gov/products/modis_products_table](https://lpdaac.usgs.gov/products/modis_products_table). Other minor functions -- including a lat-long coordinate conversion tool -- are included to aid this process.

Recent stable releases of this package have been checked and built on Windows, Mac, and Linux, and last checked on R 3.0.1 on 25/07/2013. `MODISTools` is written by [Sean Tuck](https://github.com/seantuck12) and [Helen Phillips](https://github.com/helenphillips). This package can be used under the terms of the GNU GPLv3 license; feel free to use and modify as you wish, but please cite my work where appropriate.

Changes coming soon:
* New function to produce one matrix for all timeseries data downloaded, with a row for each date in the timeseries and a column for each unique timeseries (i.e., each ASCII file downloaded).
* `MODISSubsets` and `MODISSummaries` will be extended to automate download for data from many products at a time.
* `MODISSummaries` and `QualityCheck` capability to deal with MCD43A4 (BRDF Reflectance Bands) and its quality control data in MCD43A2.


Installation
---------
`MODISTools` is now available from CRAN. So, the most recent stable release can be installed from the CRAN repository, by running:
```
install.packages("MODISTools")
```
This is the recommended way to download.

Alternatively, the most up-to-date in-development version of `MODISTools` can be installed from this github repository. To do this, install the package `devtools` if you haven't already done so.
```
install.packages("devtools")
library(devtools)
```
Then use `install_github`, with this repository name, to install `MODISTools` straight from github.
```
install_github("MODISTools", "seantuck12")
```


Dependencies
----------

http://www.omegahat.org/RCurl

http://www.omegahat.org/RSXML
