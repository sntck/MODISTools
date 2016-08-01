MODISTools
=========
R package - retrieving & using MODIS data from NASA LPDAAC archive
---------

MODISTools is an R package for retrieving and using MODIS data subsets using ORNL DAAC web service (SOAP) for subsetting from Oak Ridge National Laboratory (ORNL). It provides a batch method for retrieving subsets of MODIS [http://modis.gsfc.nasa.gov/](http://modis.gsfc.nasa.gov/) remotely sensed data and processing them to a format ready for user-friendly application in R, such as statistical modelling.

The most important function is `MODISSubsets`, for requesting subsets from a given MODIS product for multiple time-series. Each time-series is defined by a coordinate location (WGS-1984), a surrounding area of interest, and a start and end date. Automating this as a batch process reduces time, effort, and human error. Alternatively, `MODISTransects` expands upon `MODISSubsets` by extracting MODIS data along a transect, and its surrounding neighbourhood. Downloaded subsets are saved in ASCII files, but can be converted to ASCII grid files for use in a GIS environment. `MODISSummaries` computes summary statistics of downloaded subsets and organises summarised data back with the original input dataset, creating a csv file that can be easily used for modelling; this provides efficient storage of data and a transparent process from data collection, to processing, to a form that is ready for final use.

The functions were originally used for downloading vegetation indices data, but have been generalised to provide a package that performs the same functionality for any MODIS data that available through the web service. For a list of available MODIS products, see [http://daac.ornl.gov/MODIS/MODIS-menu/products.html](http://daac.ornl.gov/MODIS/MODIS-menu/products.html). Other minor functions -- including a lat-long coordinate conversion tool -- are included to aid this process.

Recent stable releases of this package have been checked and built on Windows, Mac, and Linux, and last checked on R 3.1.2 on 2014-12-22. `MODISTools` is written by [Sean Tuck](https://github.com/seantuck12) and [Helen Phillips](https://github.com/helenphillips). This package can be used under the terms of the GNU GPLv3 license; feel free to use and modify as you wish, but please cite our work where appropriate. To cite MODISTools in publications, please use:

Tuck, S.L., Phillips, H.R.P., Hintzen, R.E., Scharlemann, J.P.W., Purvis, A. and Hudson, L.N. (2014) MODISTools -- downloading and processing MODIS remotely sensed data in R. Ecology and Evolution, 4 (24), 4658--4668. DOI: 10.1002/ece3.1273.

Some of the changes in recent updates:
* New function, `MODISGrid`, that takes downloaded ASCII files and converts them into ASCII grid files, which can be used in a GIS environment.
* Optional time-series plots for diagnostics as output from `MODISSummaries`.
* `MODISGrid` now writes MODIS projection (PRJ) files for all ASCII raster grids, so their correct projection is defined. These files can now be loaded directly into a GIS environment.
* `MODISGrid` now more flexibly deals with data files that contain multiple products.
* Citation for publication in Ecology and Evolution added.
* Documentation and 'Using MODISTools' vignette updated.

Changes coming soon:
* More product-specific time-series summaries in `MODISSummaries`.

Frequently asked questions
=========
Available MODIS products
---------
Not all MODIS products are available using `MODISTools` (and the web service it uses). The list of products available using `MODISTools` can be found [here](http://daac.ornl.gov/MODIS/MODIS-menu/products.html), or by running `GetProducts()`. There is no option for selecting different versions of products.

Other tools for accessing MODIS data in R are available:
* The package [laads](https://github.com/masalmon/laads) is under development, to provide an interface to the NASA API of MODIS Level 1 and Atmosphere data products.
* `rts::ModisDownload()`.

ORNL DAAC FAQ
---------
The Oak Ridge National Laboratory Distributed Active Archive Center (ORNL DAAC) has an FAQ [here](http://daac.ornl.gov/faq/faq.shtml).

Installation
=========
The most recent stable release can be installed from the CRAN repository, by running:
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
install_github("seantuck12/MODISTools", build_vignettes=TRUE)
```