MODISTools package for R statistical computing environment
=============

Provides a batch method for retrieving subsets of NASA's MODIS (http://modis.gsfc.nasa.gov/) remotely-sensed data and processing them to a format ready for user friendly application in R, such as statistical modelling.

The main purpose of this package is to employ the MODIS SOAP Web Service to remotely interrogate the MODIS archive and locally download the requested datasets. The most important function is MODISSubsets, which allows the user to request subsets from a given MODIS product for multiple time-series. Each time-series is defined by a coordinate location (WGS-1984), a specified surrounding extent of pixels, and a start and end date. The relevant MODIS product data is then extracted for these location/time combinations. Automating this as a batch process greatly reduces time, effort, and human error. Alternatively, MODISTransects expands upon MODISSubsets by extracting MODIS data along a transect, and its surrounding neighbourhood. Downloaded subsets are saved in ascii files, which are then accessed by MODISSummaries for computing summary statistics. It also organises downloaded data back with the original input data into a csv file that can be easily used for modelling; this provides efficient storage of data and a transparent process from data collection, to processing, to a form that is ready for final use.

The functions were originally used for downloading vegetation indices data, but have been generalised to provide a package that performs the same functionality for any MODIS data for similar aims. Other minor functions -- including a lat-long coordinate conversion tool -- are included to aid this process.

Installation
-------

See the Downloads page for most recent versions of MODISTools.
Dependencies:
http://www.omegahat.org/SSOAP/
http://www.omegahat.org/XMLSchema/