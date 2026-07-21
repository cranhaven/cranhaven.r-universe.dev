# cruts

The gridded Climatic Research Unit (CRU) TS (time-series) 3.21 datasets are month-by-month variations in climate over the period 1901-2012, on high-resolution (0.5 x 0.5 degree) grids, produced by the Climatic Research Unit (CRU) at the University of East Anglia.  They are available from here:

http://catalogue.ceda.ac.uk/uuid/ac4ecbd554d0dd52a9b575d9666dc42d

This is an R package for reading in and manipulating CRU TS3.21: Climatic Research Unit (CRU) Time-Series (TS) Version 3.21 data. A test script is below

* First load all required packages

    library(cruts);library(sp);library(raster);library(stringr);library(lubridate);library(ncdf);library(rgdal)

* this dataset contains a SpatialPolygonsDataFram polygon 'ama', I want to crop the CRU TS dataset to this polygon
    
    load("mydir/polygon_data.RData") 

* the unzipped NetCDF file containing the data, I have only tested the package on this file
    
    fln <- "mydir/cru_ts3.21.1901.2012.tmp.dat.nc"

* this function returns a raster brick with the raw data
    
    br <- cruts2raster(ncfile=fln,timeRange=c("2004-01-01","2013-12-31"),poly=poly,offset=offset,type="brick")

* this function returns your polygon with the raw data averaged over each region
        
    temp <- cruts2poly(ncfile=fln,poly=ama,timeRange=c("2004-01-01","2013-12-31"),na.rm=TRUE)

* this function returns your polygon with anomalies either as a raster brick or if a SpatialPolygonsDataFrame is specified, the data averaged over each region
        
    anom <- getAnomaly(ncfile=fln,poly=ama,timeRange=c("2004-01-01","2013-12-31"),na.rm=TRUE)
