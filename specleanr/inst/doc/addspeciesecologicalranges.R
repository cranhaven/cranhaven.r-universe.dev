## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(specleanr)

## ----Loading datasets from the package----------------------------------------
data("jdsdata")
data("efidata")

wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))

#match and clean

matchd <- match_datasets(datasets = list(jds= jdsdata, efi =efidata),
                         lats = 'lat', lons = 'lon',
                         country = 'JDS4_site_ID',
                         species = c('scientificName', 'speciesname'),
                         date=c('sampling_date','Date'))

#matchclean <- check_names(matchd, colsp = 'species', verbose = FALSE, merge = TRUE)

db <- sf::read_sf(system.file('extdata/danube.shp.zip',
                              package = "specleanr"), quiet = TRUE)

## ----Extracting environmental data--------------------------------------------

refdata <- pred_extract(data = matchd, raster = wcd,
                        lat = 'decimalLatitude',
                        lon = 'decimalLongitude',
                        bbox = db,
                        colsp = 'species',
                        list = TRUE,
                        verbose = FALSE,
                        minpts = 6,
                        merge = FALSE)


## ----Ecological ranges--------------------------------------------------------

sqcep <- refdata["Squalius cephalus"]

optdata <- data.frame(species= c("Squalius cephalus", "Abramis brama"),
                      mintemp = c(6, 1.6),maxtemp = c(8.588, 21),
                      meantemp = c(8.5, 10.4), #ecoparam
                      direction = c('greater', 'greater'))


## ----outlier detection including species ecological ranges--------------------

squalius_outlier <- multidetect(data = sqcep, multiple = TRUE,
                      var = 'bio1',
                      output = 'outlier',
                      exclude = c('x','y'),
                      methods = c('zscore', 'adjbox', 'optimal', 'kmeans', "logboxplot", "hampel"),
                      optpar = list(optdf=optdata, optspcol = 'species',
                                    mincol = "mintemp", maxcol = "maxtemp"))


## ----ploting, fig.width = 6, fig.height= 4, fig.align='center'----------------

ggoutliers(squalius_outlier)



## ----clean data extraction and labelling--------------------------------------

squalius_qc_loess <- extract_clean_data(refdata = sqcep, 
                                      outliers = squalius_outlier, loess = TRUE)

#clean dataset
nrow(squalius_qc_loess)

#reference data
nrow(sqcep[[1]])

squalius_qc_labeled <- classify_data(refdata = sqcep, outliers = squalius_outlier)


## ----clean data extraction and labelling_multiple, fig.height=4.1, fig.width=4.5, fig.align='center'----


ggenvironmentalspace(squalius_qc_labeled, 
                     type = '1D',
                     ggxangle = 45, 
                     scalecolor = 'viridis',
                     xhjust = 1,
                     legend_position = 'blank',
                     ylab = "Number of records",
                     xlab = "Outlier labels")


