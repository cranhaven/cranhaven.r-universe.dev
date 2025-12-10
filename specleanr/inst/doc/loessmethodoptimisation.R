## ----setup, include=FALSE-----------------------------------------------------
cran <- identical(tolower(Sys.getenv("NOT_CRAN")), "false")
if (cran || !curl::has_internet()) {
  knitr::opts_chunk$set(eval = FALSE, collapse = TRUE, comment = "#>")
} else {
  knitr::opts_chunk$set(eval = TRUE, collapse = TRUE, comment = "#>")
}


## ----librarydata--------------------------------------------------------------
library(specleanr)

## ----dataprocessing, fig.width = 6, fig.height= 4, fig.align='center'---------

data(efidata) 

data(jdsdata) 

danube <- sf::st_read(system.file('extdata', "danube.shp.zip",
                                  package = 'specleanr'), quiet=TRUE)


df_online <- getdata(data = c("Squalius cephalus", 'Salmo trutta',"Thymallus thymallus"),
                     extent = danube,
                     gbiflim = 50,
                     inatlim = 50,
                     vertlim = 50,
                     verbose = FALSE)


mergealldfs <- match_datasets(datasets = list(efi= efidata, jds = jdsdata,
                                              onlinedata = df_online),
                              country = c('JDS4_sampling_ID'),
                              lats = 'lat', lons = 'lon',
                              species = c('speciesname', 'scientificName'))
#Cleaning data

cleannames_df <- check_names(data = mergealldfs, colsp = 'species', pct = 90,
                             merge = TRUE, verbose = FALSE)

spfilter <- cleannames_df[cleannames_df$speciescheck %in%
                                   c("Squalius cephalus", 'Salmo trutta',
                                     "Thymallus thymallus","Anguilla anguilla", 
                                     'Barbatula barbatula'),]

worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package = 'specleanr'))

#Get basin shapefile to delineate the study region: optional

danube <- sf::st_read(system.file('extdata', 'danube.shp.zip',
                                  package = 'specleanr'), quiet=TRUE)

## ----outlierdetoptplot, fig.width = 6, fig.height= 4.5, fig.align='center', dpi=120----
parm <- par(mfrow = c(2, 2),
    mar = c(3,3, 1.5, 0.5),
    oma = c(0, 0, 0, 0),
    mgp = c(1.7, 0.8, 0)
)

spp <- unique(spfilter$speciescheck)

pltout <- lapply(spp, function(s){
  
  spout <- spfilter[spfilter[,'speciescheck'] %in%s,]
  
  refdata <-  pred_extract(data= spout, raster= worldclim,
                           lat = 'decimalLatitude',
                           lon = 'decimalLongitude',
                           colsp = 'speciescheck',
                           bbox  = danube,
                           list= TRUE,
                           minpts = 10)
  
  outdet <- multidetect(data = refdata, multiple = FALSE,
                        var = 'bio6', output = 'outlier',
                        exclude = c('x','y'),
                        methods = c('zscore', 'adjbox',
                                    'logboxplot', 'distboxplot',
                                    'iqr', 'semiqr','seqfences',
                                    'hampel','kmeans',
                                    'jknife', 'onesvm',
                                    'iforest'), 
                        warn = FALSE)
  print(nrow(refdata))
  
  opt <- optimal_threshold(refdata = refdata, outliers = outdet, 
                             plotsetting = list(plot = TRUE, group = s))
  opt
  
})
par(parm)

## ----simulated data-----------------------------------------------------------
set.seed(113554333)
a <- rnorm(30, 32, 1)
b <- rnorm(30, 4, 1)
c <- rnorm(30, 0, 1)
d <- rnorm(30, 6, 1)
#add outlier rows
out <- c(409, 43, 76, 23)
out1 <- c(-0.2409, 10, 43, 22)
out2 <- c(1509, 0.43, 76, 23)

df <- data.frame(a, b, c, d)

df2 <- rbind(df, out, out1, out2)


## ----outlier detection--------------------------------------------------------
outdet2 <- multidetect(data = df2, multiple = FALSE,
                      var = 'a', output = 'outlier',
                      methods = c('zscore', 'adjbox',
                                  'logboxplot', 'distboxplot',
                                  'iqr', 'semiqr','seqfences',
                                  'hampel','kmeans',
                                  'jknife', 'onesvm',
                                  'iforest'), 
                      warn = FALSE)

## ----visualise data, fig.width = 6, fig.height= 4, fig.align='center'---------
par(mar = c(3, 3, 1.5, 1.5))

opt1 <- optimal_threshold(refdata = df2, 
                          outliers = outdet2, 
                         plotsetting = list(plot = TRUE))
opt1



## ----check for the outlier weights and data cleaning--------------------------
#get the weights for the flagged records

weights <- ocindex(x = outdet2, absolute = TRUE, props = TRUE, threshold = 0.1, warn = FALSE)

print(weights)

dfclean <- extract_clean_data(refdata = df2, outliers = outdet2, loess = TRUE)

print(dfclean)


