## ----include = FALSE----------------------------------------------------------
cran <- identical(tolower(Sys.getenv("NOT_CRAN")), "false")
if (cran || !curl::has_internet()) {
  knitr::opts_chunk$set(eval = FALSE,
  collapse = TRUE,
  comment = "#>"
  )
} else {
  knitr::opts_chunk$set(eval = TRUE,
  collapse = TRUE,
  comment = "#>"
  )
}

## ----setup, warning=FALSE, message=FALSE--------------------------------------

library(specleanr)


## ----get species occurences---------------------------------------------------

plantdf <- getdata(data = c( "Populus nigra", "Fagus sylvatica"), 
                    gbiflim = 700, inatlim = 100,
                     hasCoordinate = TRUE, 
                   extent = list(xmin = 8.15250, ymin = 42.08333, xmax=29.73583, ymax = 50.24500),
                   verbose = FALSE, warn = FALSE)



## ----environmental parameters from WORLDCLIM----------------------------------

#Get climatic variables from the package folder

worldclim <- system.file('extdata/worldclim.tiff', package = 'specleanr')

worldclim <- terra::rast(worldclim)



## ----extract data and prelimianry analysis------------------------------------

danube_basin <- sf::st_read(system.file('extdata', "danube.shp.zip", package = 'specleanr'), 
                            quiet = TRUE)

#Environmental predictors extraction for multiple species (multiple = TRUE)

multspreference_data <-  pred_extract(data= plantdf, 
                             raster= worldclim, 
                             lat = 'decimalLatitude',
                             lon = 'decimalLongitude',
                             colsp = 'species',  
                             bbox = danube_basin,
                             list= TRUE, 
                             minpts = 10, merge = FALSE, verbose = FALSE, warn = FALSE)

#Environmental prediction extraction for a single species (multiple = FALSE)
fagus_data_filtered <- subset(plantdf, species=="Fagus sylvatica")

fagus_data_reference <-  pred_extract(data= fagus_data_filtered, 
                             raster= worldclim, 
                             lat = 'decimalLatitude',
                             lon = 'decimalLongitude',
                             colsp = 'species',  
                             bbox = danube_basin, 
                             minpts = 10, merge = FALSE, 
                           verbose = FALSE, warn = FALSE)


## ----oultlier detection-------------------------------------------------------

#Flag outlier in single species data (multiple = TRUE)
multspp_outliers <- multidetect(data = multspreference_data,
                       multiple = TRUE,
                       var = 'bio1',
                       output = 'outlier',
                       exclude = c('x','y'), 
                       methods = c('adjbox', "hampel", 'zscore', 
                                   'lof', 'jknife', 'kmeans', 'mahal'),
                       silence_true_errors = FALSE, warn = FALSE, verbose = FALSE)

#Flag outlier in single species data (multiple = FALSE)

fagus_outliers <- multidetect(data = fagus_data_reference,
                       multiple = FALSE,
                       var = 'bio1',
                       output = 'outlier',
                       exclude = c('x','y'), 
                       methods = c('adjbox', "hampel", 'zscore', 
                                   'lof', 'jknife', 'kmeans', 'mahal'),
                       silence_true_errors = FALSE, warn = FALSE, verbose = FALSE)


## ----visualize outliers, fig.width = 6, fig.height= 3.5, fig.align='center'----

ggoutliers(x=multspp_outliers)

#for one species: no index needed
ggoutliers(x= fagus_outliers)


## ----threshold identifcation, fig.width = 6, fig.height= 4, fig.align='center'----

optimal1<- optimal_threshold(refdata = fagus_data_reference, 
                             outliers = fagus_outliers, 
                             plot = list(plot = TRUE, group = "Fagus sylvatica"))


opt <- optimal_threshold(refdata = multspreference_data, 
                         outliers = multspp_outliers, 
                         plotsetting = list(plot = FALSE))

## ----extract outliers from clean dataset--------------------------------------

multspp_qc_data <- extract_clean_data(refdata = multspreference_data, 
                                      outliers = multspp_outliers,
                                      loess = TRUE)

multspp_qc_label <- classify_data(refdata = multspreference_data, 
                                      outliers = multspp_outliers)


fagus_qc_data <- extract_clean_data(refdata = fagus_data_reference, 
                                    outliers = fagus_outliers,
                                    loess = TRUE)

fagus_qc_label <- classify_data(refdata = fagus_data_reference, 
                                    outliers = fagus_outliers)



## ----2d plots single species, fig.width = 5.4, fig.height= 4.2, fig.align='center'----

#for single species
ggenvironmentalspace(qcdata = fagus_qc_label,
                     xvar = 'bio1',
                     yvar = "bio18",
                     xlab = "Annual mean temperature",
                     ylab = "Precipitation of Warmest Quarter",
                     scalecolor = 'viridis',
                     pointsize = 2)


## ----2d plots multiple species, fig.width = 7.4, fig.height= 4.2, fig.align='center'----

#for single species
ggenvironmentalspace(qcdata = multspp_qc_label,
                     xvar = 'bio1',
                     yvar = "bio18",
                     xlab = "Annual mean temperature",
                     ylab = "Precipitation of Warmest Quarter",
                     scalecolor = 'viridis',
                     pointsize = 2)


## ----3d plots single species, fig.width = 7.4, fig.height= 4.2, fig.align='center'----

#for single species
ggenvironmentalspace(qcdata = fagus_qc_label,
                     xvar = 'bio1',
                     yvar = "bio18",
                     zvar = 'bio6',
                     type = "3d",
                     labelvar = "label",
                     xlab = "Annual mean temperature",
                     ylab = "Precipitation of Warmest Quarter",
                     zlab = "Min Temperature of Coldest Month",
                     scalecolor = 'viridis',
                     lpos3d = "right",
                     pointsize = 2)


