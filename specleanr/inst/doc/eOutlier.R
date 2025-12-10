## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"#, error = TRUE
)

## ----libraries, echo=TRUE, warning=FALSE, message=FALSE-----------------------

library(specleanr)


## ----datasoruces, warning=FALSE-----------------------------------------------
#==========================
#Step 1ai. Obtain Local data sources (archived in this package)
#=========================

data(efidata) #Data extract from EFIPLUS data

data(jdsdata) #Data extract from JDS4 data 

#===================================
#Step 1aii: Retrieve online data for the species: polygon to limit the extent to get records.
#=====================================
danube <- sf::st_read(system.file('extdata', "danube.shp.zip",
                                  package = 'specleanr'), quiet=TRUE)


df_online <- getdata(data = c("Squalius cephalus", 'Salmo trutta', 
                              "Thymallus thymallus","Anguilla anguilla"), 
                                extent = danube,
                                gbiflim = 50, 
                                inatlim = 50, 
                                vertlim = 50, 
                     verbose = FALSE)

dim(df_online)


## ----merging and harmonising species records handling, warning=FALSE----------

mergealldfs <- match_datasets(datasets = list(efi= efidata, jds = jdsdata, 
                                        onlinedata = df_online),
                country = c('JDS4_sampling_ID'),
                lats = 'lat', lons = 'lon',
                species = c('speciesname', 'scientificName'))

#Species names are re-cleaned since the species names from vertnet are changed.

cleannames_df <- check_names(data = mergealldfs, colsp = 'species', pct = 90, 
                             merge = TRUE, verbose = TRUE)

#Filter out species from clean names df where the species names such as synonyms like Salmo trutta fario chnaged to Slamo trutta

speciesfiltered <- cleannames_df[cleannames_df$speciescheck %in%
                                   c("Squalius cephalus", 'Salmo trutta', 
                                     "Thymallus thymallus","Anguilla anguilla"),]


## ----environmental parameters from WORLDCLIM----------------------------------

#Get climatic variables from the package folder

worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package = 'specleanr'))


## ----precleanand, echo=TRUE---------------------------------------------------

#Get basin shapefile to delineate the study region: optional

danube <- sf::st_read(system.file('extdata', 'danube.shp.zip', 
                                  package = 'specleanr'), quiet=TRUE)

#For multiple species indicate multiple TRUE
multipreclened <-  pred_extract(data= speciesfiltered, 
                             raster= worldclim, 
                             lat = 'decimalLatitude',
                             lon = 'decimalLongitude',
                             colsp = 'speciescheck',
                             bbox  = danube,  
                             list= TRUE, 
                             minpts = 10, merge = FALSE)
names(multipreclened)


thymallusdata <- speciesfiltered[speciesfiltered[,'speciescheck'] %in%c("Thymallus thymallus"),]

dim(thymallusdata)

thymallus_referencedata <-  pred_extract(data= thymallusdata, raster= worldclim, 
                             lat = 'decimalLatitude',
                             lon = 'decimalLongitude',
                             colsp = 'speciescheck',
                             bbox  = danube,
                             list= TRUE, 
                             minpts = 10)
dim(thymallus_referencedata)


## ----outlierdetection, echo=TRUE, message=FALSE, warning=FALSE----------------

#For multiple species: default settings

multiple_spp_out_detection <- multidetect(data = multipreclened,
                      multiple = TRUE,
                      var = 'bio6',
                     exclude = c('x','y'),
                      methods = c('zscore', 'adjbox',
                                                'logboxplot', 'distboxplot',
                                                'iqr', 'semiqr',
                                                'hampel','kmeans',
                                                'jknife', 'onesvm',
                                                'iforest'))
#single species:default settings

thymallus_outlier_detection <- multidetect(data = thymallus_referencedata,
                      multiple = FALSE,
                      var = 'bio6',
                      output = 'outlier',
                      exclude = c('x','y'),
                      methods = c('zscore', 'adjbox',
                                  'logboxplot', 'distboxplot',
                                  'iqr', 'semiqr',
                                  'hampel','kmeans',
                                  'jknife', 'onesvm',
                                  'iforest'))


## ----visualisation, warning=FALSE, fig.width = 6, fig.height= 5, fig.align='center'----
#for multiple species
ggoutliers(multiple_spp_out_detection)

#for single species
ggoutliers(thymallus_outlier_detection)


## ----threshold identifcation, fig.width = 6, fig.height= 5, fig.align='center'----

thymallus_opt_threshold <- optimal_threshold(refdata = thymallus_referencedata, 
                               outliers = thymallus_outlier_detection, plot = list(plot = TRUE, group = "Thymallus thymallus"))

#obtain the optimal thresholds for multiple species 

multspp_opt_threshold <- optimal_threshold(refdata = multipreclened, 
                                           outliers = multiple_spp_out_detection)


## ----extract clean dataset----------------------------------------------------

multspecies_clean <- extract_clean_data(refdata = multipreclened, 
                                        outliers = multiple_spp_out_detection, 
                                        loess =  TRUE)
head(multspecies_clean)

thymallus_qcdata <- extract_clean_data(refdata = thymallus_referencedata, 
                             outliers = thymallus_outlier_detection, 
                             loess = TRUE)


multiple_spp_qcdata <- classify_data(refdata = multipreclened, 
                                outliers = multiple_spp_out_detection, 
                                EIF = TRUE)
head(multiple_spp_qcdata)


thymallus_qc_labelled <- classify_data(refdata = thymallus_referencedata, 
                              outliers = thymallus_outlier_detection, 
                              EIF = TRUE)
head(thymallus_qc_labelled)


## ----2d plots multiple species, fig.width = 7.5, fig.height= 5.2, fig.align='center'----

#multiple species 
ggenvironmentalspace(qcdata = multiple_spp_qcdata, 
                     xvar = 'bio1', 
                     yvar = "bio18", 
                     xlab = "Annual mean temperature",
                     ylab = "Precipitation of Warmest Quarter",
                     scalecolor = 'viridis',
                     ncol = 2, 
                     nrow = 2,
                     pointsize = 2)


## ----2d plots single species, fig.width = 5.4, fig.height= 4.2, fig.align='center'----

#for single species
ggenvironmentalspace(qcdata = thymallus_qc_labelled,
                     xvar = 'bio1',
                     yvar = "bio18",
                     xlab = "Annual mean temperature",
                     ylab = "Precipitation of Warmest Quarter",
                     scalecolor = 'viridis',
                     pointsize = 2)


## ----bootstrappingoutlier detection-------------------------------------------

thymallus_outlier_boot <- multidetect(data = thymallus_referencedata,
                      multiple = FALSE,
                      var = 'bio6',
                      exclude = c('x','y'),
                      methods = c('zscore', 'adjbox',
                                  'logboxplot', 'distboxplot',
                                  'iqr', 'semiqr',
                                  'hampel','kmeans',
                                  'jknife', 'onesvm',
                                  'iforest'),
                      bootSettings = list(run = TRUE, maxrecords = 100, nb = 10))


## ----visualisationboot, fig.align='center', fig.width = 5.4, fig.height= 4.2, warning=FALSE, dpi=400----

ggoutliers(thymallus_outlier_boot)


## ----classifyboot, warning=FALSE----------------------------------------------

thymallus_qc_label_boot <- classify_data(refdata = thymallus_referencedata, 
                                outliers = thymallus_outlier_boot)


## ----ggspaceboot, warning=FALSE, fig.width = 6, fig.height= 3.6, fig.align='center', dpi=400----

ggenvironmentalspace(qcdata = thymallus_qc_label_boot, 
                     xvar = 'bio1', 
                     yvar = "bio18",
                     xlab = "Annual mean temperature",
                     ylab = "Precipitation of Warmest Quarter",
                     scalecolor = 'viridis',
                     pointsize = 2)


## ----bootpcaoutlier detection-------------------------------------------------

thymallus_outlier_boot_pca <- multidetect(data = thymallus_referencedata,
                      multiple = FALSE,
                      var = 'bio6',
                      exclude = c('x','y'),
                      methods = c('zscore', 'adjbox',
                                  'logboxplot', 'distboxplot',
                                  'iqr', 'semiqr',
                                  'hampel','kmeans',
                                  'jknife', 'onesvm',
                                  'iforest'),
                      bootSettings = list(run = TRUE, maxrecords = 100, nb = 10),
                      pc = list(exec = TRUE, npc = 6, q = FALSE))


## ----visualisationbootpca, fig.align='center', fig.width = 5.4, fig.height= 4.2, warning=FALSE, dpi=400----

ggoutliers(thymallus_outlier_boot_pca)


## ----classifybootpca, warning=FALSE-------------------------------------------

thymallus_qc_label_boot_pca <- classify_data(refdata = thymallus_referencedata, 
                                outliers = thymallus_outlier_boot_pca)


## ----ggspacebootpca, warning=FALSE, fig.width = 6, fig.height= 3.6, fig.align='center', dpi=400----

ggenvironmentalspace(qcdata = thymallus_qc_label_boot_pca, 
                     xvar = 'bio1', 
                     yvar = "bio18",
                     xlab = "Annual mean temperature",
                     ylab = "Precipitation of Warmest Quarter",
                     scalecolor = 'viridis',
                     pointsize = 2)


