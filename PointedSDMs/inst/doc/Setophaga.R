## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE,
  fig.width=8, fig.height=5
)


## ----Install PointedSDMs, warning = FALSE, message = FALSE, eval = TRUE-------

##Install if need be
library(PointedSDMs)


## ----startISDM----------------------------------------------------------------
#  
#  args(startISDM)
#  

## ----startSpecies-------------------------------------------------------------
#  
#  args(startSpecies)
#  

## ----fitISDM------------------------------------------------------------------
#  
#  args(fitISDM)
#  

## ----args for blockedCV-------------------------------------------------------
#  
#  args(blockedCV)
#  

## ----datasetOut---------------------------------------------------------------
#  
#  args(datasetOut)
#  

## ----Load packages, message=FALSE, warning=FALSE------------------------------
#  
#  library(INLA)
#  library(inlabru)
#  library(USAboundaries)
#  library(sf)
#  library(blockCV)
#  library(ggplot2)
#  library(sn)
#  library(terra)
#  library(RColorBrewer)
#  library(cowplot)
#  library(knitr)
#  library(dplyr)
#  library(spocc)
#  

## ----Map of PA----------------------------------------------------------------
#  
#  proj <- "+proj=utm +zone=17 +datum=WGS84 +units=km"
#  
#  PA <- USAboundaries::us_states(states = "Pennsylvania")
#  
#  PA <- st_transform(PA, proj)
#  

## ----get_eBird----------------------------------------------------------------
#  
#  species <- c('caerulescens', 'fusca', 'magnolia')
#  
#  dataSets <- list()
#  for (bird in species) {
#  
#    raw_data <- spocc::occ(
#                query = paste('Setophaga', bird),
#                from = "gbif",
#                date = c("2005-01-01", "2005-12-31"),
#                geometry = st_bbox(st_transform(PA,
#                  '+proj=longlat +datum=WGS84 +no_defs')))$gbif
#  
#    rows <- grep("EBIRD", raw_data$data[[paste0('Setophaga_', bird)]]$collectionCode)
#  
#    raw_data <- data.frame(raw_data$data[[1]][rows, ])
#    raw_data$Species_name <- rep(bird, nrow(raw_data))
#  
#    data_sp <- st_as_sf(
#      x = raw_data[, names(raw_data) %in% c("longitude", "latitude", 'Species_name')],
#      coords = c('longitude', 'latitude'),
#      crs = '+proj=longlat +datum=WGS84 +no_defs')
#    data_sp <- st_transform(data_sp, proj)
#  
#    dataSets[[paste0('eBird_', bird)]] <- data_sp[unlist(st_intersects(PA, data_sp)),]
#  
#    }
#  

## ----Load points--------------------------------------------------------------
#  
#  data('SetophagaData')
#  dataSets[['BBA']] <- SetophagaData$BBA
#  dataSets[['BBS']] <- SetophagaData$BBS
#  

## ----Covariate data, message = FALSE, warning = FALSE-------------------------
#  
#  covariates <- terra::rast(system.file('extdata/SetophagaCovariates.tif',
#                                        package = "PointedSDMs"))
#  
#  values(covariates$PA_lc_NLCD_2011_Tree_Canopy_L48_nlcd)[is.na(values(covariates$PA_lc_NLCD_2011_Tree_Canopy_L48_nlcd))] <- 0
#  
#  covariates <- scale(covariates)
#  
#  names(covariates) <- c('elevation', 'canopy')
#  
#  plot(covariates)
#  

## ----Mesh, warning = FALSE, message = FALSE, fig.width=8, fig.height=5--------
#  
#  mesh <- fm_mesh_2d_inla(boundary = inla.sp2segment(PA),
#                          cutoff = 0.2 * 5,
#                          max.edge = c(0.1, 0.24) * 80, #40 #120
#                          offset = c(0.1, 0.4) * 100,
#                          crs = st_crs(proj))
#  
#  mesh_plot <- ggplot() +
#               gg(mesh) +
#               ggtitle('Plot of mesh') +
#               theme_bw() +
#               theme(plot.title = element_text(hjust = 0.5))
#  mesh_plot
#  

## ----modelOptions-------------------------------------------------------------
#  
#  modelOptions <- list(control.inla =
#                         list(int.strategy = 'ccd',
#                              cmin = 0),
#                              verbose = TRUE,
#                              safe = TRUE)
#  

## ----Model prep, warning = FALSE, message = FALSE-----------------------------
#  
#  caerulescensData <- dataSets[c(1,4,5)]
#  
#  caerulescensModel <- startISDM(caerulescensData, Boundary = PA,
#                            Projection = proj, Mesh = mesh,
#                            responsePA = 'NPres', responseCounts = 'Counts',
#                            spatialCovariates = covariates,
#                            Formulas =
#                            list(
#            covariateFormula = ~ elevation*canopy)
#                               )
#  

## ----help, eval = FALSE-------------------------------------------------------
#  
#  caerulescensModel$help()
#  

## ----dataset plot, fig.width=8, fig.height=5----------------------------------
#  
#  caerulescensModel$plot() +
#    theme_bw() +
#    ggtitle('Plot of the datasets') +
#    theme(plot.title = element_text(hjust = 0.5))
#  

## ----specifySpatial-----------------------------------------------------------
#  
#  caerulescensModel$specifySpatial(sharedSpatial = TRUE,
#                                   constr = TRUE,
#                                   prior.sigma = c(0.1, 0.05),
#                                   prior.range = c(200, 0.1))
#  

## ----bias fields--------------------------------------------------------------
#  
#  caerulescensModel$addBias(datasetNames = 'eBird_caerulescens')
#  
#  caerulescensModel$specifySpatial(Bias = TRUE,
#                                   prior.sigma = c(0.1, 0.05),
#                                   prior.range = c(120, 0.1))
#  

## ----priorsFixed--------------------------------------------------------------
#  
#  caerulescensModel$priorsFixed(Effect = 'Intercept',
#                                mean.linear = 0,
#                                prec.linear = 1)
#  

## ----changeComponents---------------------------------------------------------
#  
#  caerulescensModel$changeComponents()
#  

## ----specifyRandom------------------------------------------------------------
#  
#  caerulescensModel$specifyRandom(copyModel = list(beta = list(fixed = TRUE)))
#  
#  caerulescensModel$changeComponents()
#  
#  

## ----fitISDM run--------------------------------------------------------------
#  
#  caerulescensEst <- fitISDM(data = caerulescensModel,
#                     options = modelOptions)
#  
#  summary(caerulescensEst)
#  

## ----predict and plot---------------------------------------------------------
#  
#  caerulescensPredictions <- predict(caerulescensEst,
#                                     data = fm_pixels(mesh = mesh,
#                                                      mask = PA),
#                                     spatial = TRUE,
#                                     n.samples = 100)
#  
#  plot(caerulescensPredictions, variable = c('mean', 'sd'))
#  
#  caerulescensBias <- predict(caerulescensEst,
#                                     data = fm_pixels(mesh = mesh,
#                                                      mask = PA),
#                                     bias = TRUE,
#                                     n.samples = 100)
#  
#  plot(caerulescensBias, variable = c('mean', 'sd'))
#  
#  
#  

## ----startSpeciesStart--------------------------------------------------------
#  
#  speciesModel <- startSpecies(dataSets, Boundary = PA,
#                               pointsSpatial = NULL,
#                               Projection = proj, Mesh = mesh,
#                               responsePA = 'NPres', responseCounts = 'Counts',
#                               spatialCovariates = covariates,
#                               speciesName = 'Species_name')
#  

## ----species help, eval = FALSE-----------------------------------------------
#  
#  speciesModel$help()
#  

## ----specifySpecies-----------------------------------------------------------
#  
#  speciesModel$specifySpatial(Species = TRUE,
#                              constr = TRUE,
#                              prior.sigma = c(0.01, 0.05),
#                              prior.range = c(100, 0.1))
#  
#  speciesModel$priorsFixed(Effect = 'Intercept',
#                           mean.linear = 0,
#                           prec.linear = 1)
#  
#  speciesModel$specifyRandom(speciesGroup = list(model = "iid",
#                                                 hyper = list(
#                                                   prec = list(
#                                                   initial = log(1),
#                                                   fixed = TRUE
#                                                   ))),
#                             speciesIntercepts = list(
#                               initial = log(1), fixed  = TRUE
#                             ))
#  

## ----fitSpecies---------------------------------------------------------------
#  
#  modelOptionsSpecies <- modelOptions
#  modelOptionsSpecies$control.inla$h <- 5e-4
#  modelOptionsSpecies$control.inla$tolerance <- 5e-5
#  
#  speciesEst <- fitISDM(data = speciesModel,
#                        options = modelOptionsSpecies)
#  
#  summary(speciesEst)
#  

## ----predictionsSpecies-------------------------------------------------------
#  
#  speciesPredictions <- predict(speciesEst,
#                                     data = fm_pixels(mesh = mesh,
#                                                      mask = PA),
#                                     spatial = TRUE,
#                                     n.samples = 100)
#  
#  plot(speciesPredictions)
#  

## ----spatialBlock, warning = FALSE, message = FALSE,  fig.width=8, fig.height=5----
#  
#  caerulescensModel$spatialBlock(k = 4, rows_cols = c(50, 50),
#                                 plot = TRUE, seed = 123) + theme_bw()
#  

## ----blockedCV, warning = FALSE-----------------------------------------------
#  
#  spatialBlocked <- blockedCV(data = caerulescensModel,
#                              options = modelOptions)
#  

## ----print spatialBlocked-----------------------------------------------------
#  
#  spatialBlocked
#  

## ----No fields model, message = FALSE, warning = FALSE------------------------
#  
#  no_fields <- startISDM(caerulescensData,
#                        pointsSpatial = NULL,
#                        Boundary = PA,
#                        Projection = proj, Mesh = mesh,
#                        responsePA = 'NPres', responseCounts = 'Counts',
#                        spatialCovariates = covariates,
#                        Formulas =
#                            list(
#            covariateFormula = ~ elevation*canopy)
#            )
#  
#  no_fields$spatialBlock(k = 4, rows_cols = c(50, 50),
#                         plot = TRUE, seed = 123) + theme_bw()
#  

## ----spatialBlocked_no_fields-------------------------------------------------
#  
#  spatialBlocked_no_fields <- blockedCV(data = no_fields,
#                                        options = modelOptions)
#  

## ----print spatialBlocked_no_fields-------------------------------------------
#  
#  spatialBlocked_no_fields
#  

## ----Leave one out, message = FALSE, warning = FALSE--------------------------
#  
#  dataset_out <- datasetOut(model = caerulescensEst,
#                            dataset = "BBA",
#                            predictions = TRUE)
#  
#  dataset_out
#  

