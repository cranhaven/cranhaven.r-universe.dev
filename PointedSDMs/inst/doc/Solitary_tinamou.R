## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  eval = FALSE
)


## ----setup, warning = FALSE, message = FALSE----------------------------------
#  
#  library(PointedSDMs)
#  library(terra)
#  library(INLA)
#  library(ggplot2)
#  

## ----safe, include = FALSE----------------------------------------------------
#  
#  bru_options_set(inla.mode = "experimental")
#  

## ----load data----------------------------------------------------------------
#  
#  data('SolitaryTinamou')
#  projection <- "+proj=longlat +ellps=WGS84"
#  
#  covariates <- terra::rast(system.file('extdata/SolitaryTinamouCovariates.tif',
#                                        package = "PointedSDMs"))
#  
#  datasets <- SolitaryTinamou$datasets
#  region <- st_as_sf(SolitaryTinamou$region)
#  mesh <- SolitaryTinamou$mesh
#  

## ----look at data-------------------------------------------------------------
#  
#  str(datasets)
#  class(region)
#  

## ----covariates, fig.width=8, fig.height=5------------------------------------
#  
#  covariates <- scale(covariates)
#  crs(covariates) <- projection
#  plot(covariates)
#  

## ----mesh, fig.width=8, fig.height=5------------------------------------------
#  
#  ggplot() + gg(mesh)
#  

## ----set up base model, warning = FALSE, message = FALSE----------------------
#  
#  base <- startISDM(datasets, spatialCovariates = covariates, Boundary = region,
#                   Projection = projection, responsePA = 'Present', Offset = 'area',
#                   Mesh = mesh, pointsSpatial = NULL)
#  

## ----data, fig.width=8, fig.height=5------------------------------------------
#  
#  base$plot(Boundary = FALSE) +
#    geom_sf(data = st_boundary(region)) +
#    ggtitle('Plot of the species locations by dataset')
#  

## ----priorsFixed--------------------------------------------------------------
#  
#  base$priorsFixed(Effect = 'Forest', mean.linear = 0.5, prec.linear = 0.01)
#  

## ----run base model, warning = FALSE, message = FALSE-------------------------
#  
#  baseModel <- fitISDM(data = base)
#  summary(baseModel)
#  

## ----set up model with fields, warning = FALSE, message = FALSE---------------
#  
#  fields <- startISDM(datasets, spatialCovariates = covariates, Boundary = region,
#                     Projection = projection, Mesh = mesh, responsePA = 'Present')
#  
#  fields$priorsFixed(Effect = 'Intercept', prec.linear = 1)
#  
#  for (cov in names(covariates)) fields$priorsFixed(Effect = cov, prec.linear = 1)
#  

## ----specifySpatial-----------------------------------------------------------
#  
#  fields$specifySpatial(sharedSpatial = TRUE,
#                        constr = TRUE,
#                        prior.range = c(3, 0.1),
#                        prior.sigma = c(1, 0.1))
#  

## ----run fields model, warning = FALSE, message = FALSE-----------------------
#  
#  fieldsModel <- fitISDM(fields, options = list(control.inla =
#                                                  list(int.strategy = 'eb')))
#  summary(fieldsModel)
#  

## ----correlate model----------------------------------------------------------
#  
#  correlate <- startISDM(datasets, Boundary = region,
#                   Projection = projection, Mesh = mesh,
#                   spatialCovariates = covariates$Altitude,
#                   responsePA = 'Present',
#                   pointsSpatial = 'correlate')
#  
#  correlate$priorsFixed(Effect = 'Intercept', prec.linear = 1)
#  
#  correlate$priorsFixed(Effect = 'Altitude', prec.linear = 1)
#  
#  correlate$specifySpatial(sharedSpatial = TRUE,
#                           prior.range = c(3, 0.1),
#                           prior.sigma = c(1, 0.1))
#  
#  correlate$changeComponents()
#  

## ----addBias------------------------------------------------------------------
#  
#  correlate$addBias('eBird')
#  
#  correlate$specifySpatial(Bias = TRUE,
#                        prior.range = c(2, 0.1),
#                        prior.sigma = c(0.1, 0.1))
#  

## ----run correlate model------------------------------------------------------
#  
#  correlateModel <- fitISDM(correlate,
#                            options = list(control.inla =
#                                             list(int.strategy = 'eb')))
#  summary(correlateModel)
#  

## ----predict spatial, warning = FALSE, message = FALSE------------------------
#  
#  spatial_predictions <- predict(correlateModel, mesh = mesh,
#                         mask = region,
#                         spatial = TRUE,
#                         fun = 'linear')
#  

## ----spatial, fig.width=8, fig.height=5---------------------------------------
#  
#  plot(spatial_predictions, variable = c('mean', 'sd'))
#  

## ----predict bias, warning = FALSE, message = FALSE---------------------------
#  
#  bias_predictions <- predict(correlateModel,
#                      mesh = mesh,
#                      mask = region,
#                      bias = TRUE,
#                      fun = 'linear')
#  

## ----bias, fig.width=8, fig.height=5------------------------------------------
#  
#  plot(bias_predictions)
#  

## ----datasetOut, warning = FALSE, message = FALSE-----------------------------
#  
#  eBird_out <- datasetOut(model = correlateModel, dataset = 'eBird')
#  

## ----print datasetOut---------------------------------------------------------
#  
#  eBird_out
#  

