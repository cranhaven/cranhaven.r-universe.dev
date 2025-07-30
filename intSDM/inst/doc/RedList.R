## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  eval = FALSE,
  fig.width=8, 
  fig.height=5,
  warning = FALSE,
  message = FALSE)

## ----load packages------------------------------------------------------------
#  
#  library(intSDM)
#  library(INLA)
#  

## ----initialize workflow------------------------------------------------------
#  
#  proj <- '+proj=tmerc +lat_0=0 +lon_0=15 +k=0.9996 +datum=WGS84 +units=km +x_0=500 +y_0=0 +no_defs'
#  #proj <- '+proj=utm +zone=32 +datum=WGS84 +units=km +no_defs +type=crs'
#  
#  workflow <- startWorkflow(
#          Projection = proj,
#          Species = c("Fraxinus_excelsior", "Ulmus_glabra", "Arnica_montana"),
#          saveOptions = list(projectName =  'Vascular'), Save = FALSE
#          )
#  

## ----addArea------------------------------------------------------------------
#  
#  Norway <- fm_transform(giscoR::gisco_get_countries(country = 'Norway', resolution = 60),
#                         proj)
#  Norway <- st_cast(st_as_sf(Norway), 'POLYGON')
#  Norway <- Norway[which.max(st_area(Norway)),]
#  Norway <- rmapshaper::ms_simplify(Norway, keep = 0.8)
#  Norway <- st_as_sf(fm_extensions(Norway, convex = c(10, 20))[[1]])
#  
#  workflow$addArea(Object = Norway)
#  workflow$plot()
#  

## ----addGBIF------------------------------------------------------------------
#  
#  workflow$addGBIF(datasetName = 'CZ',
#                   datasetType = 'PO',
#                   coordinateUncertaintyInMeters = '0,100',
#                   limit = 10000,
#                   datasetKey = 'b124e1e0-4755-430f-9eab-894f25a9b59c')
#  
#  workflow$addGBIF(datasetName = 'UiO',
#                   datasetType = 'PA',
#                   limit = 10000,
#                   coordinateUncertaintyInMeters = '0,100',
#                   generateAbsences = TRUE,
#                   datasetKey = 'e45c7d91-81c6-4455-86e3-2965a5739b1f')
#  
#  workflow$addGBIF(datasetName = 'NTNU',
#                   datasetType = 'PA',
#                   limit = 10000,
#                   coordinateUncertaintyInMeters = '0,100',
#                   generateAbsences = TRUE,
#                   datasetKey = 'd29d79fd-2dc4-4ef5-89b8-cdf66994de0d')
#  
#  workflow$plot(Species = TRUE)
#  

## ----addCovariates, eval = FALSE----------------------------------------------
#  
#  workflow$addCovariates(worldClim = 'tavg', res = 2.5, Function = scale)
#  
#  workflow$addCovariates(landCover = 'grassland', Function = scale)
#  
#  workflow$plot(Covariates = TRUE)
#  

## ----metadata-----------------------------------------------------------------
#  
#  workflow$obtainMeta()
#  

## ----INLA---------------------------------------------------------------------
#  
#  workflow$addMesh(cutoff = 20 * 0.5,
#                   max.edge = c(60, 180) * 0.5,
#                   offset= c(30, 250))
#  
#  workflow$plot(Mesh = TRUE)
#  

## ----Priors-------------------------------------------------------------------
#  
#  workflow$specifySpatial(prior.range = c(200, 0.2),
#                          prior.sigma = c(1, 0.01), constr = TRUE)
#  

## ----Fixed priors-------------------------------------------------------------
#  
#  workflow$specifyPriors(effectNames = 'Intercept',
#                         Mean = 0, Precision = 1)
#  
#  workflow$specifyPriors('tavg', Mean = 0, Precision = 1)
#  
#  workflow$specifyPriors('grassland', Mean = 0, Precision = 1)
#  

## ----Bias---------------------------------------------------------------------
#  
#  workflow$biasFields('CZ',
#                      prior.range = c(200, 0.2),
#                      prior.sigma = c(1, 0.01))
#  

## ----specPrior----------------------------------------------------------------
#  
#  workflow$specifyPriors(copyModel = list(beta = list(fixed = TRUE)))
#  

## ----options------------------------------------------------------------------
#  
#  workflow$workflowOutput(c('Maps', 'Model', 'Bias'))
#  

## ----Maps---------------------------------------------------------------------
#  
#  Maps <- sdmWorkflow(workflow,inlaOptions = list(num.threads = 1,
#                                                  control.inla=list(int.strategy = 'ccd',
#                                                      h = 1e-4,
#                                                      cmin = 0,
#                                                      control.vb=list(enable = FALSE)),
#                                    safe = TRUE,
#                                    verbose = TRUE,
#                                    inla.mode = 'experimental'),
#                predictionDim = c(400, 400),
#                ipointsOptions = list(method = 'direct'))

## ----MapsOut------------------------------------------------------------------
#  
#  Maps$Fraxinus_excelsior$Maps
#  Maps$Ulmus_glabra$Maps
#  Maps$Arnica_montana$Maps
#  

## ----model Summaries----------------------------------------------------------
#  
#  lapply(Maps, function(x) x$Model)
#  

