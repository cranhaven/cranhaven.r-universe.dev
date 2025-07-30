## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE,
  fig.width=8, 
  fig.height=5,
  warning = FALSE,
  message = FALSE)

## ----setup--------------------------------------------------------------------
#  
#  library(intSDM)
#  library(USAboundaries)
#  

## ----get data-----------------------------------------------------------------
#  
#  data("SetophagaData")
#  BBA <- SetophagaData$BBA
#  BBA$Species_name <- paste0('Setophaga_', BBA$Species_name)
#  BBS <- SetophagaData$BBS
#  BBS$Species_name <- paste0('Setophaga_', BBS$Species_name)
#  

## ----startWorkflow------------------------------------------------------------
#  
#  workflow <- startWorkflow(Richness = FALSE,
#    Projection = "+proj=utm +zone=17 +datum=WGS84 +units=km",
#    Species = c("Setophaga_caerulescens"),
#                #"Setophaga_fusca", "Setophaga_magnolia"),
#    saveOptions = list(projectName =  'Setophaga'), Save = FALSE
#  )
#  

## ----addArea------------------------------------------------------------------
#  
#  workflow$addArea(Object = USAboundaries::us_states(states = "Pennsylvania"))
#  

## ----download Data------------------------------------------------------------
#  
#  workflow$addGBIF(datasetName = 'eBird', datasetType = 'PO', limit = 5000,
#                   datasetKey = '4fa7b334-ce0d-4e88-aaae-2e0c138d049e',
#                   year = '2005,2009')
#  
#  workflow$addStructured(dataStructured = BBS, datasetType = 'Counts',
#                         responseName = 'Counts',
#                         speciesName = 'Species_name')
#  
#  workflow$addStructured(dataStructured = BBA, datasetType = 'PA',
#                         responseName = 'NPres',
#                         speciesName = 'Species_name')
#  
#  workflow$plot(Species = TRUE)
#  

## ----addCovariates------------------------------------------------------------
#  
#  covariates <- scale(terra::rast(system.file('extdata/SetophagaCovariates.tif',
#                                        package = "PointedSDMs")))
#  names(covariates) <- c('elevation', 'canopy')
#  
#  workflow$addCovariates(Object = covariates)
#  
#  workflow$plot(Covariates = TRUE)
#  

## ----biasFields---------------------------------------------------------------
#  
#  workflow$addMesh(cutoff = 0.2 * 5,
#                   max.edge = c(0.1, 0.24) * 80,
#                   offset = c(0.1, 0.4) * 100)
#  
#  workflow$plot(Mesh = TRUE)
#  

## ----speciyRandom-------------------------------------------------------------
#  
#  workflow$specifySpatial(prior.range = c(30, 0.1),
#                          prior.sigma = c(1, 0.1))
#  
#  workflow$biasFields(datasetName = 'eBird',
#                      prior.range = c(15, 0.1),
#                      prior.sigma = c(1, 0.1))
#  
#  workflow$specifyPriors(effectNames = 'Intercept',
#                         Mean = 0,
#                         Precision = 1)
#  

## ----outcomes-----------------------------------------------------------------
#  
#  workflow$workflowOutput(c('Model', 'Cross-validation'))
#  
#  workflow$crossValidation(Method = 'spatialBlock',
#                           blockOptions = list(k = 4,
#                                               rows_cols = c(20, 20),
#                                               plot = TRUE, seed = 123),
#                           blockCVType = "Predict")
#  

## ----sdmWorkflow--------------------------------------------------------------
#  
#  Model <- sdmWorkflow(Workflow = workflow,
#                        inlaOptions = list(control.inla=list(int.strategy = 'eb',
#                                                      diagonal = 0.1,
#                                                      cmin = 0),
#                                    safe = TRUE,
#                                    verbose = TRUE,
#                                    inla.mode = 'experimental'))
#  

## ----plot int-----------------------------------------------------------------
#  
#  Model[[1]]$Model
#  

## ----plot bias----------------------------------------------------------------
#  
#  Model[[1]]$spatialBlock
#  

