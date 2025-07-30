## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE,
  fig.width=8, 
  fig.height=5,
  warning = FALSE,
  message = FALSE)

## ----Load packages------------------------------------------------------------
#  
#  library(intSDM)
#  library(INLA)
#  

## ----startWorkflow------------------------------------------------------------
#  
#  Rich <- startWorkflow(Species = c("Lolium perenne L.","Rubus caesius L.",
#                                    "Rosa spinosissima L.",
#                                    "Poa trivialis L.",
#                                    "Galium verum L.",
#                                    "Tanacetum vulgare L.",
#                                    "Viola tricolor L.",
#                                    "Epilobium L."),
#          Projection =  '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=km +no_defs',
#                        Save = FALSE, Richness = TRUE,
#                        saveOptions = list(projectName = 'Richness'))
#  

## ----addArea------------------------------------------------------------------
#  
#  Ned <- giscoR::gisco_get_countries(country = 'Netherlands', resolution = 60)
#  Ned <- st_cast(st_as_sf(Ned), 'POLYGON')
#  Ned <- Ned[which.max(st_area(Ned)),]
#  Ned <- rmapshaper::ms_simplify(Ned, keep = 0.5)
#  Rich$addArea(Ned)
#  

## ----addGBIF------------------------------------------------------------------
#  
#  Rich$addGBIF(datasetName = 'DVD',
#               datasetKey = '740df67d-5663-41a2-9d12-33ec33876c47',
#               datasetType = 'PA', generateAbsences = TRUE)
#  
#  Rich$addGBIF(datasetName = 'iNat',
#               datasetKey = '50c9509d-22c7-4a22-a47d-8c48425ef4a7')
#  

## ----Mesh---------------------------------------------------------------------
#  
#  Rich$addMesh(max.edge = c(5, 10))
#  Rich$plot(Mesh = TRUE)
#  

## ----priors-------------------------------------------------------------------
#  
#  Rich$specifySpatial(prior.range = c(0.2,0.1),
#                      prior.sigma = c(2, 0.1))
#  
#  Rich$biasFields('iNat', prior.range = c(0.1, 0.1),
#                  prior.sigma = c(0.2, 0.1))
#  
#  Rich$specifyPriors(priorIntercept = list(prior = 'pc.prec', param = c(0.02, 0.01)))
#  

## ----modelFormula-------------------------------------------------------------
#  
#  Rich$addCovariates(worldClim = c('tavg'), res = 10, Function = scale)
#  Rich$modelFormula(covariateFormula = ~ tavg + I(tavg^2))
#  

## ----specRich-----------------------------------------------------------------
#  
#  Rich$modelOptions(ISDM = list(Offset = 'sampleSizeValue'),
#                    Richness = list(predictionIntercept = 'DVD'))
#  

## ----workflow-----------------------------------------------------------------
#  
#  Rich$workflowOutput('Maps')
#  
#  RichModel <- sdmWorkflow(Rich, inlaOptions = list(verbose = TRUE))
#  

## ----Rich---------------------------------------------------------------------
#  
#  ggplot() + gg(RichModel$Richness$Richness, aes(col = q0.025))
#  ggplot() + gg(RichModel$Richness$Richness, aes(col = q0.5))
#  ggplot() + gg(RichModel$Richness$Richness, aes(col = q0.975))
#  

## ----prob---------------------------------------------------------------------
#  
#  ggplot() + gg(RichModel$Richness$Probabilities$Galium_verum_L., aes(col = mean))
#  ggplot() + gg(RichModel$Richness$Probabilities$Tanacetum_vulgare_L., aes(col = mean))
#  
#  

