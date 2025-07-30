## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE,
  warning = FALSE,
  message = FALSE
)


## ----setup--------------------------------------------------------------------
#  library(spatstat)
#  library(PointedSDMs)
#  library(sf)
#  library(sp)
#  library(ggplot2)
#  library(inlabru)
#  library(INLA)
#  library(fmesher)

## ----load_data----------------------------------------------------------------
#  
#  data(Koala)
#  eucTrees <- Koala$eucTrees
#  boundary <- Koala$boundary
#  

## ----clean_data, echo = TRUE,fig.width=7, fig.height=5------------------------
#  
#  proj <- "+init=epsg:27700"
#  
#  boundary <- as(boundary, 'sf')
#  st_crs(boundary) <- proj
#  
#  euc <- st_as_sf(x = eucTrees,
#                  coords = c('E', 'N'),
#                  crs = proj)
#  
#  euc$food <- euc$FOOD/1000
#  euc <- euc[unlist(st_intersects(boundary, euc)),]
#  
#  class(trees)
#  
#  mesh = fm_mesh_2d_inla(boundary = boundary,
#                         max.edge = c(10, 20),
#                         offset = c(15,20),
#                         cutoff = 2)
#  mesh$crs <- st_crs(proj)
#  
#  ggplot() +
#    geom_sf(data = st_boundary(boundary)) +
#    geom_sf(data = euc, aes(color = koala)) +
#    ggtitle('Plot showing number of koalas at each site')
#  
#  ggplot() +
#    geom_sf(data = st_boundary(boundary)) +
#    geom_sf(data = euc, aes(color = food)) +
#    ggtitle('Plot showing the food value index at each site')
#  
#  

## ----points_only,fig.width=7, fig.height=5------------------------------------
#  
#  points <- startISDM(euc, Boundary = boundary,
#                      Projection = proj,
#                      Mesh = mesh)
#  
#  points$specifySpatial(sharedSpatial = TRUE,
#                        prior.range = c(120, 0.1),
#                        prior.sigma = c(1, 0.1))
#  
#  pointsModel <- fitISDM(points, options = list(control.inla = list(int.strategy = 'eb')))
#  

## ----p and p, fig.width=7, fig.height=5---------------------------------------
#  
#  pointsPredictions <- predict(pointsModel, mask = boundary,
#                               mesh = mesh, predictor = TRUE)
#  
#  pointsPlot <- plot(pointsPredictions, variable = 'mean',
#                     plot = FALSE)
#  
#  pointsPlot$predictions$predictions$mean +
#    gg(euc)
#  

## ----include_marks,fig.width=7, fig.height=5----------------------------------
#  
#  marks <- startMarks(euc, Boundary = boundary,
#                      Projection = proj,
#                      markNames = c('food', 'koala'),
#                      markFamily = c('gamma', 'poisson'),
#                      Mesh = mesh)
#  
#  marks$specifySpatial(sharedSpatial = TRUE,
#                        prior.range = c(120, 0.1),
#                        prior.sigma = c(1, 0.1))
#  
#  marks$specifySpatial(Mark = 'koala',
#                       prior.range = c(120, 0.1),
#                       prior.sigma = c(1, 0.1))
#  
#  marks$specifySpatial(Mark = 'food',
#                       prior.range = c(60, 0.1),
#                       prior.sigma = c(1, 0.1))
#  
#  marksModel <- fitISDM(marks, options = list(control.inla = list(int.strategy = 'eb'),
#                                               safe = TRUE))

## ----p and p 2,fig.width=7, fig.height=5--------------------------------------
#  
#  foodPredictions <- predict(marksModel, mask = boundary,
#                             mesh = mesh, marks = 'food', spatial = TRUE,
#                             fun = 'exp')
#  
#  koalaPredictions <- predict(marksModel, mask = boundary,
#                              mesh = mesh, marks = 'koala', spatial = TRUE)
#  
#  plot(foodPredictions, variable = c('mean', 'sd'))
#  plot(koalaPredictions, variable = c('mean', 'sd'))
#  

## ----marks_add_scaling,fig.width=7, fig.height=5------------------------------
#  
#  marks2 <- startMarks(euc, Boundary = boundary,
#                       Projection = proj,
#                       markNames = 'food',
#                       markFamily = 'gaussian',
#                       Mesh = mesh)
#  
#  marks2$updateFormula(Mark = 'food',
#        newFormula = ~ exp(food_intercept + (shared_spatial + 1e-6)*scaling + food_spatial))
#  
#  marks2$changeComponents(addComponent = 'scaling(1)')
#  
#  marks2$specifySpatial(sharedSpatial = TRUE,
#                        prior.sigma = c(1, 0.01),
#                        prior.range = c(120, 0.01))
#  
#  marks2$specifySpatial(Mark = 'food',
#                        prior.sigma = c(1, 0.01),
#                        prior.range = c(120, 0.01))
#  
#  marksModel2 <- fitISDM(marks2, options = list(control.inla = list(int.strategy = 'eb'),
#                                                bru_max_iter = 2, safe = TRUE))
#  
#  predsMarks2 <- predict(marksModel2, mask = boundary, mesh = mesh,
#      formula =  ~ (food_intercept + (shared_spatial + 1e-6)*scaling + food_spatial))
#  
#  plot(predsMarks2)

