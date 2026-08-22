## ----setup1, include = FALSE--------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, results='hide', warning=FALSE, message=FALSE----------------------
library(terra)
library(dplyr)
library(sf)
library(roads)

## colours for displaying weight raster 
if(requireNamespace("viridis", quietly = TRUE)){
  # Use colour blind friendly palette if available
  rastColours <- c('grey50', viridis::viridis(20))
} else {
  rastColours <- c('grey50', terrain.colors(20))
}

# terra objects need to be wrapped to be saved, this unwraps them
CLUSexample <- prepExData(CLUSexample)

## -----------------------------------------------------------------------------
weightRaster <- CLUSexample$cost

## ----fig.show='hold'----------------------------------------------------------
## existing roads network
roadsLine <- sf::st_sfc(geometry = sf::st_linestring(
  matrix(c(0.5, 4.5, 4.5, 4.5),
         ncol = 2, byrow = T) 
)) %>%
  sf::st_as_sf(crs = sf::st_crs(weightRaster))

## ----fig.show='hold', fig.width=6.5, fig.height=4.85--------------------------
## landings as spatial points
landings <- roads::CLUSexample$landings

## plot example scenario
par(omi = c(0,0,0,1.2))
plot(weightRaster, col = rastColours, main = 'Example Scenario')
plot(roadsLine, add = TRUE)
plot(landings, add = TRUE, pch = 19)
legend(x = 7.25, y = 5, legend = c("landings", "roads"), pch = c(19, NA), 
       lwd = c(NA, 1),
       xpd = NA, inset = -0.1, xjust = 1)


## ----fig.show='hold', fig.width=6.5, fig.height=4.85--------------------------
## project new roads using the 'snap' approach
projRoads_snap <- roads::projectRoads(landings, weightRaster, roadsLine,
                                      roadMethod = 'snap')

par(omi = c(0,0,0,1.2))
## plot the weight raster, landings, and roads segments to the landings
plot(weightRaster, col = rastColours, main = "'Snapped' roads")
points(landings, pch = 19)  
plot(projRoads_snap$roads, add = TRUE) 

## update legend
legend(x = 7.25, y = 5, legend = c("landings", "roads"), pch = c(19, NA), 
       lwd = c(NA, 1),
       xpd = NA, inset = -0.1, xjust = 1)

## ----fig.show='hold', fig.width=6.5, fig.height=4.85--------------------------
## project new roads using the 'LCP' approach
projRoads_lcp <- roads::projectRoads(landings, 
                                        weightRaster, 
                                        roadsLine, 
                                        roadMethod = 'lcp')

par(omi = c(0,0,0,1.2))
## plot the weight raster and overlay it with new roads
plot(weightRaster, col = rastColours, main = "'LCP' roads")
plot(projRoads_lcp$roads, add = TRUE)
points(landings, pch = 19) 
## legend
legend(x = 7.25, y = 5, legend = c("landings", "roads"), pch = c(19, NA), 
       lwd = c(NA, 1),
       xpd = NA, inset = -0.1, xjust = 1)

## ----fig.show='hold', fig.width=6.5, fig.height=4.85--------------------------
## project new roads using the 'ILCP' approach
projRoads_ilcp <- roads::projectRoads(landings, 
                                        weightRaster, 
                                        roadsLine, 
                                        roadMethod = 'ilcp')

par(omi = c(0,0,0,1.2))
## plot the weight raster and overlay it with new roads
plot(weightRaster, col = rastColours, main = "'ILCP' roads")
plot(projRoads_ilcp$roads, add = TRUE)
points(landings, pch = 19)  ## landings points
## legend
legend(x = 7.25, y = 5, legend = c("landings", "roads"), pch = c(19, NA), 
       lwd = c(NA, 1),
       xpd = NA, inset = -0.1, xjust = 1)

## ----fig.show='hold', fig.width=6.5, fig.height=4.85--------------------------
## project new roads using the 'ILCP' approach
projRoads_ilcp2 <- roads::projectRoads(st_coordinates(landings)[4:1,], 
                                        weightRaster, 
                                        roadsLine, 
                                        roadMethod = 'ilcp')

par(omi = c(0,0,0,1.2))
## plot the weight raster and overlay it with new roads
plot(weightRaster, col = rastColours, main = "'ILCP' roads")
plot(projRoads_ilcp2$roads, add = TRUE)
points(landings, pch = 19)  ## landings points
## legend
legend(x = 7.25, y = 5, legend = c("landings", "roads"), pch = c(19, NA), 
       lwd = c(NA, 1),
       xpd = NA, inset = -0.1, xjust = 1)

## ----fig.show='hold',  fig.width=6.5, fig.height=4.85-------------------------
## project new roads using the 'MST' approach
projRoads_mst <- roads::projectRoads(landings, 
                                        weightRaster,
                                        roadsLine, 
                                        roadMethod = 'mst')

par(omi = c(0,0,0,1.2))
## plot the weight raster and overlay it with new roads
plot(weightRaster, col = rastColours, main = "'MST' roads")
plot(projRoads_mst$roads, add = TRUE)
points(landings, pch = 19) 
## legend
legend(x = 7.25, y = 5, legend = c("landings", "roads"), pch = c(19, NA), 
       lwd = c(NA, 1),
       xpd = NA, inset = -0.1, xjust = 1)

## ----fig.show='hold', fig.width=7,fig.height=6.5------------------------------
## colours for displaying weight raster
if(requireNamespace("viridis", quietly = TRUE)){
  # Use colour blind friendly palette if available
  rastColours2 <- c('grey50', viridis::viridis(30))
} else {
  rastColours2 <- c('grey50', terrain.colors(30))
}

## scenario 
demoScen <- prepExData(demoScen)
scen <- demoScen[[1]]
## landing sets 1 to 4 of this scenario 
land.pnts <- scen$landings.points[scen$landings.points$set %in% c(1:4),]
## plot the weight raster and landings
par(mar=par('mar')/2)
plot(scen$cost.rast, col = rastColours2, main = 'Cost and landings (by set)')
plot(land.pnts %>% st_geometry(), add = TRUE, pch = 21, cex = 2, bg = 'white')
text(st_coordinates(land.pnts), labels = land.pnts$set, cex = 0.6, adj = c(0.5, 0.3),
     xpd = TRUE)

## ----fig.show='hold', fig.width=7,fig.height=6.86-----------------------------
## project roads for landing sets 1 to 4, with independent one-time simulations
oneTime_sim <- list() ## empty list 
for (i in 1:4){
  oneTime_sim <- c(oneTime_sim,
                       roads::projectRoads(land.pnts[land.pnts$set==i,],
                                              scen$cost.rast,
                                              scen$cost.rast==0,
                                              roadMethod='mst')$roads)
}

## plot
oldpar <- par(mfrow = c(2, 2), mar = par('mar')/2)
for (i in 1:4){
  oneTime_sim[[i]][!oneTime_sim[[i]]] <- NA 
  plot(scen$cost.rast, col = rastColours2, 
       main = paste0('Landings set ', i),
       legend = FALSE)
  plot(oneTime_sim[[i]], add = TRUE, col = "grey50", legend = FALSE)
  plot(st_geometry(land.pnts[land.pnts$set == i, ]), add = TRUE,
       pch = 21, cex = 1.5, bg = 'white')
}


## ----fig.show='hold', fig.width=5,fig.height=4.85-----------------------------
## raster representing the union of completely independent simulations for multiple sets
oneTime_sim <- rast(oneTime_sim)
independent <- any(oneTime_sim, na.rm = TRUE)
## set non-road to NA for display purposes
independent[!independent] <- NA

## plot 
plot(scen$cost.rast, col = rastColours2,
     main = 'Union of independent sim results',
     legend = FALSE)

plot(independent, col = 'grey30', add = TRUE, legend = FALSE)

plot(st_geometry(land.pnts), add = TRUE, pch = 21, cex = 1.5, bg = 'white')

## ----fig.show='hold', fig.width=7, fig.height=9.97----------------------------
## continuing on with demo scenario 1
## landing sets 1 to 4 of this scenario as a raster stack
land.stack <- scen$landings.stack[[1:4]]

# initialize sim list with first landings set
multiTime_sim <- list(projectRoads(land.stack[[1]], scen$cost.rast, 
                                               scen$road.line))

# iterate over landings sets using the sim list from the previous run as input
for (i in 2:nlyr(land.stack)) {
  multiTime_sim <- c(
    multiTime_sim,
    list(projectRoads(sim =  multiTime_sim[[i-1]], landings = land.stack[[i]]))
  ) 
}

par(mfrow = c(3, 2))
par(mar = par('mar')/2)
  plot(scen$cost.rast, col = rastColours2, main = 'Roads at time t = 0', 
       legend = FALSE)
  plot(scen$road.line, col = 'grey30', add = TRUE, legend = FALSE)
  
for (i in 1:length(multiTime_sim)){
  plot(multiTime_sim[[i]]$weightRaster, col = rastColours2, 
       main = paste0('Roads at time t = ', i), legend = FALSE)
  plot(multiTime_sim[[i]]$roads, col = 'grey30', add = TRUE, legend = FALSE)
  plot(st_geometry(land.pnts[land.pnts$set == i, ]), add = TRUE, pch = 21, 
         cex = 1.5, bg = 'white')
  if (i >= 2){
    plot(st_geometry(land.pnts[land.pnts$set < i, ]), add = TRUE, pch = 1, cex = 1.5)
    plot(st_geometry(land.pnts[land.pnts$set == i, ]), add = TRUE, pch = 21, 
         cex = 1.5, bg = 'white')
  }
}

## ----fig.width=6,fig.height=5-------------------------------------------------
harvPoly <- demoScen[[1]]$landings.poly

outCent <- getLandingsFromTarget(harvPoly)
plot(sf::st_geometry(harvPoly))
plot(outCent, col = "red", add = TRUE)

# Get random sample with density 0.02 pts per unit area
outRand <- getLandingsFromTarget(harvPoly, 0.02, sampleType = "random")
prRand <- projectRoads(outRand, scen$cost.rast, scen$road.line)

plot(scen$cost.rast, main = "Random Landings in Harvest Blocks",
     col = rastColours2)
plot(harvPoly, add = TRUE)
plot(prRand$roads, add = TRUE,  col = "grey50")
plot(outRand, col = "red", add = TRUE)

# Get regular sample with density 0.02 pts per unit area
outReg <- getLandingsFromTarget(harvPoly, 0.02, sampleType = "regular")
prReg <- projectRoads(outReg, scen$cost.rast,scen$road.line)


plot(scen$cost.rast, main = "Regular Landings in Harvest Blocks",
     col = rastColours2)
plot(harvPoly, add = TRUE)
plot(prReg$roads, add = TRUE, col = "grey50")
plot(outReg, col = "red", add = TRUE)


# clean up 
par(oldpar)

