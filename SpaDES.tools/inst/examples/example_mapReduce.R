library(data.table)
library(terra)

origDTThreads <- data.table::setDTthreads(2L)
origNcpus <- options(Ncpus = 2L)

ras <- rast(ext(0, 15, 0, 15), res = 1)
fullRas <- randomPolygons(ras, numTypes = 2)
names(fullRas) <- "mapcodeAll"
uniqueComms <- unique(fullRas)
reducedDT <- data.table(uniqueComms,
                        communities = sample(1:1000, length(uniqueComms)),
                        biomass = rnbinom(length(uniqueComms), mu = 4000, 0.4))
biomass <- rasterizeReduced(reducedDT, fullRas, "biomass")

# The default key is the layer name of the fullRas, so rekey incase of miskey
setkey(reducedDT, biomass)

communities <- rasterizeReduced(reducedDT, fullRas, "communities")
coltab(communities) <- c("blue", "orange", "red")
if (interactive()) {
  terra::plot(c(biomass, communities, fullRas))
}

## with a factor SpatRaster, the mapcode should correspond to the
## active category (not the ids)
cls <- data.frame(id = sort(unique(as.vector(fullRas[]))))
cls$Bclass <- LETTERS[cls$id]
levels(fullRas) <- cls
is.factor(fullRas)

clsDT <- as.data.table(cls)
reducedDT <- reducedDT[clsDT, on = "mapcodeAll==id"]
reducedDT[, mapcodeAll := Bclass]

biomass2 <- rasterizeReduced(reducedDT, fullRas, "biomass")

# clean up
data.table::setDTthreads(origDTThreads)
options(Ncpus = origNcpus)
