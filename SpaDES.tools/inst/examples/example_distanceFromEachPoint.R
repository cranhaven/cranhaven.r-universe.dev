library(terra)

origDTThreads <- data.table::setDTthreads(2L)
origNcpus <- options(Ncpus = 2L)

n <- 2
distRas <- rast(ext(0, 40, 0, 40), res = 1)
coords <- cbind(x = round(runif(n, xmin(distRas), xmax(distRas))) + 0.5,
                y = round(runif(n, xmin(distRas), xmax(distRas))) + 0.5)

# inverse distance weights
dists1 <- distanceFromEachPoint(coords, landscape = distRas)
indices <- cellFromXY(distRas, dists1[, c("x", "y")])
invDist <- tapply(dists1[, "dists"], indices, function(x) sum(1 / (1 + x))) # idw function
distRas[] <- as.vector(invDist)
if (interactive()) {
  # clearPlot()
  terra::plot(distRas)
}

# With iterative summing via cumulativeFn to keep memory use low, with same result
dists1 <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                landscape = distRas, cumulativeFn = `+`)
idwRaster <- rast(distRas)
idwRaster[] <- dists1[, "dists"]
if (interactive()) terra::plot(idwRaster)

all(idwRaster[] == distRas[]) # TRUE

# A more complex example of cumulative inverse distance sums, weighted by the value
#  of the origin cell
ras <- rast(ext(0, 34, 0, 34), res = 1, val = 0)
rp <- randomPolygons(ras, numTypes = 10) ^ 2
n <- 15
cells <- sample(ncell(ras), n)
coords <- xyFromCell(ras, cells)
distFn <- function(landscape, fromCell, dist) landscape[fromCell] / (1 + dist)

#beginCluster(3) # can do parallel
dists1 <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                landscape = rp, distFn = distFn, cumulativeFn = `+`)
#endCluster() # if beginCluster was run

idwRaster <- rast(ras)
idwRaster[] <- dists1[, "dists"]
if (interactive()) {
  # clearPlot()
  terra::plot(rp)
  sp1 <- vect(coords)
  terra::plot(sp1, add = TRUE)
  terra::plot(idwRaster)
  terra::plot(sp1, add = TRUE)
}

# On linux; can use numeric passed to cl; will use mclapply with mc.cores = cl
if (identical(Sys.info()["sysname"], "Linux")) {
  dists1 <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                  landscape = rp, distFn = distFn,
                                  cumulativeFn = `+`, cl = 2)
}

# clean up
data.table::setDTthreads(origDTThreads)
options(Ncpus = origNcpus)
