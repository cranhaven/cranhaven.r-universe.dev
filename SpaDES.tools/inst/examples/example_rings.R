library(terra)

origDTThreads <- data.table::setDTthreads(2L)
origNcpus <- options(Ncpus = 2L)
set.seed(1462)

# Make random forest cover map
emptyRas <- terra::rast(terra::ext(0, 1e2, 0, 1e2), res = 1)

# start from two cells near middle
loci <- (ncell(emptyRas) / 2 - ncol(emptyRas)) / 2 + c(-3, 3)

# No overlap is default, occurs randomly
emptyRas[] <- 0
rngs <- rings(emptyRas, loci = loci, minRadius = 7, maxRadius = 9, returnIndices = TRUE)
emptyRas[rngs$indices] <- rngs$id
if (interactive()) {
  terra::plot(emptyRas)
}

# Variable ring widths, including centre cell for smaller one
emptyRas[] <- 0
rngs <- rings(emptyRas, loci = loci, minRadius = c(0, 7), maxRadius = c(8, 18),
              returnIndices = TRUE)
emptyRas[rngs$indices] <- rngs$id
if (interactive()) {
  terra::plot(emptyRas)
}

# clean up
data.table::setDTthreads(origDTThreads)
options(Ncpus = origNcpus)
