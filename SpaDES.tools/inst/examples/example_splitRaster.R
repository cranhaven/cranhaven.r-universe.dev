library(terra)

origDTThreads <- data.table::setDTthreads(2L)
origNcpus <- options(Ncpus = 2L)
set.seed(1462)

## an example with dimensions: nrow = 77,  ncol = 101, nlayers = 3
b <- rast(system.file("ex/logo.tif", package = "terra"))
r <- b[[1]] # use first layer only
nx <- 3
ny <- 4

tmpdir <- dir.create(file.path(tempdir(), "splitRaster-example"), showWarnings = FALSE)

y0 <- splitRaster(r, nx, ny, path = file.path(tmpdir, "y0")) # no buffer

## buffer: 10 pixels along both axes
y1 <- splitRaster(r, nx, ny, c(10, 10), path = file.path(tmpdir, "y1"))

## buffer: half the width and length of each tile
y2 <- splitRaster(r, nx, ny, c(0.5, 0.5), path = file.path(tmpdir, "y2"))

## the original raster:
if (interactive()) plot(r) # may require a call to `dev()` if using RStudio

## the split raster:
layout(mat = matrix(seq_len(nx * ny), ncol = nx, nrow = ny))
plotOrder <- unlist(lapply(split(1:12, rep(1:nx, each = ny)), rev))

if (interactive()) {
  invisible(lapply(y0[plotOrder], terra::plot))
}

## parallel splitting
if (requireNamespace("raster", quietly = TRUE) &&
    requireNamespace("parallel")) {
  if (interactive()) {
    n <- pmin(parallel::detectCores(), 4) # use up to 4 cores
    raster::beginCluster(n, type = "PSOCK")
    y3 <- splitRaster(r, nx, ny, c(0.7, 0.7), path = file.path(tmpdir, "y3"))
    raster::endCluster()
    if (interactive()) {
      invisible(lapply(y3[plotOrder], terra::plot))
    }
  }
}

## can be recombined using `terra::merge`
m0 <- do.call(merge, y0)
all.equal(m0, r) ## TRUE

m1 <- do.call(merge, y1)
all.equal(m1, r) ## TRUE

m2 <- do.call(merge, y2)
all.equal(m2, r) ## TRUE

## or recombine using mergeRaster
n0 <- mergeRaster(y0)
all.equal(n0, r) ## TRUE

n1 <- mergeRaster(y1)
all.equal(n1, r) ## TRUE

n2 <- mergeRaster(y2)
all.equal(n2, r) ## TRUE

# clean up
data.table::setDTthreads(origDTThreads)
options(Ncpus = origNcpus)
unlink(tmpdir, recursive = TRUE)
