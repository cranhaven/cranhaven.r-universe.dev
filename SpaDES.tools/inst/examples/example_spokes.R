library(data.table)
library(terra)

origDTThreads <- data.table::setDTthreads(2L)
origNcpus <- options(Ncpus = 2L)
set.seed(1234)

ras <- terra::rast(terra::ext(0, 10, 0, 10), res = 1, val = 0)
rp <- randomPolygons(ras, numTypes = 10)

if (interactive())
  terra::plot(rp)

angles <- seq(0, pi * 2, length.out = 17)
angles <- angles[-length(angles)]
n <- 2
loci <- sample(terra::ncell(rp), n)
coords <- terra::vect(terra::xyFromCell(rp, loci))
stopRule <- function(landscape) landscape < 3
d2 <- spokes(rp, coords = coords, stopRule = stopRule,
             minRadius = 0, maxRadius = 50,
             returnAngles = TRUE, returnDistances = TRUE,
             allowOverlap = TRUE, angles = angles, returnIndices = TRUE)

# Assign values to the "patches" that were in the viewshed of a ray
rasB <- terra::rast(ras)
rasB[] <- 0
rasB[d2[d2[, "stop"] == 1, "indices"]] <- 1

if (interactive()) {
  rasB[rasB == 0] <- NA
  terra::plot(rasB, add = TRUE, col = "red", legend = FALSE)
}

if (NROW(d2) > 0) {
  sp1 <- terra::vect(d2[, c("x", "y")])
  if (interactive())
    terra::plot(sp1, add = TRUE, pch = 19)
}
if (interactive())
  terra::plot(coords, add = TRUE, pch = 19, col = "blue")

# clean up
data.table::setDTthreads(origDTThreads)
options(Ncpus = origNcpus)
