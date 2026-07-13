library(data.table)
library(terra)

origDTThreads <- data.table::setDTthreads(2L)
origNcpus <- options(Ncpus = 2L)
set.seed(1462)

# circle centred
ras <- rast(ext(0, 15, 0, 15), res = 1, val = 0)
middleCircle <- cir(ras)
ras[middleCircle[, "indices"]] <- 1
circlePoints <- vect(middleCircle[, c("x", "y")])
if (interactive()) {
  # clearPlot()
  terra::plot(ras)
  terra::plot(circlePoints, add = TRUE)
}

# circles non centred
ras <- randomPolygons(ras, numTypes = 4)
n <- 2
agent <- vect(cbind(x = stats::runif(n, xmin(ras), xmax(ras)),
                    y = stats::runif(n, xmin(ras), xmax(ras))))

cirs <- cir(ras, agent, maxRadius = 15, simplify = TRUE) ## TODO: empty with some seeds! e.g. 1642
cirsSP <- vect(cirs[, c("x", "y")]) ## TODO: error with some seeds! e.g. 1642
cirsRas <- rast(ras)
cirsRas[] <- 0
cirsRas[cirs[, "indices"]] <- 1

if (interactive()) {
  terra::plot(ras)
  terra::plot(cirsRas, add = TRUE, col = c("transparent", "#00000055"))
  terra::plot(agent, add = TRUE)
  terra::plot(cirsSP, add = TRUE)
}

# Example comparing rings and cir
hab <- rast(system.file("extdata", "hab1.tif", package = "SpaDES.tools"))
radius <- 4
n <- 2
coords <- vect(cbind(x = stats::runif(n, xmin(hab), xmax(hab)),
                     y = stats::runif(n, xmin(hab), xmax(hab))))

# cirs
cirs <- cir(hab, coords, maxRadius = rep(radius, length(coords)), simplify = TRUE)

ras1 <- rast(hab)
ras1[] <- 0
ras1[cirs[, "indices"]] <- cirs[, "id"]
if (interactive()) {
  terra::plot(ras1)
}

# rings
loci <- cellFromXY(hab, crds(coords))
cirs2 <- rings(hab, loci, maxRadius = radius, minRadius = radius - 1, returnIndices = TRUE)

ras2 <- rast(hab)
ras2[] <- 0
ras2[cirs2$indices] <- cirs2$id
if (interactive()) {
  terra::plot(c(ras1, ras2))
}

hab <- rast(system.file("extdata", "hab2.tif", package = "SpaDES.tools"))
cirs <- cir(hab, coords, maxRadius = 44, minRadius = 0)
ras1 <- rast(hab)
ras1[] <- 0
cirsOverlap <- data.table::data.table(cirs)[, list(sumIDs = sum(id)), by = indices]
ras1[cirsOverlap$indices] <- cirsOverlap$sumIDs
if (interactive()) {
  terra::plot(ras1)
}

# Provide a specific set of angles
ras <- rast(ext(0, 330, 0, 330), res = 1)
ras[] <- 0
n <- 2
coords <- cbind(x = stats::runif(n, xmin(ras), xmax(ras)),
                y = stats::runif(n, xmin(ras), xmax(ras)))
circ <- cir(ras, coords, angles = seq(0, 2 * pi, length.out = 21),
            maxRadius = 200, minRadius = 0, returnIndices = FALSE,
            allowOverlap = TRUE, returnAngles = TRUE)

# clean up
data.table::setDTthreads(origDTThreads)
options(Ncpus = origNcpus)
