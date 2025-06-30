rm(list = ls(all.names = TRUE))

options(warn = -1)  # Temporarily suppress warnings
old_omp_thread_limit <- Sys.getenv("OMP_THREAD_LIMIT")
Sys.setenv(OMP_THREAD_LIMIT = "2")

library(psgp)
set.seed(100)
# set up data:
data(meuse)
coordinates(meuse) <- ~x+y
meuse$value <- log(meuse$zinc)
data(meuse.grid)
gridded(meuse.grid) <- ~x+y
proj4string(meuse) <- CRS("EPSG:28992")
proj4string(meuse.grid) <- CRS("EPSG:28992")

# set up intamap object:
psgpObject <- createIntamapObject(
  observations = meuse,
  formulaString = as.formula(value~1),
  predictionLocations = meuse.grid,
  class = "psgp"
)

# run test:
checkSetup(psgpObject)

# do interpolation steps:
psgpObject <- estimateParameters(psgpObject)

# make prediction
psgpObject <- spatialPredict(psgpObject)

# Plot prediction
# plotIntamap(psgpObject)
# plotIntamap(meuse, pch=1, cex=sqrt(meuse$value)/20, add=TRUE)

# Restore original settings at the end
Sys.setenv(OMP_THREAD_LIMIT = old_omp_thread_limit)
options(warn = 0)  # Restore warning level
