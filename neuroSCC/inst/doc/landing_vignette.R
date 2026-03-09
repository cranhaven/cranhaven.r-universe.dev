## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(neuroSCC)

## -----------------------------------------------------------------------------
niftiFile <- system.file("extdata", "syntheticControl1.nii.gz", package = "neuroSCC")

# Load and clean data using neuroCleaner
petData <- neuroCleaner(niftiFile)

# Inspect the data
head(petData)
str(petData)

## -----------------------------------------------------------------------------
# Create database for control subjects
controlPattern <- "^syntheticControl.*\\.nii.gz$"
databaseControls <- databaseCreator(pattern = controlPattern, control = TRUE, quiet = FALSE)

# Inspect the created database
head(databaseControls)
table(databaseControls$CN_number)

## -----------------------------------------------------------------------------
# Create matrix for Z-slice 35
matrixControls <- matrixCreator(databaseControls, paramZ = 35, quiet = FALSE)

# Inspect matrix structure
dim(matrixControls)
str(matrixControls)

## -----------------------------------------------------------------------------
# Perform mean normalization
normalizedMatrix <- meanNormalization(matrixControls, returnDetails = FALSE)


## ----fig.alt="Contours for brain imaging data"--------------------------------
# Extract contours from sample data
contours <- neuroContour(niftiFile, paramZ = 35, levels = 0, plotResult = TRUE)

# Check contours structure
length(contours)
str(contours[[1]])

## ----fig.alt="Delaunay triangulations for brain imaging data", eval = requireNamespace("ImageSCC", quietly = TRUE)----

if (!requireNamespace("Triangulation", quietly = TRUE)) {
  cat("Triangulation package is not installed.\nInstall it using: remotes::install_github('FIRST-Data-Lab/Triangulation')\n")
} else {
  # Perform triangulation with the first contour
  mesh <- Triangulation::TriMesh(n = 15, contours[[1]])
  
  # Inspect mesh
  print(mesh[["V"]][1:10, ])
  print(mesh[["Tr"]][1:10, ])
}


