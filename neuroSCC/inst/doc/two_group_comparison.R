## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
library(neuroSCC)

## -----------------------------------------------------------------------------

# Recreate necessary data from sample files
cat("Creating sample data for 2-group comparison...\n")

# Create databases for Control and Pathological groups
databaseControls <- databaseCreator(pattern = "^syntheticControl[12]\\.nii\\.gz$", control = TRUE, quiet = TRUE)
databasePathological <- databaseCreator(pattern = "^syntheticPathological[12]\\.nii\\.gz$", control = FALSE, quiet = TRUE)

# Create matrices suitable for SCC
matrixControls <- matrixCreator(databaseControls, paramZ = 35, quiet = TRUE)
matrixPathological <- matrixCreator(databasePathological, paramZ = 35, quiet = TRUE)

# Normalize matrices
normalizedControls <- meanNormalization(matrixControls)
normalizedPathological <- meanNormalization(matrixPathological)

# Extract contours from a control subject for SCC triangulation
niftiPath <- system.file("extdata", "syntheticControl1.nii.gz", package = "neuroSCC")
contours <- neuroContour(niftiPath, paramZ = 35, levels = c(0), plotResult = FALSE)

cat("Data preparation completed successfully.\n")


## -----------------------------------------------------------------------------

if (requireNamespace("ImageSCC", quietly = TRUE)) {
  message("'ImageSCC' package is available.")
} else {
  message("This vignette requires the 'ImageSCC' package.")
  message("You can install it from GitHub with:")
  message("  remotes::install_github('FIRST-Data-Lab/ImageSCC')")
}


## ----eval = requireNamespace("ImageSCC", quietly = TRUE), echo=TRUE-----------

# Check for Triangulation package
triangulation_available <- requireNamespace("Triangulation", quietly = TRUE)

if (triangulation_available) {
  message("'Triangulation' package is available.")
} else {
  message("'Triangulation' package not available.")
  message("Install with: remotes::install_github('FIRST-Data-Lab/Triangulation')")
}

# Proceed only if both packages are available
if (triangulation_available) {

  # Try loading precomputed SCC object from data/
  if (!exists("SCCcomp", inherits = FALSE) &&
      "SCCcomp" %in% data(package = "neuroSCC")$results[, "Item"]) {
    
    message("Loading precomputed SCC group comparison from package data...")
    suppressMessages(data("SCCcomp", package = "neuroSCC"))

  } else if (!exists("SCCcomp", inherits = FALSE)) {

    message("Precomputed data not found. Running SCC estimation...")

    # Perform SCC computation (only for development)
    SCCcomp <- ImageSCC::scc.image(
      Ya = normalizedPathological,
      Yb = normalizedControls,
      Z = contours[[1]],
      d.est = 5,
      d.band = 2,
      r = 1,
      V.est.a = as.matrix(contours[[1]]),
      Tr.est.a = as.matrix(contours[[1]]),
      V.band.a = as.matrix(contours[[1]]),
      Tr.band.a = as.matrix(contours[[1]]),
      penalty = TRUE,
      lambda = 10^{seq(-6, 3, 0.5)},
      alpha.grid = c(0.10, 0.05, 0.01),
      adjust.sigma = TRUE
    )
  }
}



## ----eval=TRUE----------------------------------------------------------------
# Extract significant points from SCC results
significantPoints <- getPoints(SCCcomp)

# Load true ROI data provided within the package
roi_path <- system.file("extdata", "ROIsample_Region2_18.nii.gz", package = "neuroSCC")
trueROI <- processROIs(roi_path, region = "Region2", number = "18", save = FALSE)

# Get total coordinate dimensions
dimensions <- getDimensions(roi_path)
totalCoords <- expand.grid(x = 1:dimensions$xDim, y = 1:dimensions$yDim)

# Calculate metrics to evaluate detection performance
metrics <- calculateMetrics(
  detectedPoints = significantPoints$positivePoints,
  truePoints = trueROI,
  totalCoords = dimensions,
  regionName = "Group_vs_Group"
)

# Display calculated metrics
print(metrics)

