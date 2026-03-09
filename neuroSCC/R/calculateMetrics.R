#' Evaluate SCC or SPM Detection Performance
#'
#' @description
#' Computes Sensitivity, Specificity, Positive Predictive Value (PPV), and Negative Predictive Value (NPV)
#' by comparing detected points with ground truth ROI points. This function is used to assess
#' the accuracy of SCC- or SPM-based detection in neuroimaging analysis.
#'
#' @param detectedPoints A data frame containing detected coordinates (\code{x}, \code{y}).
#'        SCC-detected points should be obtained using \code{\link{getPoints}}.
#'        SPM-detected points should be obtained using \code{\link{getSPMbinary}}.
#' @param truePoints A data frame with ground truth ROI coordinates (\code{x}, \code{y}),
#'        extracted via \code{\link{processROIs}}.
#' @param totalCoords A list with the full voxel grid dimensions, created by \code{\link{getDimensions}}.
#'        Must include named elements \code{xDim} and \code{yDim}.
#' @param regionName A character string used to label the output region.
#'
#' @return A data frame with the following evaluation metrics
#' \itemize{
#'   \item \code{region}: Name of the analyzed region.
#'   \item \code{sensitivity}: True positive rate (TP / (TP + FN) * 100).
#'   \item \code{specificity}: True negative rate (TN / (TN + FP) * 100).
#'   \item \code{PPV}: Positive predictive value (TP / (TP + FP) * 100).
#'   \item \code{NPV}: Negative predictive value (TN / (TN + FN) * 100).
#' }
#'
#' @details
#' This function requires three precomputed objects
#' \itemize{
#'   \item \code{detectedPoints}: SCC-detected points from \code{\link{getPoints}} or SPM-detected points from \code{\link{getSPMbinary}}.
#'   \item \code{truePoints}: Ground truth ROIs extracted using \code{\link{processROIs}}.
#'   \item \code{totalCoords}: Full voxel coordinate grid from \code{\link{getDimensions}}.
#' }
#'
#' @examples
#' # Load precomputed inputs for the example
#' data("calculateMetricsExample", package = "neuroSCC")
#'
#' # Evaluate SCC and SPM detection performance
#' with(calculateMetricsExample, {
#'   metricsSCC <- calculateMetrics(detectedSCC, trueROI, totalCoords, "Region2_SCC")
#'   metricsSPM <- calculateMetrics(detectedSPM, trueROI, totalCoords, "Region2_SPM")
#'
#'   print(metricsSCC)
#'   print(metricsSPM)
#' })
#'
#' @seealso
#' \code{\link{getPoints}} for SCC-detected regions. \cr
#' \code{\link{getSPMbinary}} for binary SPM-detected points. \cr
#' \code{\link{processROIs}} for defining ground truth ROIs. \cr
#' \code{\link{getDimensions}} for generating the coordinate grid.
#'
#' @export
#' @importFrom stats rpois
calculateMetrics <- function(detectedPoints, truePoints, totalCoords, regionName) {

  # 1. Validate Inputs
  # ---------------------------
  if (!is.character(regionName) || length(regionName) != 1) {
    stop("'regionName' must be a single character string.")
  }

  if (!all(c("x", "y") %in% colnames(detectedPoints))) {
    stop("'detectedPoints' must be a data frame with 'x' and 'y' columns.")
  }

  if (!all(c("x", "y", "pet") %in% colnames(truePoints))) {
    stop("'truePoints' must be a data frame containing 'x', 'y', and 'pet' columns.")
  }

  if (!all(c("xDim", "yDim") %in% names(totalCoords))) {
    stop("'totalCoords' must be a list containing 'xDim' and 'yDim'.")
  }

  # 2. Process Inputs
  # ---------------------------
  # Extract only voxels marked as ROI (pet = 1)
  truePoints <- subset(truePoints, pet == 1, select = c("x", "y"))

  # Generate total coordinate grid from getDimensions() output
  totalCoords <- expand.grid(x = 1:totalCoords$xDim, y = 1:totalCoords$yDim)

  # Merge x and y into a single identifier
  detectedPoints <- tidyr::unite(detectedPoints, "x_y", x, y, sep = "_", remove = FALSE)
  truePoints <- tidyr::unite(truePoints, "x_y", x, y, sep = "_", remove = FALSE)
  totalCoords <- tidyr::unite(totalCoords, "x_y", x, y, sep = "_", remove = FALSE)

  # 3. Compute True Positives, False Positives, False Negatives, and True Negatives
  # ---------------------------
  TP <- nrow(dplyr::inner_join(detectedPoints, truePoints, by = "x_y"))
  FP <- nrow(dplyr::setdiff(detectedPoints, truePoints))
  FN <- nrow(dplyr::setdiff(truePoints, detectedPoints))

  trueNegatives <- dplyr::setdiff(totalCoords, truePoints)
  detectedNegatives <- dplyr::setdiff(totalCoords, detectedPoints)
  TN <- nrow(dplyr::inner_join(trueNegatives, detectedNegatives, by = "x_y"))

  # 4. Compute Sensitivity, Specificity, PPV, and NPV
  # ---------------------------
  sensitivity <- if ((TP + FN) > 0) (TP / (TP + FN)) * 100 else NA
  specificity <- if ((TN + FP) > 0) (TN / (TN + FP)) * 100 else NA
  PPV <- if ((TP + FP) > 0) (TP / (TP + FP)) * 100 else NA
  NPV <- if ((TN + FN) > 0) (TN / (TN + FN)) * 100 else NA

  # 5. Return Results
  # ---------------------------
  result <- data.frame(
    region = regionName,
    sensitivity = sensitivity,
    specificity = specificity,
    PPV = PPV,
    NPV = NPV
  )

  return(result)
}
