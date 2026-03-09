#' Precomputed Inputs for SCC vs. SPM Performance Evaluation
#'
#' @name calculateMetricsExample
#' @docType data
#'
#' @description
#' A dataset containing all necessary inputs for demonstrating \code{\link{calculateMetrics}}.
#' It enables reproducible and fast example code that compares SCC-detected and SPM-detected
#' points against a known ground truth ROI.
#'
#' These inputs were generated using sample PET and ROI files included in the \code{neuroSCC} package.
#'
#' @format A set of four objects:
#' \describe{
#'   \item{\code{detectedSCC}}{Data frame of SCC-detected coordinates (from \code{\link{getPoints}}).}
#'   \item{\code{detectedSPM}}{Data frame of SPM-detected coordinates (from \code{\link{getSPMbinary}}).}
#'   \item{\code{trueROI}}{Ground truth ROI voxel data (from \code{\link{processROIs}}).}
#'   \item{\code{totalCoords}}{List with full image grid dimensions (from \code{\link{getDimensions}}).}
#' }
#'
#' @usage data("calculateMetricsExample")
#'
#' @seealso \code{\link{calculateMetrics}}, \code{\link{getPoints}}, \code{\link{getSPMbinary}},
#'          \code{\link{processROIs}}, \code{\link{getDimensions}}
#'
#' @source Simulated PET neuroimaging study for testing SCC and SPM detection accuracy.
#'
#' @keywords datasets
NULL
