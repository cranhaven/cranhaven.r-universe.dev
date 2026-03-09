#' Process ROI Voxel Data from a NIfTI File
#'
#' @description
#' Processes Regions of Interest (ROIs) from a binary NIfTI file by extracting voxel-level
#' coordinates and labeling each voxel as part of the ROI or not. The function preserves the
#' spatial structure and is typically used to prepare ground truth ROIs for comparison with
#' SCC-detected regions via \code{\link{calculateMetrics}}.
#'
#' @param roiFile \code{character}. Path to the binary NIfTI file containing ROI data.
#' @param region \code{character}. Name of the ROI region (e.g., \code{"Region2"}).
#' @param number \code{character}. Identifier for the subject or group (e.g., \code{"18"}).
#' @param save \code{logical}. If \code{TRUE}, saves the result as an \code{.RDS} file. If \code{FALSE},
#'        returns a data frame in the console. Default is \code{TRUE}.
#' @param outputDir \code{character}. Directory where the ROI table will be saved if \code{save = TRUE}.
#'        Default is a temporary file: \code{tempdir()}.
#' @param verbose \code{logical}. If \code{TRUE}, displays progress messages. Default is \code{TRUE}.
#'
#' @return A data frame with voxel-level ROI information.
#' \itemize{
#'   \item \code{group} – Combined identifier built from \code{region} and \code{number}.
#'   \item \code{z}, \code{x}, \code{y} – Voxel coordinates.
#'   \item \code{pet} – Binary value indicating ROI membership (\code{1} = ROI, \code{0} = non-ROI).
#' }
#' If \code{save = TRUE}, the data frame is saved as an \code{.RDS} file and not returned to the console.
#'
#' @details
#' The function uses \code{\link{neuroCleaner}} to load and flatten the NIfTI file into a structured
#' data frame. All voxels are retained, with the \code{pet} column indicating which ones are part
#' of the ROI (\code{1}) versus background (\code{0}). An ROI label is added in the \code{group} column.
#'
#' This output is used as ground truth for evaluating detection performance in SCC analyses.
#'
#' @examples
#' # Load and process a sample ROI NIfTI file (console output)
# roiFile <- system.file("extdata", "ROIsample_Region2_18.nii.gz", package = "neuroSCC")
# processedROI <- processROIs(roiFile, region = "Region2", number = "18", save = FALSE)
# head(processedROI)
#'
#' @seealso
#' \code{\link{calculateMetrics}} for evaluating SCC detection performance. \cr
#' \code{\link{neuroCleaner}} for reading and structuring voxel data.
#'
#' @export
processROIs <- function(roiFile,
                        region,
                        number,
                        save = TRUE,
                        outputDir = tempdir(),
                        verbose = TRUE) {

  # 1. Validate Inputs
  # ---------------------------
  if (!file.exists(roiFile)) stop("ROI file not found: ", roiFile)
  if (!is.character(region) || nchar(region) == 0) stop("region must be a non-empty string.")
  if (!is.character(number) || nchar(number) == 0) stop("number must be a non-empty string.")
  if (!is.logical(save) || length(save) != 1) stop("save must be TRUE or FALSE.")
  if (!is.logical(verbose) || length(verbose) != 1) stop("verbose must be TRUE or FALSE.")

  # 2. Load NIfTI File Using neuroCleaner
  # ---------------------------
  if (verbose) message("Loading NIfTI file...")
  voxelData <- neuroCleaner(roiFile)

  # 3. Assign ROI Group Identifier
  # ---------------------------
  voxelData$group <- paste0(region, "_number", number)

  # 4. Reorder Columns to Ensure group is First
  # ---------------------------
  columnOrder <- c("group", "z", "x", "y", "pet")
  voxelData <- voxelData[, columnOrder]

  # 5. Save or Print Results
  # ---------------------------
  if (save) {
    if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)
    outputFile <- file.path(outputDir, paste0("ROItable_", region, "_", number, ".RDS"))
    saveRDS(voxelData, outputFile)
    if (verbose) message("ROI table saved to: ", outputFile)
  } else {
    return(voxelData)
  }
}
