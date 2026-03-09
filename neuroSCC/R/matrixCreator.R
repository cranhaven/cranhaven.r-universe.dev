#' Convert Database to Functional Data Matrix Format
#'
#' @description
#' Converts a PET image database (created via \code{\link{databaseCreator}}) into
#' a matrix format suitable for functional data analysis.
#' Each row of the resulting matrix corresponds to a subject, and each column to a voxel's PET intensity
#' values at a specified brain slice.
#'
#' @param database A data frame created by \code{\link{databaseCreator}}, containing
#'        voxel-level PET image data, including subject identifiers, coordinates, and intensity values.
#' @param paramZ An integer specifying the z-coordinate (slice) to extract. Default is \code{35}.
#' @param useSequentialNumbering \code{logical}. If \code{TRUE}, assigns sequential subject IDs
#'        instead of extracting them from filenames. Not currently used inside this function. Default is \code{FALSE}.
#' @param quiet \code{logical}. If \code{TRUE}, suppresses progress messages. Default is \code{FALSE}.
#'
#' @return A numeric matrix where
#' \itemize{
#'   \item Each row represents one subject's PET values at the selected z-slice.
#'   \item Each column corresponds to a voxel (flattened as a 1D row).
#' }
#'
#' @details
#' This function performs the following steps
#' \enumerate{
#'   \item Verifies that the specified z-slice exists in the database.
#'   \item Identifies the correct subject grouping column (\code{CN_number} or \code{AD_number}).
#'   \item Determines the matrix dimensions using \code{x} and \code{y} coordinates.
#'   \item Extracts PET intensities per subject at the given slice.
#'   \item Replaces any \code{NaN} values with \code{0} to ensure numerical stability.
#' }
#'
#' This function typically follows \code{\link{databaseCreator}} and precedes
#' \code{\link{meanNormalization}} in the neuroSCC workflow.
#'
#' @examples
#' # NOTE: To keep example runtime short, only one synthetic PET file is used.
#' # For full analysis, expand the filename pattern accordingly.
#'
#' # Step 1: Generate a database for a single subject
#' controlPattern <- "^syntheticControl1\\.nii\\.gz$"
#' databaseControls <- databaseCreator(pattern = controlPattern, control = TRUE, quiet = TRUE)
#'
#' # Step 2: Convert the database into a matrix format
#' matrixControls <- matrixCreator(databaseControls, paramZ = 35, quiet = TRUE)
#'
#' # Display dimensions of the matrix
#' dim(matrixControls)
#'
#' @seealso
#' \code{\link{databaseCreator}} for generating the input database. \cr
#' \code{\link{meanNormalization}} for scaling matrix data prior to SCC computation.
#'
#' @export
matrixCreator <- function(database, paramZ = 35, useSequentialNumbering = FALSE, quiet = FALSE) {
  # 1. Input validation
  if (!is.data.frame(database)) {
    stop("'database' must be a data frame created by databaseCreator")
  }

  # Validate z-slice exists
  zValues <- unique(database$z)
  if (!(paramZ %in% zValues)) {
    stop(sprintf("Specified z-slice %d not found. Available z-slices: %s",
                 paramZ, paste(zValues, collapse = ", ")))
  }

  # 2. Determine group identification
  if ("CN_number" %in% colnames(database)) {
    groupCol <- "CN_number"
  } else if ("AD_number" %in% colnames(database)) {
    groupCol <- "AD_number"
  } else {
    stop("Database must contain either 'CN_number' or 'AD_number' column")
  }

  # 3. Calculate matrix dimensions
  zSliceData <- subset(database, z == paramZ)
  xMax <- max(zSliceData$x)
  yMax <- max(zSliceData$y)
  matrixDim <- xMax * yMax

  # 4. Get unique subject numbers
  subjectNumbers <- unique(database[[groupCol]])

  # 5. Preallocate the matrix
  matrixResult <- matrix(nrow = length(subjectNumbers), ncol = matrixDim)

  # 6. Process each subject
  for (i in seq_along(subjectNumbers)) {
    subjectID <- subjectNumbers[i]

    # Print progress if not quiet
    if (!quiet) {
      message(sprintf("Processing Subject %s", subjectID))
    }

    # Extract PET values for the subject at the specified z-slice
    subsetData <- database[database[[groupCol]] == subjectID & database$z == paramZ, ]
    Y <- ifelse(is.nan(subsetData$pet), 0, subsetData$pet)

    # Assign to matrix row
    matrixResult[i, ] <- as.numeric(Y)
  }

  return(matrixResult)
}
