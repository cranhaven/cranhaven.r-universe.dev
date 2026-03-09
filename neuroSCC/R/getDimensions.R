#' Get Dimensions from a Neuroimaging File
#'
#' @description
#' Extracts voxel dimension information from a NIfTI or similar neuroimaging file.
#' This function is designed to work with \code{\link{neuroCleaner}}, but it can also be used
#' independently to inspect image dimensions.
#'
#' @param file A NIfTI file object or a file path pointing to a NIfTI image.
#'
#' @return A named list with the following elements
#' \itemize{
#'   \item \code{xDim} – Number of voxels along the X axis.
#'   \item \code{yDim} – Number of voxels along the Y axis.
#'   \item \code{zDim} – Number of slices along the Z axis.
#'   \item \code{dim} – Total number of voxels in a 2D slice (calculated as \code{xDim * yDim}).
#' }
#'
#' @details
#' The function accepts either a file path or a preloaded \code{nifti} object.
#' If a file path is provided, it uses \code{oro.nifti::readNIfTI()} to load the image.
#' This function ensures consistent dimension extraction across the \code{neuroSCC} pipeline.
#'
#' @examples
#' # Get the file path for a sample NIfTI file
#' niftiFile <- system.file("extdata", "syntheticControl1.nii.gz", package = "neuroSCC")
#'
#' # Extract dimensions from the NIfTI file
#' dimensions <- getDimensions(niftiFile)
#'
#' # Display the extracted dimensions
#' print(dimensions)
#'
#' @export
getDimensions <- function(file) {
  # Input validation
  if (missing(file)) {
    stop("A NIFTI file or file path must be provided")
  }

  # Handle different input types
  if (is.character(file)) {
    # If a file path is provided, read the NIFTI file
    tryCatch({
      file <- oro.nifti::readNIfTI(fname = file, verbose = FALSE, warn = -1)
    }, error = function(e) {
      stop("Unable to read the NIFTI file: ", e$message)
    })
  }

  # Validate that the input is a NIFTI object
  if (!inherits(file, "nifti")) {
    stop("Input must be a NIFTI file path or a nifti object")
  }

  # Extract dimensions
  xDim <- file@dim_[2]
  yDim <- file@dim_[3]
  zDim <- file@dim_[4]
  dim <- xDim * yDim

  # Return dimensions
  list(
    xDim = xDim,
    yDim = yDim,
    zDim = zDim,
    dim = dim
  )
}
