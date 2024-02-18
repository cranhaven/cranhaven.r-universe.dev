#' @title Imaging registration using \code{'NiftyReg'}
#' @description Registers 'CT' to 'MRI', or 'MRI' to another 'MRI'
#' @param source source imaging data, or a \code{'nifti'} file path;
#' for example, 'CT'
#' @param target target imaging data to align to; for example, 'MRI'
#' @param method method of transformation, choices are \code{'rigid'},
#' \code{'affine'}, or \code{'nonlinear'}
#' @param interpolation how volumes should be interpolated, choices are
#' \code{'cubic'}, \code{'trilinear'}, or \code{'nearest'}
#' @param threads,symmetric,verbose,... see \code{\link[RNiftyReg]{niftyreg}}
#' @returns See \code{\link[RNiftyReg]{niftyreg}}
#' @examples
#'
#' if(interactive()) {
#'
#' source <- system.file("extdata", "epi_t2.nii.gz", package="RNiftyReg")
#' target <- system.file("extdata", "flash_t1.nii.gz", package="RNiftyReg")
#' aligned <- register_volume(source, target, verbose = FALSE)
#'
#' source_img <- aligned$source[[1]]
#' target_img <- aligned$target
#' aligned_img <- aligned$image
#'
#' par(mfrow = c(2, 2), mar = c(0.1, 0.1, 3.1, 0.1))
#'
#' pal <- grDevices::grey.colors(256, alpha = 1)
#' image(source_img[,,30], asp = 1, axes = FALSE,
#'       col = pal, main = "Source image")
#' image(target_img[,,64], asp = 1, axes = FALSE,
#'       col = pal, main = "Target image")
#' image(aligned_img[,,64], asp = 1, axes = FALSE,
#'       col = pal, main = "Aligned image")
#'
#' # bucket fill and calculate differences
#' aligned_img[is.nan(aligned_img) | aligned_img <= 1] <- 1
#' target_img[is.nan(target_img) | aligned_img <= 1] <- 1
#' diff <- abs(aligned_img / target_img - 1)
#' image(diff[,,64], asp = 1, axes = FALSE,
#'       col = pal, main = "Percentage Difference")
#'
#' }
#'
#' @export
register_volume <- function(
    source, target, method = c("rigid", "affine", "nonlinear"),
    interpolation = c("cubic", "trilinear", "nearest"),
    threads = detect_threads(), symmetric = TRUE, verbose = TRUE, ...) {
  method <- match.arg(method)
  interpolation <- match.arg(interpolation)
  interpolation <- list(
    cubic = 3L,
    trilinear = 1L,
    nearest = 0L
  )[[interpolation]]

  if(isTRUE(is.character(source))) {
    source <- RNiftyReg::readNifti(source)
  }
  if(isTRUE(is.character(target))) {
    target <- RNiftyReg::readNifti(target)
  }

  RNiftyReg::niftyreg(
    source = source,
    target = target,
    scope = method,
    interpolation = interpolation,
    threads = threads,
    internal = FALSE,
    symmetric = symmetric,
    estimateOnly = FALSE,
    verbose = verbose,
    ...
  )
}
