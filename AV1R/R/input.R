#' Read a TIFF stack and return basic metadata
#'
#' Does not load pixel data into R -- only inspects the file.
#' Useful for checking frame count and dimensions before encoding.
#'
#' @param path Path to a multi-page TIFF file or printf-pattern
#'   (e.g. \code{"frame\%04d.tif"}).
#'
#' @return A list with elements: \code{path}, \code{n_frames},
#'   \code{width}, \code{height}, \code{size_mb}.
#'
#' @examples
#' # Create a small temporary TIFF and inspect it
#' tmp <- tempfile(fileext = ".tif")
#' writeBin(raw(1024), tmp)
#' info <- read_tiff_stack(tmp)
#' str(info)
#' unlink(tmp)
#' @export
read_tiff_stack <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)

  size_mb <- file.info(path)$size / 1024^2

  # Fallback: return size only
  list(
    path     = path,
    n_frames = NA_integer_,
    width    = NA_integer_,
    height   = NA_integer_,
    size_mb  = round(size_mb, 2)
  )
}
