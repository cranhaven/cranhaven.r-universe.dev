#' Regional thresholding of greyscale images
#'
#' Perform thresholding of greyscale images by applying a method
#' regionally, using a segmentation map.
#'
#' @details
#' This function supports several thresholding methods applied within the
#' regions defined by `segmentation`:
#'
#' \describe{
#'   \item{Methods from the `autothresholdr` package:}{Any method supported by
#'   [autothresholdr::auto_thresh()] can be used by specifying its name.
#'   For example, `"IsoData"` applies the classic iterative intermeans algorithm
#'   \insertCite{isodata;textual}{rcaiman}, which is among the most recommended
#'   for canopy photography \insertCite{Jonckheere2005}{rcaiman}.}
#'    \item{In-package implementation of IsoData:}{Use `"thr_isodata"` to apply
#'   [thr_isodata()], a native implementation of the same algorithm}
#'   \item{Two-corner method:}{Use `"thr_twocorner"` to apply
#'   [thr_twocorner()], which implements a geometric thresholding strategy based
#'   on identifying inflection points in the histogram, first introduced to
#'   canopy photography by \insertCite{Macfarlane2011;textual}{rcaiman}. Since
#'   this method tend to fail, the fallback is `thr_isodata`}
#' }
#'
#' @param r numeric [terra::SpatRaster-class] of one layer. Typically the blue
#'   channel of a canopy photograph.
#' @param segmentation numeric [terra::SpatRaster-class] of one layer. A labeled
#'   segmentation map defining the regions over which to apply the thresholding
#'   method. Ring segmentation (see [ring_segmentation()]) is often preferred
#'   for fisheye images \insertCite{Leblanc2005}{rcaiman}.
#' @param method character vector of length one. Name of the thresholding method
#'   to apply. See *Details*.
#'
#' @note
#' When methods from the `autothresholdr` package are used, `r` values
#' should be constrained to the range \eqn{[0, 1]}. See [normalize_minmax()].
#'
#' @return Logical [terra::SpatRaster-class] (`TRUE` for sky, `FALSE` for
#'   non-sky) of the same dimensions as `r`.
#'
#' @export
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' path <- system.file("external/DSCN4500.JPG", package = "rcaiman")
#' zenith_colrow <- c(1276, 980)
#' diameter <- 756*2
#' caim <- read_caim(path, zenith_colrow - diameter/2, diameter, diameter)
#' z <- zenith_image(ncol(caim), lens("Nikon_FCE9"))
#' r <- invert_gamma_correction(caim$Blue)
#' r <- correct_vignetting(r, z, c(0.0638, -0.101)) %>% normalize_minmax()
#' rings <- ring_segmentation(z, 15)
#' bin <- binarize_by_region(r, rings, "thr_isodata")
#' plot(bin)
#' }
binarize_by_region <- function(r, segmentation, method) {
  .assert_single_layer(r)
  .assert_single_layer(segmentation)
  .assert_same_geom(r, segmentation)
  .check_vector(method, "character", 1)

  .fun <- switch(method,
    thr_isodata = thr_isodata,
    thr_twocorner = function(x) tryCatch(thr_twocorner(x)$tm,
                                         error = function(e) thr_isodata(x))
  )

  if (is.null(.fun)) {
    .was_normalized(r, "r")
    if (!requireNamespace("autothresholdr", quietly = TRUE)) {
      stop(paste(
        "Package \"autothresholdr\" needed for this function to work.",
        "Please install it."
      ),
      call. = FALSE
      )
    }

    .fun <- function(dns) {
      autothresholdr::auto_thresh(round(dns * 255),
                                  method,
                                  ignore_black = TRUE,
                                  ignore_white = TRUE,
                                  ignore_na = TRUE)[1] / 255
    }
  }
  bin <- binarize_with_thr(r, .get_min(r))
  .binarize_per_ring <- function(segment_id) {
    indices <- segmentation == segment_id
    thr <- .fun(r[indices])
    bin[indices] <<- r[indices] > thr
  }
  segs <- unique(terra::values(segmentation)) %>% as.numeric()
  segs <- segs[!is.na(segs)]
  segs <- segs[segs != 0]
  Map(.binarize_per_ring, segs)
  bin
}
