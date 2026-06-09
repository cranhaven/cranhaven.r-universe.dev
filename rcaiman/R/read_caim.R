#' Read a canopy image from a file
#'
#' Reads a born-digital image (typically RGB-JPEG or RGB-TIFF) using
#' [terra::rast()] and returns a [terra::SpatRaster-class] object. Optionally, it can
#' extract a rectangular region of interest (ROI) specified by the user.
#'
#' This function is intended for importing color hemispherical photographs, such
#' as those obtained with digital cameras equipped with fisheye lenses. For raw
#' image files (e.g., NEF, CR2), see [read_caim_raw()].
#'
#' Internally, this is a wrapper around [terra::rast()], so support for image
#' formats depends on the capabilities of the `terra` package.
#'
#' If no arguments are provided, a sample image will be returned.
#'
#' @section Selecting a Region of Interest:
#' To load a specific subregion from the image, use the arguments `upper_left`,
#' `width`, and `height`. These are expressed in raster coordinates, similar to
#' a spreadsheet layout: **columns first, then rows**. In other words, specify
#' coordinates as `c(column, row)`, **not** `c(row, column)`, which is typical
#' in `data.frame` objects.
#'
#' While any image editor can be used to obtain these values, this function was
#' tested with [ImageJ](https://imagej.net/ij/), particularly the Fiji
#' distribution. A recommended workflow:
#' 1. Open the image in Fiji.
#' 2. Draw a rectangular selection.
#' 3. Go to *Edit > Selection > Specify...* to read `upper_left`, `width`, and `height`.
#'
#' @param path character vector of length one.  Path to an image file, including
#'   extension. If `NULL`, an example image is returned.
#' @param upper_left numeric vector of length two. Pixel coordinates of the
#'   upper-left corner of the ROI, in the format `c(column, row)`.
#' @param width,height numeric vector of length one. Size (in pixels) of the
#'   rectangular ROI to read.
#'
#' @return Numeric [terra::SpatRaster-class], typically with layers named `"Red"`,
#'   `"Green"`, and `"Blue"`. If the file format or metadata prevents automatic
#'   layer naming, names will be inferred and a warning may be issued.
#'
#' @note
#' The example image was created from a raw photograph taken with a Nikon Coolpix
#' 5700 and a FC-E9 auxiliary lens, processed with the following code:
#'
#' ```
#' zenith_colrow <- c(1290, 988)/2
#' diameter <- 756
#' z <- zenith_image(diameter, lens("Nikon_FCE9"))
#' a <- azimuth_image(z)
#' m <- !is.na(z)
#' caim <- read_caim_raw("DSCN4606.NEF")
#' caim <- crop_caim(caim, zenith_colrow - diameter/2, diameter, diameter)
#' caim <- correct_vignetting(caim, z, c(0.0638, -0.101))
#' caim <- c(mean(caim$Y, caim$M), caim$G, caim$C)
#' caim <- fisheye_to_equidistant(caim, z, a, m, radius = 300, k = 1)
#' write_caim(caim, "example.tif", 16)
#' ```
#'
#' @export
#'
#' @seealso [write_caim()]
#'
#' @examples
#' path <- system.file("external/DSCN4500.JPG", package = "rcaiman")
#' zenith_colrow <- c(1276, 980)
#' diameter <- 756*2
#' caim <- read_caim(path, zenith_colrow - diameter/2, diameter, diameter)
#' plot(caim$Blue)
read_caim <- function(path = NULL, upper_left = NULL, width = NULL,
                      height = NULL) {

  .check_vector(path, "character", 1, allow_null = TRUE)
  if (is.null(path)) {
    path <- system.file("external/example.tif", package = "rcaiman")
  }
  .assert_file_exists(path)
  .check_vector(upper_left, "numeric", 2, allow_null = TRUE, sign = "positive")
  .check_vector(width, "numeric", 1, allow_null = TRUE, sign = "positive")
  .check_vector(height, "numeric", 1, allow_null = TRUE, sign = "positive")


  r <- tryCatch(terra::rast(path),
                warning = function(w) terra::flip(terra::rast(path)))

  terra::ext(r) <- terra::ext(0, ncol(r), 0, nrow(r))
  # https://spatialreference.org/ref/sr-org/7589/
  terra::crs(r) <- "epsg:7589"


  if (all(!is.null(upper_left), !is.null(height), !is.null(width))) {

    xmn <- terra::xFromCol(r, upper_left[1])
    xmx <- terra::xFromCol(r, upper_left[1] + width)
    ymx <- terra::yFromRow(r, upper_left[2])
    ymn <- terra::yFromRow(r, upper_left[2] + height)

    if (any(is.na(xmn), is.na(xmx), is.na(ymn), is.na(ymx))) {
      stop(
        paste(
          "The selection is outside the picture border, please",
          "review upper_left, height, and width."
        )
      )
    }
    e <- terra::ext(xmn, xmx, ymn, ymx)
    r <- terra::crop(r, e)
    terra::ext(r) <- terra::ext(0, ncol(r), 0, nrow(r))
  }

  file_ext <- filenamer::as.filename(path)$ext %>% toupper()

  if (file_ext %in% c("JPG", "JPEG", "TIF", "TIFF")) {
    if (terra::nlyr(r) == 3) names(r) <- c("Red", "Green", "Blue")
  } else {
    if (terra::nlyr(r) == 3) {
      names(r) <- c("Red", "Green", "Blue")
      warning("Layers were named presuming an RGB file")
    }
  }
  r
}



