#' Generate an ellipse projected in equidistant space
#'
#' Compute the image coordinates of an ellipse as it appears under an
#' equidistant projection, centered at a given pixel and oriented by zenith and
#' azimuth angles.
#'
#' @param x numeric vector of length one. Column coordinate of the ellipse center.
#' @param y numeric vector of length one. Row coordinate of the ellipse center.
#' @param theta numeric vector of length one. Zenith angle (deg) at `(x, y)`.
#' @param phi numeric vector of length one. Azimuth angle (deg). Aligns the
#'   ellipse minor axis with the radius from the image center.
#' @param size_px numeric vector of length one. Diameter (pixels) of the
#'   circular sampling area at the image center; under the equidistant
#'   projection it becomes an ellipse away from the center.
#' @param transpose logical vector of length one. Whether to draw the ellipse on
#'   the transposed image prior to rotation (useful when source data were
#'   captured in portrait mode to keep on-screen orientation consistent).
#'
#' @return two-column matrix with `x` and `y` image coordinates of the ellipse.
#' @noRd
.make_elli <- function(x, y, theta, phi, size_px, transpose = FALSE) {
  # Ellipse aligned with the raster grid
  theta <- theta*pi/180
  if (transpose) {
    rx <- size_px
    ry <- size_px * (theta/sin(theta))
    angle <- (360 - phi) * pi / 180
  } else {
    rx <- size_px * (theta/sin(theta))
    ry <- size_px
    angle <- phi * pi / 180
  }

  t <- seq(0, 2 * pi, length.out = 50)
  ell_x <- rx * cos(t)
  ell_y <- ry * sin(t)

  # Rotate the ellipse to align it with the projection
  x_rot <- x + ell_x * cos(angle) - ell_y * sin(angle)
  y_rot <- y + ell_x * sin(angle) + ell_y * cos(angle)

  cbind(x = x_rot, y = y_rot)
}


#' Extract radiometry data for calibration
#'
#' @description
#' Buid a datasets for vignetting modeling by sistematically extracting
#' radiometry from images taken with the aid of a portable light source and the
#' calibration board detailed in [calibrate_lens()].
#'
#' @section Guidance:
#' Lenses have the inconvenient property of increasingly attenuating light along
#' the direction orthogonal to the optical axis. This phenomenon is known as the
#' vignetting effect and varies with lens model and aperture setting. The method
#' outlined here, known as the simple method, is explained in details in
#' \insertCite{Diaz2024;textual}{rcaiman}. Next explanation might serve mostly
#' as a quick recap of it.
#'
#' The development of the simple method was done with a Kindle Paperwhite eBooks
#' reader of 6" with built-in light. However, an iPhone 6 plus was also tested
#' in the early stages of development and no substantial differences were
#' observed. A metal bookends desk book holder was used to fasten the eBook
#' reader upright and a semi-transparent paper to favor a Lambertian light
#' distribution. In addition, the latter was used to draw on in order to
#' guide pixel sampling. The book holder also facilitated the alignment of the
#' screen with the dotted lines of the printed quarter-circle.
#' \if{html}{\figure{lightSource.jpg}{options: style="display:block;margin:0 auto;max-width:50%;"}}
#' \if{latex}{\figure{lightSource.jpg}}
#' As a general guideline, a wide variety of mobile devices could be used as
#' light sources, but if scattered data points are obtained with it, then other
#' light sources should be tested in order to double check that the light
#' quality is not the reason for points scattering.
#'
#' With the room only illuminated by the portable light source, nine photographs
#' should be taken with the light source located in the equivalent to 0, 10, 20,
#' 30, 40, 50, 60, 70, and 80 degrees of zenith angle, respectively. Camera
#' configuration should be in manual mode and set with the aperture (f/number)
#' for which a vignetting function is required. The shutter speed should be
#' regulated to obtain light-source pixels with middle grey values. The nine
#' photographs should be taken **without changing the camera configuration and
#' the light conditions**.
#' \if{html}{\figure{calibrationBoardVignetting.jpg}{options: style="display:block;margin:0 auto;max-width:50%;"}}
#' \if{latex}{\figure{calibrationBoardVignetting.jpg}}
#' This code exemplifies how to use the function to obtain data and base R
#' functions to obtain the vignetting function (\eqn{f_v}).
#' ````
#' zenith_colrow <- c(1500, 997)
#' diameter <- 947*2
#' z <- zenith_image(diameter, c(0.689, 0.0131, -0.0295))
#' a <- azimuth_image(z)
#'
#' .read_raw <- function(path_to_raw_file) {
#'   r <- read_caim_raw(path_to_raw_file, only_blue = TRUE)
#'   r <- crop_caim(r, zenith_colrow - diameter/2, diameter, diameter)
#'   r <- fisheye_to_equidistant(r, z, a, m, radius = diameter/2,
#'                               k = 1, p = 1, rmax = 100)
#' }
#'
#' l <- Map(.read_raw, dir("raw/up/", full.names = TRUE))
#' up_data <- extract_radiometry(l)
#' l <- Map(.read_raw, dir("raw/down/", full.names = TRUE))
#' down_data <- extract_radiometry(l)
#' l <- Map(.read_raw, dir("raw/right/", full.names = TRUE))
#' right_data <- extract_radiometry(l)
#' l <- Map(.read_raw, dir("raw/left/", full.names = TRUE))
#' left_data <- extract_radiometry(l)
#'
#' ds <- rbind(up_data, down_data, right_data, left_data)
#'
#' plot(ds, xlim = c(0, pi/2), ylim= c(0.5,1.05),
#'       col = c(rep(1,9),rep(2,9),rep(3,9),rep(4,9)))
#' legend("bottomleft", legend = c("up", "down", "right", "left"),
#'        col = 1:4, pch = 1)
#'
#' fit <- lm((1 - ds$radiometry) ~ poly(ds$theta, 3, raw = TRUE) - 1)
#' summary(fit)
#' coef <- -fit$coefficients #did you notice the minus sign?
#' .fv <- function(x) 1 + coef[1] * x + coef[2] * x^2 + coef[3] * x^3
#' curve(.fv, add = TRUE, col = 2)
#' coef
#' ````
#' Once one of the aperture settings is calibrated, it can be used to calibrate
#' all the rest. To do so,  the equipment should be used to take photographs in
#' all desired exposition and without moving the camera, including the aperture
#' already calibrated and preferably under an open sky in stable diffuse light
#' conditions. Below code can be used as a template.
#'
#' ````
#' zenith_colrow <- c(1500, 997)
#' diameter <- 947*2
#' z <- zenith_image(diameter, c(0.689, 0.0131, -0.0295))
#' a <- azimuth_image(z)
#'
#' files <- dir("raw/", full.names = TRUE)
#' l <- list()
#' for (i in seq_along(files)) {
#'   if (i == 1) {
#'     # because the first aperture was the one already calibrated
#'     .read_raw <- function(path_to_raw_file) {
#'         r <- read_caim_raw(path_to_raw_file, only_blue = TRUE)
#'         r <- crop_caim(r, zenith_colrow - diameter/2, diameter, diameter)
#'         r <- correct_vignetting(r, z, c(0.0302, -0.320, 0.0908))
#'         r <- fisheye_to_equidistant(r, z, a, m, radius = diameter/2,
#'                                     k = 1, p = 1, rmax = 100)
#'       }
#'   } else {
#'       .read_raw <- function(path_to_raw_file) {
#'           r <- read_caim_raw(path_to_raw_file, only_blue = TRUE)
#'           r <- crop_caim(r, zenith_colrow - diameter/2, diameter, diameter)
#'           r <- fisheye_to_equidistant(r, z, a, m, radius = diameter/2,
#'                                       k = 1, p = 1, rmax = 100)
#'         }
#'   }
#'   l[[i]] <- .read_raw(files[i])
#' }
#'
#' ref <- l[[1]]
#' rings <- ring_segmentation(zenith_image(ncol(ref), lens()), 3)
#' theta <- seq(1.5, 90 - 1.5, 3) * pi/180
#'
#' .fun <- function(r) {
#'   r <- extract_feature(r, rings, return = "vector")
#'   r/r[1]
#' }
#'
#' l <- Map(.fun, l)
#'
#' .fun <- function(x) {
#'   x / l[[1]][] # because the first is the one already calibrated
#' }
#' radiometry <- Map(.fun, l)
#'
#' l <- list()
#' for (i in 2:length(radiometry)) {
#'   l[[i-1]] <- data.frame(theta = theta, radiometry = radiometry[[i]][])
#' }
#' ds <- l[[1]]
#' head(ds)
#' ````
#' The result is one dataset (ds) for each file. This is all what it is needed
#' before using base R functions to fit a vignetting function
#'
#' @param l list of preprocessed images ([terra::SpatRaster-class]) suitable for
#'   radiometry sampling. Images must comply with the equidistant projection.
#' @param size_px numeric vector of length one. Diameter (pixels) of the
#'   circular sampling area at the image center; off-center, the sampled region
#'   becomes an ellipse under the equidistant projection. If `NULL` (default),
#'   two percent of image width is used (`round(terra::ncol(r) * 0.02)`).
#'
#' @references \insertAllCited{}
#'
#' @note
#' This function does not fit the vignetting function itself. The output is a
#' dataset to be used in subsequent modeling steps. See above sections for
#' guidance.
#'
#' @return `data.frame` con dos columnas:
#' \describe{
#'   \item{`theta`}{zenith angle en radianes.}
#'   \item{`radiometry`}{digital number normalizado.}
#' }
#'
#' @export
extract_radiometry <- function(l, size_px = NULL) {
  msn <- "`l` must be a list composed of single-layer [terra::SpatRaster-class]."
  if (is.list(l)) {
    tryCatch(rast(l), error = function(e) stop(msn))
    lapply(l, function(x) tryCatch(.assert_single_layer(x),
                                   error = function(e) stop(msn))
           )
  } else {
    stop(msn)
  }
  .check_vector(size_px, "numeric", 1, allow_null = TRUE, sign = "positive")

  r <- max(terra::rast(l))
  z <- zenith_image(ncol(r), lens())
  a <- azimuth_image(z)

  if (is.null(size_px)) {
    size_px <- round(terra::ncol(r) * 0.02)
  }
  .size_fun <- function(theta, size_px) {
    theta <- theta*pi/180
    size_px * (theta/sin(theta))
  }

  message("Look for instructions on the popup window")

  grDevices::x11()
  plot(r, legend = FALSE, col = grDevices::grey((1:255)/255))
  .show_popup(
   "Mark two points to zoom in to the area of interest.
   Think these points as the opposite corners of a rectangle."
  )
  r <- terra::crop(r, terra::draw())
  z <- terra::crop(z, r)
  a <- terra::crop(a, r)
  if ((terra::ncol(r) / terra::nrow(r)) < 1) {
    r <- terra::t(r)
    z <- terra::t(z)
    a <- terra::t(a)
    transpose <- TRUE
  } else {
    transpose <- FALSE
  }
  plot(r, legend = FALSE, col = grDevices::grey((1:255)/255))

  .show_popup("Mark one point in each central cross.")

  coords <- terra::click(n = 1)
  points(coords, pch = 4, col = "red", cex = 1)

  for (i in 2:length(l)) {
    foo <- terra::click(n = 1)
    points(foo, pch = 4, col = "red", cex = 1)
    coords <- rbind(coords, foo)
  }


  img_points <- data.frame(col = terra::colFromX(r, coords[,1]),
                           row = terra::rowFromY(r, coords[,2]))

  theta <- extract_dn(z, img_points)[,3]
  phi <- extract_dn(a, img_points)[,3]

  z <- matrix(ncol = 5) %>% as.data.frame()
  colnames(z) <- c("object", "part", "x", "y", "hole")
  for (i in seq_along(img_points$row)) {
    elli <- .make_elli(coords[i,1], coords[i,2], theta[i], phi[i], size_px,
                       transpose)
    elli <- cbind(i, 1, elli, 0)
    elli2 <- .make_elli(coords[i,1], coords[i,2], theta[i], phi[i], size_px/2,
                        transpose)
    elli2 <- cbind(i, 1, elli2, 1)
    colnames(elli) <- c("object", "part", "x", "y", "hole")
    if (i == 1) {
      z <- rbind(elli, elli2)
    } else {
      z <- rbind(z, elli, elli2)
    }
  }

  p <- vect(z, "polygons")
  plot(p, add = TRUE)
  dns <- terra::extract(r, p, fun = stats::median)
  theta <- theta*pi/180
  radiometry <- dns[, 2]
  ds <- data.frame(theta, radiometry)

  ds <- rbind(ds, -ds)
  ds$radiometry <- abs(ds$radiometry)
  spline_fun <- splinefun(ds)
  ds <- ds[1:(nrow(ds)/2),]
  ds$radiometry <- ds$radiometry / spline_fun(0)

  ds
}
