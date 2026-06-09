#' Estimate sun angular coordinates
#'
#' Estimates the sun’s zenith and azimuth angles (deg) from a canopy
#' hemispherical photograph, using either direct detection of the solar disk or
#' indirect cues from the circumsolar region.
#'
#' This function can operate under two alternative assumptions for
#' estimating the sun position:
#'
#' \describe{
#'   \item{Veiled sun}{The solar disk is visible or partially obscured; the
#'   position is inferred from localized brightness peaks.}
#'   \item{Obscured sun}{The solar disk is not visible; the position is inferred
#'   from radiometric and spatial cues aggregated over the circumsolar region.}
#' }
#'
#' When `method = "assume_veiled"`, `g` and `angular_radius_sun` are ignored.
#' Estimates refer to positions above the horizon; therefore, estimated angles
#' may require further manipulation if the photograph was acquired under
#' crepuscular light.
#'
#' @note A scientific article presenting and validating this method is currently
#' under preparation.
#'
#' @param g single-layer [terra::SpatRaster-class] with integer values. Sky
#'   segmentation map produced by [sky_grid_segmentation()].
#' @param method character vector of length one. Estimation mode:
#'   `"assume_obscured"` (default) or `"assume_veiled"`.
#' @param angular_radius_sun numeric vector of length one. Maximum angular
#'   radius (in degrees) used to define the circumsolar region.
#'
#' @inheritParams extract_sky_points
#' @inheritParams sky_grid_segmentation
#'
#' @returns Named numeric vector of length two, with names `z` and `a`,
#'   representing the sun’s zenith and azimuth angles (in degrees).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#' m <- !is.na(z)
#' r <- caim$Blue
#'
#' bin <- binarize_by_region(r, ring_segmentation(z, 15), "thr_isodata") &
#'   select_sky_region(z, 0, 88)
#'
#' g <- sky_grid_segmentation(z, a, 10)
#' sun_angles <- estimate_sun_angles(r, z, a, bin, g,
#'                                   angular_radius_sun = 30)
#' row_col <- row_col_from_zenith_azimuth(z, a,
#'                                        sun_angles["z"],
#'                                        sun_angles["a"])
#' plot(caim$Blue)
#' points(row_col[1,2], nrow(caim) - row_col[1,1], col = "yellow",
#'        pch = 8, cex = 3)
#' }
estimate_sun_angles <- function(r, z, a, bin, g,
                                angular_radius_sun = 30,
                                method = "assume_obscured") {

  .this_requires_EBImage()

  .check_r_z_a_m(r, z, a, r_type = "single")
  .assert_logical_mask(bin)
  .assert_same_geom(r, bin)
  .assert_choice(method, c("assume_obscured", "assume_veiled"))


  if (method == "assume_obscured") {
    .assert_sky_grid(g)
    .assert_same_geom(r, g)
    .check_vector(angular_radius_sun, "numeric", 1, sign = "positive")

    if (!is.numeric(angular_radius_sun) || length(angular_radius_sun) != 1) {
      stop("`angular_radius_sun` must be a numeric vector of length one.")
    }
    if (!.is_sky_grid(g)) {
      stop("'g' must be the output of 'sky_grid_segmentation()'.")
    }
    if (!terra::compareGeom(r, g, stopOnError = FALSE, messages = FALSE)) {
      stop("`g` must be of the same dimensions as `r`")
    }

    # Select cells with at least one extremely bright sky pixel
    g[!bin] <- NA
    r <- extract_feature(r, g, function(x) quantile(x, 0.99, na.rm = TRUE))
    m <- r >= quantile(unique(terra::values(r)), 0.9, na.rm = TRUE)
    m[is.na(m)] <- 0

    # Merge adjacent white segments
    labeled_m <- EBImage::bwlabel(as.array(m)[,,1])
    labeled_m <- terra::rast(labeled_m)
    terra::ext(labeled_m) <- terra::ext(r)
    terra::crs(labeled_m) <- terra::crs(r)
    # Calc membership to class "sun seed"
    .fun <- function(x) {
      x <- unique(x) # to count the cells instead of the pixels
      length(x)
    }
    size <- extract_feature(g, labeled_m, .fun, return = "vector") %>%
      normalize_minmax()
    dn <- extract_feature(r, labeled_m, median, return = "vector") %>%
      normalize_minmax()
    if (any(is.nan(dn))) dn[] <- 1 #preventative programming
    if (any(is.nan(size))) size[] <- 1 #preventative programming
    membership_posibility <- size*0.25 + dn*0.75

    # Find sun seed
    sun <- which.max(membership_posibility)

    # Find circumsolar region
    ## get coordinates of every object
    rcells <- r
    rcells[] <- 1:ncell(r)
    .get_center <- function(x) {
      if (length(x) > 1) {
        xy <- terra::xyFromCell(r, x)
        x <- x[grDevices::chull(xy)]

        rad_z <- z[x] %>% .degree2radian()
        rad_a <- a[x] %>% .degree2radian()

        x3d <- sin(rad_z) * cos(rad_a)
        y3d <- sin(rad_z) * sin(rad_a)
        z3d <- cos(rad_z)

        x_mean <- mean(x3d[,])
        y_mean <- mean(y3d[,])
        z_mean <- mean(z3d[,])

        # to transform it to the unit vector in order to force it to be on the
        # sphere of radius one
        norm <- sqrt(x_mean^2 + y_mean^2 + z_mean^2)
        x_mean <- x_mean / norm
        y_mean <- y_mean / norm
        z_mean <- z_mean / norm

        # codify
        zenith <- acos(z_mean) %>% .radian2degree()
        azimuth <- atan2(y_mean, x_mean) %% (2 * pi) %>% .radian2degree()
        za <- c(zenith, azimuth)
      } else {
        za <- c(z[x][,], a[x][,])
      }
      round(za[1], 2) * 1e6 + round(za[2], 2)
    }
    za <-  extract_feature(rcells, labeled_m, .get_center, return = "vector")

    ## decodify
    zenith <- trunc(za/1e4)
    azimuth <- (za/1e4 - zenith) * 1e4
    zenith <- zenith/100
    za <- data.frame(zenith, azimuth) %>% .degree2radian()

    ## calc distance to sun seed
    d <- calc_spherical_distance(za[, 1], za[, 2], za[sun, 1], za[sun, 2])
    seg_labels <- extract_feature(labeled_m, labeled_m, return = "vector")

    ## classify circumsolar region based on distance
    i <- d > .degree2radian(angular_radius_sun)
    if (any(i)) {
      rcl <- data.frame(seg_labels[i], 0)
      m <- terra::classify(labeled_m, rcl)
    } else {
      m <- labeled_m
    }
    m <- m != 0

    # Calc coordinates of the circumsolar region
    za <-  extract_feature(rcells, m, .get_center, return = "vector")
    zenith <- trunc(za/1e4)
    azimuth <- (za/1e4 - zenith) * 1e4
    zenith <- zenith/100

    zenith_azimuth <- c(zenith, azimuth)
  } else {
    r[!bin] <- 0
    pan <- fisheye_to_pano(r, z, a,
                           function(x) quantile(x, 0.99, na.rm = TRUE), 1)
    zenith_azimuth <- terra::rowColFromCell(pan, which.max(pan[,])) %>%
      as.numeric()
  }
  sun_angles <- c(zenith_azimuth[1], zenith_azimuth[2])
  names(sun_angles) <- c("z", "a")
  sun_angles
}
