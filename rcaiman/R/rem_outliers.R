#' Remove statistical outliers in sky points
#'
#' @description
#' Remove sky points considered outliers relative to their local
#' neighbors in a user-specified variable.
#'
#' @details
#' Based on the Statistical Outlier Removal (SOR) filter from the
#' [PCL library](https://pointclouds.org/). Distances are computed on a spherical
#' surface. The number of neighbors is controlled by `k`, and `angular_radius`
#' sets the maximum search radius (deg). If fewer than `k` neighbors are found
#' within that radius, the point is retained due to insufficient evidence for
#' removal. The decision criterion follows
#' \insertCite{Leys2013;textual}{rcaiman}:
#'
#' \eqn{M - laxity \times MAD < x_i < M + laxity \times MAD}
#'
#' where \eqn{x_i} is the value from `r` at sky point `i`,  \eqn{M} and
#' \eqn{MAD} are the median and median absolute deviation, respectively,
#' computed from the the neighbors of \eqn{x_i}, and \eqn{laxity} is the
#' user-defined threshold.
#'
#' `cutoff_side` controls which side(s) of the inequality are evaluated:
#' `"both"` (default), `"left"` (left tail only), or `"right"` (right tail
#' only).
#'
#' @inheritParams extract_dn
#' @inheritParams sky_grid_segmentation
#' @inheritParams interpolate_spherical
#' @param angular_radius numeric vector of length one. The maximum radius for
#'   searching k-nearest neighbors (KNN) in degrees.
#' @param laxity numeric vector of length one.
#' @param cutoff_side character vector of length one. Options are "both"
#'   (default), "upper" or "lower". Controls which side(s) of the inequality are
#'   evaluated to detect outliers. See Details.
#' @param trend numeric vector of length one or `NULL`. Zero to three. Specifies
#'   the order of the polynomial surface fitted to the neighbors to account for
#'   spatial trends. Use NULL (default) to skip detrending.
#'
#' @note This function assumes that `sky_points` and the
#' [terra::SpatRaster-class] objects refer to the same image geometry. No checks
#' are performed.
#'
#' @return The retained points represented as a [data.frame] with columns `row`
#'   and `col`, same as `sky_points`.
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' r <- caim$Blue
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#' m <- !is.na(z)
#' bin <- binarize_by_region(r, ring_segmentation(z, 30),
#'                           method = "thr_isodata")
#' bin <- bin & select_sky_region(z, 0, 80)
#' g <- sky_grid_segmentation(z, a, 5, first_ring_different = TRUE)
#' sky_points <- extract_sky_points(r, bin, g,
#'                                  dist_to_black = 3)
#' plot(r)
#' points(sky_points$col, nrow(caim) - sky_points$row, col = 2, pch = 10)
#'
#' sky_points <- rem_outliers(sky_points, r, z, a,
#'                                  k = 5,
#'                                  angular_radius = 20,
#'                                  laxity = 2,
#'                                  cutoff_side = "left")
#' points(sky_points$col, nrow(caim) - sky_points$row, col = 3, pch = 0)
#' }
rem_outliers <- function(sky_points, r, z, a,
                         k = 20,
                         angular_radius = 20,
                         laxity = 2,
                         cutoff_side = "both",
                         use_window = TRUE,
                         trend = NULL) {

  .check_sky_points(sky_points)
  .check_r_z_a_m(r, z, a, r_type = "single")
  .check_vector(k, "integerish", 1, sign = "positive")
  if (k < 3) stop("`k` must be greater than three.")
  .check_vector(angular_radius, "numeric", 1, sign = "positive")
  .check_vector(laxity, "numeric", 1, sign = "positive")
  .assert_choice(cutoff_side, c("both", "left", "right"))
  .check_vector(use_window, "logical", 1)
  .check_vector(trend, "integerish", 1, allow_null = TRUE,  sign = "positive")
  if (!is.null(trend)) stop("`trend` must be lower than four.")

  angular_radius <- .degree2radian(angular_radius)

  sky_points <- extract_dn(c(z, a, r), sky_points, use_window = TRUE)
  names(sky_points)[3:5] <- c("z", "a", "dn")

  .calculate_sor <- function(i) {
    sky_points[, c("z", "a")] <- .degree2radian(sky_points[, c("z", "a")])
    spherical_distance <- calc_spherical_distance (sky_points$z,
                                                   sky_points$a,
                                                   sky_points[i, "z"],
                                                   sky_points[i, "a"])
    order_idx <- order(spherical_distance)
    sorted_distance <- spherical_distance[order_idx][2:(k + 1)]
    if (!is.null(trend)) stopifnot(trend <= 6)
    tryCatch(
      if (all(sorted_distance <= angular_radius)) {
        if (!is.null(trend)) {
          the_point <- sky_points[order_idx[1], c("row", "col", "dn")]
          sky_points <- sky_points[order_idx[2:(k + 1)], c("row", "col", "dn")]
          xy <- terra::cellFromRowCol(r, sky_points$row, sky_points$col) %>%
            terra::xyFromCell(r, .)
          fit <- spatial::surf.ls(x = xy[, 1],
                                  y = xy[, 2],
                                  z = sky_points[, "dn"],
                                  np = trend)
          pred <- predict(fit, xy[,1], xy[,2])
          dns <- fit$z - pred
          xy <- terra::cellFromRowCol(r, the_point$row, the_point$col) %>%
            terra::xyFromCell(r, .)
          the_point_dn <- the_point[, "dn"] - predict(fit, xy[,1], xy[,2])
        } else {
          the_point_dn <- sky_points[order_idx[1], "dn"]
          dns <- sky_points[order_idx[2:(k + 1)], "dn"]
        }
        return(c(the_point_dn,
                 stats::median(dns, na.rm = TRUE),
                 stats::mad(dns, na.rm = TRUE)))
      } else {
        return(c(NA, NA, NA))
      },
      error = function(e) {
      stop("Try other order of magnitud for 'trend' or look
       for sky points below the horizon or on it.")
      }
    )
  }

  result <- lapply(seq_len(nrow(sky_points)), .calculate_sor) %>% unlist() %>%
      matrix(., ncol = 3, byrow = TRUE)

  value <- result[, 1]
  central_tendency <- result[, 2]
  dispersion <- result[, 3]

  deviation <- (value - central_tendency) / dispersion
  i <- switch(cutoff_side,
              right = deviation <= laxity,
              left = deviation >= -laxity,
              both = abs(deviation) <= laxity)

  i[is.na(i)] <- TRUE
  sky_points[i, c("row", "col") ]
}
