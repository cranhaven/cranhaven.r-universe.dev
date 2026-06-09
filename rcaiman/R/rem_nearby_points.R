#' Remove nearby sky points
#'
#' @description
#' Select a subset of points so that no retained pair is closer than `min_dist`
#' in planar or spherical space.
#'
#' @details
#' When `space = "planar"`, distances are computed in image coordinates and `z`
#' and `a` are ignored. When `space = "spherical"`, distances are angular (deg)
#' in hemispherical coordinates. If `r` is provided, points are ranked by the
#' extracted raster values and retained in descending order.
#'
#' @inheritParams extract_rr
#' @inheritParams sky_grid_segmentation
#' @param min_dist numeric vector of length one. Minimum allowed distance
#'   between retained points. Units: pixels for `"planar"`, deg for `"spherical"`.
#' @param r single-layer [terra::SpatRaster-class] or `NULL`. Optional ranking
#'   raster used to prioritize retention (higher values kept first).
#' @param space character vector of length one. Coordinate system for distances:
#'   `"planar"` (default) or `"spherical"`.
#'
#' @note
#' It is assumed that `sky_points` were extracted from an image with the same
#' dimensions as the `r`, `z`, and `a` rasters. No checks are performed.
#'
#' @return A `data.frame` with columns `row` and `col` for retained points.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' r <- caim$Blue
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#' bin <- binarize_by_region(r, ring_segmentation(z, 30),
#'                           method = "thr_isodata")
#' bin <- bin & select_sky_region(z, 0, 80)
#' g <- sky_grid_segmentation(z, a, 10, first_ring_different = TRUE)
#' sky_points <- extract_sky_points(r, bin, g, dist_to_black = 3)
#'
#' # planar
#' sky_points_p <- rem_nearby_points(sky_points, r, min_dist = 100,
#'                                         space = "planar")
#' plot(r)
#' points(sky_points$col, nrow(caim) - sky_points$row, col = 2, pch = 10)
#' points(sky_points_p$col, nrow(caim) - sky_points_p$row, col = 3, pch = 0)
#'
#' # spherical
#' sky_points_s <- rem_nearby_points(sky_points, r, z, a, min_dist = 30,
#'                                         space = "spherical")
#' plot(r)
#' points(sky_points$col, nrow(caim) - sky_points$row, col = 2, pch = 10)
#' points(sky_points_s$col, nrow(caim) - sky_points_s$row, col = 3, pch = 0)
#' }
rem_nearby_points <- function(sky_points,
                              r = NULL,
                              z = NULL,
                              a = NULL,
                              min_dist = 3,
                              space = "planar",
                              use_window = TRUE) {
  if (!is.null(z)) .assert_single_layer(a)
  if (!is.null(r) && !is.null(z) && !is.null(a)) {
    .check_r_z_a_m(r, z, a, r_type = "single")
  }
  if (is.null(r) && !is.null(z) && !is.null(a)) {
    .check_r_z_a_m(NULL, z, a, r_type = "single")
  }
  .check_vector(min_dist, "integerish", 1, sign = "nonnegative")
  .assert_choice(space, c("planar", "spherical"))
  .check_vector(use_window, "logical", 1)

  .dist_spherical <- function(zenith_azimuth) {
    theta <- zenith_azimuth[, 1]  # zenith (rad)
    phi   <- zenith_azimuth[, 2]  # azimuth (rad)

    # Convert to unit vectors in Cartesian coordinates
    x <- sin(theta) * cos(phi)
    y <- sin(theta) * sin(phi)
    z <- cos(theta)

    # Matrix of 3D unit vectors
    V <- cbind(x, y, z)

    # Compute pairwise dot products
    dot <- tcrossprod(V)  # n x n matrix of dot products

    # Clamp to avoid acos() domain errors due to numerical issues
    dot <- pmin(pmax(dot, -1), 1)

    # Angular distances (radians)
    d <- acos(dot)

    rownames(d) <- rownames(zenith_azimuth)
    colnames(d) <- rownames(zenith_azimuth)

    d
  }

  if (!is.null(r)) {
    ord <- extract_dn(r, sky_points, use_window = use_window)[,3]
    ord <- order(ord, decreasing = TRUE)
    sky_points <- sky_points[ord, ]
  }

  if (space == "planar") {
    coords <- sky_points[, c("col", "row")]
    dists <- as.matrix(stats::dist(coords))
  } else {
    sky_points <- extract_dn(c(z, a), sky_points, use_window = FALSE)
    coords <- sky_points[, c(3, 4)] %>% .degree2radian()
    dists <- .dist_spherical(coords)
    min_dist <- .degree2radian(min_dist)
  }

  n <- nrow(coords)
  selected <- logical(n)
  considered <- logical(n)
  to_consider <- seq_len(n)
  while (length(to_consider) > 0) {
    i <- to_consider[1]
    selected[i] <- TRUE
    considered[i] <- TRUE
    too_close <- which(dists[i, ] <= min_dist)
    considered[too_close] <- TRUE
    to_consider <- which(!considered)
  }
  sky_points[selected, c("row", "col"), drop = FALSE]
}
