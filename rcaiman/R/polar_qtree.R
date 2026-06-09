#' Generate polar quadtree segmentation
#'
#' @description
#' Segment a hemispherical image into large circular trapezoids and
#' recursively split them into four trapezoids of equal angular size whenever
#' brightness heterogeneity exceeds a predefined threshold.
#'
#' @details
#' A circular trapezoid, hereafter referred to as a cell, is the
#' intersection of a ring (zenith‑angle band) and a sector (azimuth‑angle band).
#' Heterogeneity within a cell is measured as the standard deviation of pixel
#' values (a first‑order texture metric). The change in heterogeneity due to
#' splitting is `delta`, defined as the sum of the standard deviations of the
#' four subcells minus the standard deviation of the parent cell. A split is
#' kept where `delta > scale_parameter`. For multi‑layer `r`, `delta` is
#' computed per layer and averaged to decide splits. Angular resolution at level
#' `i` is `angle_width / 2^i`.
#'
#' @param r numeric [terra::SpatRaster-class]. One or more layers used to drive
#'   heterogeneity.
#' @param scale_parameter numeric vector of length one. Threshold on `delta`
#'   controlling splits (see *Details*).
#' @param max_splittings numeric vector of length one. Maximum recursion depth.
#'
#' @inheritParams sky_grid_segmentation
#'
#' @return  Single-layer [terra::SpatRaster-class] with integer values and the
#'   same number of rows and columns as `r`.
#'
#' @export polar_qtree
#'
#' @examples
#' \dontrun{
#' # Find large patches of white ---------------------------------------------
#' caim <- read_caim()
#' r <- caim$Blue
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#'
#' bin <- binarize_with_thr(r, thr_isodata(r[]))
#' plot(bin)
#'
#' seg <- polar_qtree(bin, z, a, 0, 30, 3)
#' plot(extract_feature(bin, seg) == 1)
#' }
polar_qtree <- function(r, z, a,
                        scale_parameter,
                        angle_width = 30,
                        max_splittings = 6) {
  .check_r_z_a_m(r, z, a, r_type = "any")
  .check_vector(scale_parameter, "numeric", 1, sign = "nonnegative")
  .check_vector(angle_width, "numeric", 1, sign = "positive")
  .check_vector(max_splittings, "integerish", 1, sign = "positive")

  angle.wds <- vapply(0:max_splittings, function(x) angle_width/2^x, 1)
  ges <- Map(sky_grid_segmentation, z, a, angle.wds)
  .calc_delta_single_layer <- function(r) {
    if (any(is.na(r)[])) {
      .sd <- function(x) {
        x <- sd(x, na.rm = TRUE)
        if (all(is.na(x))) x <- 0
        x
      }
      sd_now <- Map(function(g) extract_feature(r, g, .sd,
                                                return = "vector"),
                    ges)
    } else {
      sd_now <- Map(function(g) extract_feature(r, g, sd,
                                                return = "vector"),
                    ges)
    }
    indices_if_split <- Map(function(i) extract_feature(ges[[i-1]], ges[[i]],
                                                        max,
                                                        return = "vector"),
                            2:length(ges))
    sd_if_split <- Map(function(i) tapply(sd_now[[i+1]],
                                          indices_if_split[[i]], sum),
                       1:(length(ges)-1))
    delta <- Map(function(i) sd_if_split[[i]] - sd_now[[i]],
                 seq_along(sd_if_split))
    delta
  }

  if (terra::nlyr(r) > 1) {
    delta <- Map(.calc_delta_single_layer, as.list(r))
    delta <- Map(function(i) {
                  x <- Map(function(j) delta[[j]][[i]], seq_along(delta))
                  apply(as.data.frame(x), 1, mean)
               }, seq_along(delta[[1]]))
  } else {
    delta <- .calc_delta_single_layer(r)
  }

  it_should_be_splited <- lapply(delta, function(x) x > scale_parameter)

  it_should_be_splited <- lapply(
    seq_along(it_should_be_splited),
    function(i) {
      terra::subst(
        ges[[i]],
        names(delta[[i]]) %>%
          as.numeric(),
        it_should_be_splited[[i]]
      )
    }
  )

  for (i in 1:(length(it_should_be_splited) - 1)) {
    it_should_be_splited[[i + 1]] <- it_should_be_splited[[i]] *
      it_should_be_splited[[i + 1]]
  }

  g <- ges[1]
  ges <- ges[-1]
  ges <- Map(function(i) {
    r <- ges[[i]]
    r[r == 0] <- NA
    # r + i*10000000
    r + max(ges[[i]][], na.rm = TRUE)
  }, seq_along(ges))
  ges <- terra::rast(ges)

  it_should_be_splited <- terra::rast(it_should_be_splited)
  it_should_be_splited[is.na(it_should_be_splited)] <- 0

  seg <- ges * it_should_be_splited
  seg <- terra::rast(c(g, seg))
  seg <- max(seg)
  seg[is.na(seg)] <- 0

  names(seg) <- "Polar quad-tree"
  seg
}
