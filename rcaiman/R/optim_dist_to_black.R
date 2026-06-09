#' Optimize minimum distance to black pixels
#'
#' @description
#' Estimate an optimal buffer (`dist_to_black`) to keep sampled sky points away
#' from candidate canopy pixels (black pixels).
#'
#' @details
#' The heuristic seeks the largest buffer that still yields uniform angular
#' coverage. It iteratively decreases `dist_to_black` while monitoring the
#' percentage of 30 deg sky‑grid cells covered by sampled points. If coverage
#' is low, the buffer is relaxed (and may be removed). This balances border
#' avoidance with representativeness across the sky vault.
#'
#' @inheritParams sky_grid_segmentation
#' @inheritParams extract_sky_points
#' @inheritParams compute_canopy_openness
#'
#' @return numeric vector of length one to be passed as `dist_to_black` to
#'   [extract_sky_points()], or `NULL` if no buffer is advised.
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
#' g <- sky_grid_segmentation(z, a, 10, first_ring_different = TRUE)
#'
#' dist_to_black <- optim_dist_to_black(r, z, a, m, bin, g)
#' dist_to_black
#'
#' bin <- grow_black(bin, 11)
#' plot(bin)
#' dist_to_black <- optim_dist_to_black(r, z, a, m, bin, g)
#' dist_to_black
#' }
#' @export
optim_dist_to_black <- function(r, z, a, m, bin, g) {
  .check_r_z_a_m(r, z, a, m, r_type = "single")
  .assert_logical_mask(bin)
  .assert_sky_grid(g)

  g30 <- sky_grid_segmentation(z, a, 30)
  g30[!m] <- 0
  dist_to_black <- 11
  sampling_pct <- 0
  while (sampling_pct < 100 & dist_to_black > 3) {
    dist_to_black <- dist_to_black - 2
    sky_points <- extract_sky_points(r, bin, g,
                                     dist_to_black = dist_to_black)
    v <- cellFromRowCol(r, sky_points$row, sky_points$col) %>%
      xyFromCell(r, .) %>% vect()
    sampling_pct <- (extract(g30, v)[,2] %>% unique() %>% length()) /
      (unique(g30)[,1] %>% length() %>% subtract(1)) * 100
  }
  if (sampling_pct < 75) {
    dist_to_black <- 1
    sky_points <- extract_sky_points(r, bin, g,
                                     dist_to_black = dist_to_black)
    v <- cellFromRowCol(r, sky_points$row, sky_points$col) %>%
      xyFromCell(r, .) %>% vect()
    sampling_pct <- (extract(g30, v)[,2] %>% unique() %>% length()) /
      (unique(g30)[,1] %>% length() %>% subtract(1)) * 100
  }
  if (sampling_pct < 50) {
    dist_to_black <- NULL
  }
  dist_to_black
}
