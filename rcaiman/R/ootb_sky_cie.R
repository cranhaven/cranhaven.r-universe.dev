#' Out-of-the-box CIE sky model and raster
#'
#' @description
#' Fit and validate a CIE general sky model from a canopy photograph without
#' manual parameter tuning, and return the predicted raster.
#'
#' @details
#' Runs a full pipeline to fit a CIE sky model to radiance from a canopy
#' image:
#'
#' \enumerate{
#'   \item a preliminary estimate of sky digital numbers is attempted using the
#'   two-corner method  aiming to start with a comprehensive sampling of the sky
#'   vault (see `method = "detect_bg_dn"` of [apply_by_direction()]).
#'   \item sky point extraction is performed with [extract_sky_points()], using
#'   information from a binary mask (`bin`) and post-filtering with
#'   [rem_nearby_points()] and [rem_outliers()].
#'   \item relative radiance is computed with [extract_rr()] and fitted to CIE
#'   sky models using [fit_cie_model()], selecting the best among different
#'   initial conditions and optimization methods.
#'   \item model validation is performed via [validate_cie_model()].
#'   \item raster prediction with [cie_image()].
#' }
#'
#' @note
#' This function is part of a paper under preparation.
#'
#' @param r numeric [terra::SpatRaster-class] of one layer. Typically, the blue
#'   band of a a canopy photograph. Digital numbers should be linearly related
#'   to radiance. See [read_caim_raw()] for details.
#' @param gs `list` where each element is the output of
#'   [sky_grid_segmentation()]. See *Examples* for guidance.
#' @param min_spherical_dist numeric vector. Values passed to
#'   [rem_nearby_points()].
#' @param method character vector. Optimization methods for [fit_cie_model()].
#' @param custom_sky_coef optional numeric vector of length five. If `NULL`
#'   (default), the 15 standard CIE skies are tested as starting conditions. Use
#'   this to avoid recomputing the initial step (depending only on `method`)
#'   when testing other inputs.
#'
#' @inheritParams compute_canopy_openness
#' @inheritParams sky_grid_segmentation
#' @inheritParams extract_sky_points
#' @inheritParams fit_trend_surface
#' @inheritParams fit_cie_model
#' @inheritParams apply_by_direction
#'
#' @export
#'
#' @return List with:
#' \describe{
#'   \item{`rr_raster`}{numeric [terra::SpatRaster-class]. Predicted relative radiance.}
#'   \item{`model`}{list returned by [fit_cie_model()]. The optimal fit.}
#'   \item{`model_validation`}{list returned by [validate_cie_model()].}
#'   \item{`dist_to_black`}{Value of `dist_to_black` used in [extract_sky_points()]
#'     for the optimal fit.}
#'   \item{`use_window`}{`logical`. Whether a window was used in [extract_rr()]
#'     for the optimal fit.}
#'   \item{`min_spherical_dist`}{Value of `min_dist` used in
#'     `rem_nearby_points(space = "spherical")` for the optimal fit.}
#'   \item{`sky_points`}{`data.frame` with columns `row` and `col`. Locations of
#'     sky points.}
#'   \item{`sun_row_col`}{`data.frame` with the estimated sun‑disk position in
#'     image coordinates.}
#'   \item{`g`}{Sky grid used for the optimal fit (as returned by [sky_grid_segmentation()]).}
#'   \item{`tested_grids`}{character vector describing the tested grid configurations.}
#'   \item{`tested_distances`}{character vector of tested `min_dist` values in
#'     `rem_nearby_points(space = "spherical")`.}
#'   \item{`tested_methods`}{character vector of optimization methods tested in
#'     [fit_cie_model()].}
#'   \item{`optimal_start`}{starting parameters selected after testing the 15 CIE skies.}
#'   \item{`model_up`}{model fitted to relative radiance detected with the
#'     two‑corner method, if that step succeeded; otherwise `NULL`.}
#' }
#'
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' r <- caim$Blue
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#' m <- !is.na(z)
#'
#' bin <- ootb_bin(caim, z, a, m, TRUE)
#'
#' set.seed(7)
#' gs <- list(
#'   #high res
#'   sky_grid_segmentation(z, a, 2.25, first_ring_different = TRUE),
#'   sky_grid_segmentation(z, a, 2.8125, first_ring_different = TRUE),
#'   #medium res
#'   sky_grid_segmentation(z, a, 9, first_ring_different = TRUE),
#'   sky_grid_segmentation(z, a, 10, first_ring_different = TRUE),
#'   #low res
#'   sky_grid_segmentation(z, a, 15, first_ring_different = FALSE),
#'   sky_grid_segmentation(z, a, 18, first_ring_different = FALSE)
#' )
#'
#' sky_cie <- ootb_sky_cie(r, z, a, m, bin, gs,
#'                         method = c("Nelder-Mead", "BFGS", "CG", "SANN"),
#'                         min_spherical_dist = seq(0, 12, 3),
#'                         parallel = TRUE)
#'
#' sky_cie$rr_raster
#' plot(sky_cie$rr_raster)
#' sky_cie$model_validation$rmse
#' plot(sky_cie$model_validation$pred, sky_cie$model_validation$obs)
#' abline(0,1)
#'
#' ratio <- r/sky_cie$rr_raster/sky_cie$model$rr$zenith_dn
#' plot(ratio)
#' plot(select_sky_region(ratio, 0.95, 1.05))
#' plot(select_sky_region(ratio, 1.15, max(ratio[], na.rm = TRUE)))
#'
#' plot(bin)
#' points(sky_cie$sky_points$col,
#'        nrow(caim) - sky_cie$sky_points$row, col = 2, pch = 10)
#'
#' }
ootb_sky_cie <- function(r, z, a, m, bin, gs,
                         min_spherical_dist = seq(0, 12, 3),
                         method = c("Nelder-Mead", "BFGS", "CG", "SANN"),
                         custom_sky_coef = NULL,
                         parallel = TRUE
                         ) {

  #basic checks handled by the functions called below

# Functions ---------------------------------------------------------------

  .get_marks_single <- function(r, z, a, m, bin, cv, g) {
    # Sun coordinate
    sun_angles <- estimate_sun_angles(r, z, a, bin, g, angular_radius_sun = 30,
                                      method = "assume_obscured")

    # Obtain parameter
    dist_to_black <- optim_dist_to_black(r, z, a, m, bin, g)

    # Sky points
    sky_points <- extract_sky_points(r, bin, g, dist_to_black)

    ## Apply filters
    sky_points <- rem_nearby_points(sky_points, r, min_dist = 3, space = "planar")

    sky_points <- rem_outliers(sky_points, cv, z, a,
                             k = 10,
                             angular_radius = 30,
                             laxity = 2,
                             cutoff_side = "right",
                             use_window = !is.null(dist_to_black))

    sky_points <- tryCatch(rem_outliers(sky_points, r, z, a,
                                      k = 20,
                                      angular_radius = 45,
                                      laxity = 3,
                                      cutoff_side = "both",
                                      use_window = !is.null(dist_to_black),
                                      trend = 3),
           error = function(e) rem_outliers(sky_points, r, z, a,
                                          k = 5,
                                          angular_radius = 20,
                                          laxity = 2,
                                          cutoff_side = "left",
                                          use_window = !is.null(dist_to_black))
    )

    rr <- extract_rr(r, z, a, sky_points,
                               no_of_points = 3,
                               use_window = !is.null(dist_to_black))
    list(dist_to_black = dist_to_black,
         rr = rr,
         sun_angles = sun_angles)
  }


  .get_marks_multi <- function(row_vals, col_vals,
                               r_vals, z_vals, a_vals,
                               m_vals, bin_vals, cv_vals,
                               angle_width, first_ring_different) {
    # Rebuilt rasters
    r <- terra::rast(nrows = max(row_vals, na.rm = TRUE),
                     ncols = max(col_vals, na.rm = TRUE))
    terra::ext(r) <- terra::ext(0, ncol(r), 0, nrow(r))
    # https://spatialreference.org/ref/sr-org/7589/
    terra::crs(r) <- "epsg:7589"
    terra::values(r) <- r_vals

    cv <- bin <- m <- a <- z <- rast(r)
    terra::values(z) <- z_vals
    names(z) <- "Zenith image"
    terra::values(a) <- a_vals
    names(a) <- "Azimuth image"
    terra::values(m) <- m_vals
    m <- binarize_with_thr(m, 0.5)
    terra::values(bin) <- bin_vals
    bin <- binarize_with_thr(bin, 0.5)
    terra::values(cv) <- cv_vals

    g <- sky_grid_segmentation(z, a, angle_width, first_ring_different)

    .get_marks_single(r, z, a, m, bin, cv, g)
  }


  .fit <- function(dist_to_black.rr.sun_angles,
                   g,
                   custom_sky_coef,
                   method,
                   parallel,
                   sun_angles2) {
    sun_angles <- dist_to_black.rr.sun_angles$sun_angles
    civic_twilight <- c(seq(sun_angles["z"], 90, length = 5), seq(91, 96, 1))


    suns <- lapply(seq_along(civic_twilight),
                   function(i) c(z = civic_twilight[i], sun_angles["a"]))
    suns[[length(suns) + 1]] <- sun_angles2

    models <- if (parallel) {
      # Only to avoid note from check, code is OK without this line.
      sun <- NA

      .with_cluster(.cores(), {
        foreach(sun = suns) %dopar% {
          fit_cie_model(dist_to_black.rr.sun_angles$rr,
                        sun,
                        custom_sky_coef = custom_sky_coef,
                        method = method)
        }
      })
    } else {
      lapply(suns, function(sun) {
        fit_cie_model(dist_to_black.rr.sun_angles$rr,
                      sun,
                      custom_sky_coef = custom_sky_coef,
                      method = method)})
    }

    metrics <- lapply(seq_along(models),
                      function(i) models[[i]]$metric) %>% unlist
    i <- which.min(metrics)

    models[[i]]
  }


  .find_best_dist_single <- function(r, z, a, min_spherical_dist,
                                     dist_to_black.rr.sun_angles,
                                     model_osun) {
    if (is.null(model_osun)) return(NULL)
    sky_points <- model_osun$rr$sky_points[, c("row", "col")]
    dist_to_black <- dist_to_black.rr.sun_angles$dist_to_black
    .get_rrs <- function(min_spherical_dist) {
      sky_points <- rem_nearby_points(sky_points, r, z, a, min_spherical_dist,
                                      use_window = !is.null(dist_to_black))
      extract_rr(r, z, a, sky_points, no_of_points = 3,
                           use_window = !is.null(dist_to_black))
    }
    rrs <- lapply(min_spherical_dist, .get_rrs)
    models <- lapply(rrs,
                     function(rr) {
                       fit_cie_model(rr, model_osun$sun_angles,
                                     custom_sky_coef = model_osun$coef,
                                     method = model_osun$method)
                     })
    metrics <- lapply(seq_along(models),
                      function(i) models[[i]]$metric) %>% unlist
    i <- which.min(metrics)
    model <- models[[i]]
    model$method_sun <-  model_osun$method_sun
    list(min_spherical_dist = min_spherical_dist[i],
         model = model)
  }

  .find_best_dist_multi <- function(row_vals, col_vals,
                                    r_vals, z_vals, a_vals,
                                    min_spherical_dist,
                                    dist_to_black.rr.sun_angles,
                                    model_osun) {
    # Rebuilt rasters
    r <- terra::rast(nrows = max(row_vals, na.rm = TRUE),
                     ncols = max(col_vals, na.rm = TRUE))
    terra::ext(r) <- terra::ext(0, ncol(r), 0, nrow(r))
    # https://spatialreference.org/ref/sr-org/7589/
    terra::crs(r) <- "epsg:7589"
    terra::values(r) <- r_vals

    a <- z <- rast(r)
    terra::values(z) <- z_vals
    names(z) <- "Zenith image"
    terra::values(a) <- a_vals
    names(a) <- "Azimuth image"

    .find_best_dist_single(r, z, a, min_spherical_dist,
                           dist_to_black.rr.sun_angles,
                           model_osun)
  }


  cv <- terra::focal(r, 3, sd) / terra::focal(r, 3, mean)

# Extract vectors ---------------------------------------------------------

    if (parallel) {
    row_vals <- terra::rowFromCell(r, terra::cells(r))
    col_vals <- terra::colFromCell(r, terra::cells(r))
    r_vals <- terra::values(r)
    z_vals <- terra::values(z)
    a_vals <- terra::values(a)
    m_vals <- terra::values(m)
    bin_vals <- terra::values(bin)
    cv_vals <- terra::values(cv)
  }

  sun_angles2 <- estimate_sun_angles(r, z, a, bin, NULL, NULL,
                                     method = "assume_veiled")


# Detect sky dn -----------------------------------------------------------

  if (is.null(custom_sky_coef)) {
    is_from_detected_sky_dn <- TRUE
    sky <- apply_by_direction(r, z, a, m,
                              spacing = 5, laxity = 2.5,
                              fov = c(30, 40),
                              method = "detect_bg_dn",
                              parallel = parallel)
    g <- sky_grid_segmentation(z, a, 22.5, first_ring_different = FALSE)
    sky_points <- extract_sky_points(sky$n, !is.na(sky$dn), g,
                                     dist_to_black = 1)

    if (nrow(sky_points) > ((unique(g[m]) %>% length()) * 0.75)) {
      rr <- extract_rr(sky$dn, z, a, sky_points,
                                 no_of_points = 3,
                                 use_window = FALSE)

      # Sun coordinate
      sun_angles <- estimate_sun_angles(r, z, a, bin, g,
                                        angular_radius_sun = 30,
                                        method = "assume_obscured")

      # Force the sun low and add that to the results
      civic_twilight <- c(seq(sun_angles["z"], 90, length = 5), seq(91, 96, 1))

      suns <- lapply(seq_along(civic_twilight),
                     function(i) c(z = civic_twilight[i], sun_angles["a"]))
      suns[[length(suns) + 1]] <- sun_angles2

      # Model
      if (parallel) {
        # Only to avoid note from check, code is OK without this line.
        sun <- NA

        .with_cluster(.cores(), {
          models <- foreach(sun = suns) %dopar% {
            fit_cie_model(rr, sun, method = method)
          }
        })
      } else {
        models <- lapply(suns, function(sun) fit_cie_model(rr, sun,
                                                          method = method))
      }

      metrics <- lapply(seq_along(models), function(i) models[[i]]$metric) %>%
        unlist
      i <- which.min(metrics)
      model_up <- models[[i]]

      if (is.null(model_up$opt_result$message)) {
        custom_sky_coef <- model_up$coef
      } else {
        custom_sky_coef <- NULL
      }
    } else {
      custom_sky_coef <- NULL
    }
  } else {
    is_from_detected_sky_dn <- FALSE
  }

  if (is.null(custom_sky_coef)) {
    is_from_detected_sky_dn <- FALSE
  }


# Get marks ---------------------------------------------------------------

  l.dist_to_black.rr.sun_angles <- if (parallel && length(gs) > 1) {
    .with_cluster(.cores(), {
      foreach(i = seq_along(gs)) %dopar% {
        .get_marks_multi(row_vals, col_vals,
                         r_vals, z_vals, a_vals,
                         m_vals, bin_vals, cv_vals,
                         attr(gs[[i]], "angle_width"),
                         attr(gs[[i]], "first_ring_different"))
      }
    })
  } else {
     Map(.get_marks_single, r, z, a, m, bin, cv,gs)
  }



# Fit ---------------------------------------------------------------------

  models <- tryCatch(Map(.fit,
                          l.dist_to_black.rr.sun_angles,
                          gs,
                          lapply(seq_along(gs), function(i) custom_sky_coef),
                          lapply(seq_along(gs), function(i) method),
                          lapply(seq_along(gs), function(i) parallel),
                          lapply(seq_along(gs), function(i) sun_angles2)),
                      error = function(e) NULL)

  if (lapply(models, function(x) is.null(x)) %>% unlist() %>% all()) {
    warning("Fit failed")
    return(NULL)
  }


# Optim sun ---------------------------------------------------------------

  l.model_osun <- if (parallel && length(gs) > 1) {
    .with_cluster(.cores(), {
        foreach(i = seq_along(gs)) %dopar% {
          optim_sun_angles(models[[i]], method)
        }
    })
  } else {
    Map(optim_sun_angles,
        models,
        lapply(seq_along(gs), function(i) method))
  }


# Try subsamples ----------------------------------------------------------

  l.min_spherical_dist.model <-if (any(min_spherical_dist != 0)) {
    if (parallel && length(gs) > 1) {
      .with_cluster(.cores(), {
        foreach(i = seq_along(gs)) %dopar% {
          .find_best_dist_multi(row_vals, col_vals,
                                r_vals, z_vals, a_vals,
                                min_spherical_dist,
                                l.dist_to_black.rr.sun_angles[[i]],
                                l.model_osun[[i]])
        }
      })
    } else {
      Map(.find_best_dist_single,
          r, z, a,
          min_spherical_dist,
          l.dist_to_black.rr.sun_angles,
          l.model_osun)
    }


  } else {
      Map(function(dist_to_black.rr.sun_angles, model_osun) {
                     list(min_spherical_dist = 0,
                          model = model_osun)
                    },
           l.dist_to_black.rr.sun_angles,
           l.model_osun)
  }


# Validate ----------------------------------------------------------------

  .validate <- function(i) {
    if (is.null(l.min_spherical_dist.model[[i]])) return(NULL)
    validate_cie_model(l.min_spherical_dist.model[[i]]$model, k = 10)

  }
  l.model_validation <- if (parallel && length(gs) > 1) {
    .with_cluster(.cores(), {
      foreach(i = seq_along(gs)) %dopar% .validate(i)
    })
  } else {
    lapply(seq_along(gs), .validate)
  }


# Prepare output ----------------------------------------------------------

  metrics <- lapply(seq_along(l.model_validation),
                    function(i) l.model_validation[[i]]$metric) %>% unlist
  i <- which.min(metrics)

  if (metrics[i] == 1e10) {
    warning("`ootb_sky_cie()` failed unexpectedly.")
    return(NULL)
  } else {
    custom_sky_coef <- models[[i]]$start
    attr(custom_sky_coef, "is_from_detected_sky_dn") <- is_from_detected_sky_dn
    .get_sky_cie <- function(z, a, model) {
      cie_image(z, a, model$sun_angles,  model$coef)
    }

    model <- l.min_spherical_dist.model[[i]]$model
    sun_row_col <- row_col_from_zenith_azimuth(z, a,
                                               model$sun_angles["z"],
                                               model$sun_angles["a"])

    list(rr_raster = .get_sky_cie(z, a, model),
         model = model,
         model_validation = l.model_validation[[i]],
         dist_to_black = l.dist_to_black.rr.sun_angles[[i]]$dist_to_black,
         use_window = !is.null(l.dist_to_black.rr.sun_angles[[i]]$dist_to_black),
         min_spherical_dist = l.min_spherical_dist.model[[i]]$min_spherical_dist,
         sky_points = model$rr$sky_points[, c("row", "col")],
         sun_row_col = sun_row_col,
         g = gs[[i]],
         tested_grids = lapply(gs, function(g) {
            paste(attr(g, "angle_width"), attr(g, "first_ring_different"))
          }) %>% unlist %>% paste0(., collapse = "; "),
         tested_distances = paste0(min_spherical_dist, collapse = "; "),
         tested_methods = paste0(method, collapse = "; "),
         optimal_start = custom_sky_coef,
         model_up = if (is_from_detected_sky_dn) model_up else NULL
         )
  }
}
