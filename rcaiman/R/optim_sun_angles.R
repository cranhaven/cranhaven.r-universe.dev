.optim_sun_angles <- function(sun_angles, rr, sky_coef, method) {
  .normalize_angles <- function(zenith, azimuth) {
    if (zenith < 0) {
      zenith <- -zenith        # Make zenith positive
      azimuth <- azimuth + 180 # Rotate azimuth 180°
    }

    # Ensure azimuth is in [0, 360)
    azimuth <- azimuth %% 360

    angles <- c(zenith, azimuth)
    names(angles) <- c("z", "a")
    angles
  }
  .refine_sun_coord <- function(param) {
    zenith <- param[1] * 9
    azimuth <- param[2] * 36

    chi <- calc_spherical_distance(zenith %>% .degree2radian(),
                                   azimuth %>% .degree2radian(),
                                   sun_angles["z"] %>% .degree2radian(),
                                   sun_angles["a"] %>% .degree2radian())


    pred <- .cie_sky_model(AzP = rr$sky_points$a %>% .degree2radian(),
                           Zp = rr$sky_points$z %>% .degree2radian(),
                           AzS = azimuth %>% .degree2radian(),
                           Zs =  zenith %>% .degree2radian(),
                           sky_coef[1], sky_coef[2], sky_coef[3],
                           sky_coef[4], sky_coef[5])

    (.calc_rmse(pred - rr$sky_points$rr) / mean(rr$sky_points$rr)) *
      max(1, .radian2degree(chi)/10)
  }
  fit <- tryCatch(stats::optim(c(sun_angles["z"]/9,
                                 sun_angles["a"]/36),
                               .refine_sun_coord,
                               method = method),
                  error = function(e) NULL)

  tryCatch(.normalize_angles(fit$par[1]*9, fit$par[2]*36),
           error = function(e) NULL)
}


#' Optimize sun angular coordinates
#'
#' @description
#' Refine the solar position in a fitted CIE sky model by optimizing zenith and
#' azimuth to best match observed relative radiance.
#'
#' @details
#' Evaluates one or more methods from [stats::optim()] starting at
#' `model$sun_angles`. After each optimization the model is re‑fitted and the
#' process repeats until the change in solar position is < 1 deg. The best
#' result across methods is kept.
#'
#' @param model list returned by [fit_cie_model()].
#' @param method character vector. One or more optimization methods supported by
#'   [stats::optim()]. Each is applied independently.
#'
#' @return List like `model`, potentially with updated `sun_angles` and
#'   `metric`, and a new `method_sun` indicating the best optimization method.
#'   If no improvement is found, `method_sun` is `NULL`.
#'
#' @note
#' The objective function penalizes solutions that move the sun position by
#' more than 10 deg from the initial estimate to discourage unrealistic shifts.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#'
#' # See fit_cie_model() for details on below file
#' path <- system.file("external/sky_points.csv",
#'                     package = "rcaiman")
#' sky_points <- read.csv(path)
#' sky_points <- sky_points[c("Y", "X")]
#' colnames(sky_points) <- c("row", "col")
#' sun_angles <- c(z = 39.5, a = 17.4)
#'
#' rr <- extract_rr(caim$Blue, z, a, sky_points)
#'
#' set.seed(7)
#' model <- fit_cie_model(rr, sun_angles, general_sky_type = "Clear")
#'
#' print(model$sun_angles)
#' print(model$metric)
#'
#' plot(model$rr$sky_points$rr, model$rr$sky_points$pred)
#' abline(0,1)
#' lm(model$rr$sky_points$pred~model$rr$sky_points$rr) %>% summary()
#'
#' model <- optim_sun_angles(model)
#' print(model$sun_angles)
#' print(model$metric)
#' model$method_sun
#' }
optim_sun_angles <- function(model,
                             method = c("Nelder-Mead", "BFGS", "CG", "SANN")) {
  if (!is.list(model)) {
    stop("`model` must be a list returned by `fit_cie_model()`. ")
  }
  required_names <- c("rr", "opt_result", "coef", "sun_angles",
                      "method", "start" , "metric")
  if (!all(required_names %in% names(model))) {
    stop(sprintf("`model` must contain %s.",
                 paste(sprintf('"%s"', required_names), collapse = ", ")))
  }

  repeat {
    pre_optim_sun <- model$sun_angles
    suns <- lapply(method, function(x) .optim_sun_angles(model$sun_angles,
                                                         model$rr,
                                                         model$coef,
                                                         method = x))
    j <- lapply(suns, function(x) !is.null(x)) %>% unlist()
    method_sun <- NULL
    if (any(j)) {
      method_subset <- method[j]
      suns <- lapply(seq_along(method_subset), function(i) suns[[i]])
      models <- lapply(suns, function(sun_angles) fit_cie_model(
                                                   model$rr, sun_angles,
                                                   custom_sky_coef = model$coef,
                                                   method = model$method)
                       )
      metrics <- lapply(seq_along(models),
                        function(i) models[[i]]$metric) %>% unlist
      i <- which.min(metrics)
      method_sun <- method_subset[i]
      model <- models[[i]]
    }
    delta_chi <- calc_spherical_distance(pre_optim_sun["z"],
                                         pre_optim_sun["a"],
                                         model$sun_angles["z"],
                                         model$sun_angles["a"])
    if (delta_chi > pi/180) break
  }
  model$method_sun <- method_sun
  model
}
