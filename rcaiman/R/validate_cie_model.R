#' Validate CIE sky models
#'
#' @description
#' Validate CIE sky models fitted with [fit_cie_model()] (or [ootb_sky_cie()])
#' using k-fold cross-validation on relative radiance.
#'
#' @details
#' Validation uses k-fold cross-validation with `k = 10` by default
#' \insertCite{Kohavi1995}{rcaiman}. For each fold, predictions are
#' compared against observed relative radiance and a simple linear regression
#' of predicted vs. observed is fitted, following
#' \insertCite{Pineiro2008;textual}{rcaiman}. Outliers are detected with a
#' median–MAD rule (see [rem_outliers()]) using a threshold of 3
#' and removed before fitting the regression.
#'
#' @param model list. Output of [fit_cie_model()].
#' @param k numeric vector of length one. Number of folds.
#'
#' @return A list with:
#' \describe{
#'   \item{lm}{An object of class `lm` (see [stats::lm()]) for predicted vs. observed.}
#'   \item{pred}{Numeric vector of predicted relative radiance used in `lm`.}
#'   \item{obs}{Numeric vector of observed relative radiance used in `lm`.}
#'   \item{r_squared}{Coefficient of determination (\eqn{R^2}).}
#'   \item{rmse}{Root mean squared error (RMSE).}
#'   \item{mae}{Median absolute error (MAE).}
#'   \item{is_outlier}{Logical vector marking outliers (MAD > 3) in the original sky-point set.}
#'   \item{metric}{Numeric value. Mean squared deviation as in \insertCite{Gauch2003;textual}{rcaiman}.}
#' }
#'
#' @references \insertAllCited{}
#' @export
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#' path <- system.file("external/sky_points.csv", package = "rcaiman")
#' sky_points <- read.csv(path)[c("Y", "X")]
#' names(sky_points) <- c("row", "col")
#' rr <- extract_rr(caim$Blue, z, a, sky_points)
#'
#' set.seed(7)
#' model <- fit_cie_model(rr, sun_angles = c(z = 49.5, a = 27.4),
#'                        general_sky_type = "Clear", method = "CG")
#' val <- validate_cie_model(model, k = 10)
#' val$r_squared
#' val$rmse
#' }
validate_cie_model <- function(model, k = 10) {
  if (!is.list(model)) {
    stop("`model` must be a list returned by `fit_cie_model()`. ")
  }
  required_names <- c("rr", "opt_result", "coef", "sun_angles",
                      "method", "start" , "metric")
  if (!all(required_names %in% names(model))) {
    stop(sprintf("`model` must contain %s.",
                 paste(sprintf('"%s"', required_names), collapse = ", ")))
  }
  .check_vector(k, "integerish", 1, sign = "positive")

  .get_metric <- function(x, y) {
    #10.2134/agronj2003.1442
    reg <- tryCatch(lm(x~y),
                    error = function(e) NULL,
                    warning = function(w) NULL)
    if (is.null(reg)) {
      MSE <- 1e10
    } else {
      m <- stats::coef(reg)[2]
      r_squared <- summary(reg) %>% .$r.squared
      SB <- (mean(x) - mean(y))^2
      NU <- (1 - m)^2 * mean(x^2)
      LC <- (1 - r_squared) * mean(y^2)
      MSE <- SB + NU + LC
    }
  }

  .noise <- function(w = 1) {
    coef_sd <- apply((rcaiman::cie_table[, 1:5]), 2, sd) * w
    Map(function(i) stats::rnorm(1, 0, coef_sd[i]), 1:5) %>% unlist()
  }

  # k=10 based on https://dl.acm.org/doi/10.5555/1643031.1643047
  folds <- seq_along(model$rr$sky_points$row)
  folds <- split(folds, 1:k) %>% suppressWarnings()
  x <- c()
  y <- c()
  for (i in 1:k) {
    rr.2 <- model$rr
    rr.2$sky_points <- model$rr$sky_points[-folds[[i]],]
    model.2 <- fit_cie_model(rr.2,  model$sun_angles,
                                 custom_sky_coef = model$coef + .noise(0.1),
                                 method = model$method)

    x <- c(x, .cie_sky_model(AzP = model$rr$sky_points[folds[[i]], "a"] %>%
                               .degree2radian(),
                             Zp = model$rr$sky_points[folds[[i]], "z"] %>%
                               .degree2radian(),
                             AzS = model$sun_angles["a"] %>%
                               .degree2radian(),
                             Zs =  model$sun_angles["z"] %>%
                               .degree2radian(),
                             model$coef[1], model$coef[2], model$coef[3],
                             model$coef[4], model$coef[5]))
    y <- c(y, model$rr$sky_points[folds[[i]], "rr"])
  }

  # Following Leys2013 10.1016/j.jesp.2013.03.013
  error <- y - x
  u <- abs((error - stats::median(error)) / stats::mad(error)) < 3
  x <- x[u]
  y <- y[u]

  reg <- lm(x~y) #following Pineiro2008 10.1016/j.ecolmodel.2008.05.006

  list(lm = reg,
       pred = reg$model$x,
       obs = reg$model$y,
       r_squared = summary(reg) %>% .$r.squared,
       rmse = .calc_rmse(reg$model$y - reg$model$x),
       mae = stats::median(abs(reg$model$y - reg$model$x)),
       is_outlier = !u,
       metric = .get_metric(reg$model$x, reg$model$y) %>% unname()
       )
}
