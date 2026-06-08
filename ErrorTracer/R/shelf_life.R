# R/shelf_life.R -- shelf_life(): forecast horizon / shelf life analysis

#' Compute the forecast shelf life
#'
#' Quantifies \emph{when} a forecast becomes uninformative by comparing the
#' width of credible intervals to a response scale.  A forecast is
#' uninformative when its CI width exceeds \code{threshold * response_scale}.
#'
#' The function operates in three modes depending on the available data and
#' whether the uninformative threshold is crossed within the forecast window:
#'
#' \describe{
#'   \item{\strong{Observed}}{The threshold is crossed within the
#'     forecast/validation window.  The shelf life is the first time point
#'     at which \code{ratio >= threshold}.}
#'   \item{\strong{Projected}}{All forecast periods remain informative but
#'     the CI/range ratio is trending upward.  A linear trend is fitted to
#'     the ratios and extrapolated to estimate when the threshold would be
#'     reached.  The projected crossing time \eqn{t^* = (\tau - a) / b}
#'     (where \eqn{\tau} is the threshold, \eqn{a} the fitted intercept,
#'     \eqn{b} the fitted slope) is reported together with a Monte Carlo
#'     standard error \code{se_t_star} derived via the delta method.}
#'   \item{\strong{Lower bound}}{All forecast periods are informative with
#'     no upward trend in the ratio.  The shelf life is reported as a lower
#'     bound: \code{> last observed time}.}
#' }
#'
#' The intended workflow is:
#' \enumerate{
#'   \item Fit the model (\code{\link{et_fit}}).
#'   \item Predict on held-out data or a future time window
#'     (\code{\link{et_predict}}).
#'   \item Call \code{shelf_life()} on those predictions.
#'   \item If the threshold is not crossed in the held-out window, the
#'     \emph{projected} mode extrapolates the horizon automatically.
#' }
#'
#' @param predictions An \code{et_prediction} or \code{et_prediction_list}.
#' @param response_scale Numeric vector of length 2 (\code{c(min, max)})
#'   giving the response scale used as the denominator in the
#'   CI-width / range ratio.  For unbounded responses use
#'   \code{range(training_data$response)} as a conservative default, or
#'   supply a biologically motivated interval.
#'   The effective range is \code{diff(response_scale)}.
#' @param ci_level Numeric.  The credible interval level to use (default
#'   0.90).  Must be present in the \code{et_prediction} object.
#' @param threshold Numeric.  CI width / response scale above which the
#'   forecast is uninformative (default 1.0).
#' @param time_col Character.  Optional column in
#'   \code{predictions$newdata} to use as the time axis.  If \code{NULL},
#'   observation index is used.
#' @param min_slope_for_projection Numeric.  Minimum linear slope (of
#'   ratio vs.\ time) required to attempt extrapolation when all periods
#'   are informative.  Below this value the shelf life is reported as a
#'   lower bound only.  Default \code{1e-4}.
#' @param max_extrapolation_factor Numeric.  Cap on how far the linear
#'   projection may reach beyond the observed window.  If the projected
#'   crossing time exceeds
#'   \code{max(time) + max_extrapolation_factor * (max(time) - min(time))},
#'   the result is reported as a lower bound instead of a projection.
#'   Set to \code{Inf} to disable the cap.  Default \code{10}.
#' @param ... Unused.
#' @param plausible_range Deprecated.  Use \code{response_scale} instead.
#'
#' @return An \code{et_shelf_life} object (a \code{data.frame}) with columns:
#'   \describe{
#'     \item{obs_id}{Observation index.}
#'     \item{time}{Time axis value.}
#'     \item{ci_width}{Width of the credible interval at \code{ci_level}.}
#'     \item{plausible_range}{Effective response scale (scalar diff).}
#'     \item{ratio}{CI width / response scale.}
#'     \item{informative}{Logical; \code{TRUE} when ratio < threshold.}
#'   }
#'   For grouped predictions a \code{group} column is prepended.
#'   The object carries three attributes that drive \code{print()}:
#'   \describe{
#'     \item{\code{horizon}}{Named list with elements \code{value},
#'       \code{type} (\code{"observed"}, \code{"projected"}, or
#'       \code{"lower_bound"}), \code{last_informative},
#'       \code{description}, and (for projected) \code{se_t_star}.}
#'     \item{\code{horizon_by_group}}{For grouped objects: named list of
#'       per-group horizon lists.}
#'     \item{\code{threshold}}{The threshold value used.}
#'   }
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' df  <- data.frame(y = rnorm(20), year = 2001:2020, x1 = rnorm(20))
#' fit <- et_fit(y ~ x1, data = df,
#'               chains = 1, iter = 500, warmup = 250,
#'               cores = 1, refresh = 0)
#' new_df <- data.frame(x1 = rnorm(10), year = 2021:2030)
#' pred   <- et_predict(fit, newdata = new_df,
#'                      n_draws = 200, n_perturb = 50)
#' sl <- shelf_life(pred,
#'                  response_scale            = range(df$y),
#'                  ci_level                  = 0.90,
#'                  threshold                 = 1.0,
#'                  time_col                  = "year",
#'                  max_extrapolation_factor  = 10)
#' print(sl)
#' }
#' @export
shelf_life <- function(predictions,
                       response_scale,
                       ci_level = 0.90,
                       threshold = 1.0,
                       time_col = NULL,
                       min_slope_for_projection = 1e-4,
                       max_extrapolation_factor = 10,
                       ...,
                       plausible_range = NULL) {
  # Deprecated alias: plausible_range -> response_scale
  if (!is.null(plausible_range)) {
    if (!missing(response_scale)) {
      stop("Cannot specify both 'response_scale' and 'plausible_range'.")
    }
    warning(
      "'plausible_range' is deprecated; use 'response_scale' instead.",
      call. = FALSE
    )
    return(shelf_life(
      predictions              = predictions,
      response_scale           = plausible_range,
      ci_level                 = ci_level,
      threshold                = threshold,
      time_col                 = time_col,
      min_slope_for_projection = min_slope_for_projection,
      max_extrapolation_factor = max_extrapolation_factor,
      ...
    ))
  }
  UseMethod("shelf_life")
}

#' @export
shelf_life.et_prediction <- function(predictions,
                                     response_scale,
                                     ci_level = 0.90,
                                     threshold = 1.0,
                                     time_col = NULL,
                                     min_slope_for_projection = 1e-4,
                                     max_extrapolation_factor = 10,
                                     ...,
                                     plausible_range = NULL) {
  .compute_shelf_life_single(
    predictions = predictions,
    response_scale = response_scale,
    ci_level = ci_level,
    threshold = threshold,
    time_col = time_col,
    min_slope_for_projection = min_slope_for_projection,
    max_extrapolation_factor = max_extrapolation_factor
  )
}

#' @export
shelf_life.et_prediction_list <- function(predictions,
                                          response_scale,
                                          ci_level = 0.90,
                                          threshold = 1.0,
                                          time_col = NULL,
                                          min_slope_for_projection = 1e-4,
                                          max_extrapolation_factor = 10,
                                          ...,
                                          plausible_range = NULL) {
  horizon_by_group <- list()

  parts <- lapply(names(predictions$predictions), function(g) {
    pred <- predictions$predictions[[g]]
    if (is.null(pred)) return(NULL)

    sl <- .compute_shelf_life_single(
      pred, response_scale, ci_level, threshold,
      time_col, min_slope_for_projection, max_extrapolation_factor
    )
    horizon_by_group[[g]] <<- attr(sl, "horizon")
    cbind(data.frame(group = g, stringsAsFactors = FALSE), sl)
  })

  result <- do.call(rbind, Filter(Negate(is.null), parts))
  result <- structure(result, class = c("et_shelf_life", "data.frame"))
  attr(result, "horizon_by_group") <- horizon_by_group
  attr(result, "threshold") <- threshold
  result
}

#' @export
shelf_life.default <- function(predictions, ...) {
  stop("shelf_life() expects an et_prediction or et_prediction_list object.")
}

# ******************************************************************************
# Internal: compute shelf life for a single et_prediction
# ______________________________________________________________________________

.compute_shelf_life_single <- function(predictions, response_scale,
                                        ci_level, threshold, time_col,
                                        min_slope_for_projection,
                                        max_extrapolation_factor = 10) {
  ci_df <- predictions$credible_intervals
  avail_levels <- unique(ci_df$ci_level)

  if (!ci_level %in% avail_levels) {
    stop(
      "ci_level ", ci_level, " not found in predictions. ",
      "Available: ", paste(avail_levels, collapse = ", "),
      ". Re-run et_predict() with the desired level in ci_levels."
    )
  }

  ci_sub <- ci_df[ci_df$ci_level == ci_level, ]

  if (length(response_scale) != 2) {
    stop("response_scale must be a numeric vector of length 2: c(min, max).")
  }
  pr <- abs(diff(response_scale))
  if (pr < 1e-12) stop("response_scale min and max are equal.")

  time_vals <- if (!is.null(time_col) &&
                   time_col %in% colnames(predictions$newdata)) {
    predictions$newdata[[time_col]]
  } else {
    ci_sub$row_id
  }

  result_df <- data.frame(
    obs_id = ci_sub$row_id,
    time = time_vals,
    ci_width = ci_sub$width,
    plausible_range = pr,
    ratio = ci_sub$width / pr,
    informative = ci_sub$width < threshold * pr,
    stringsAsFactors = FALSE
  )

  # Compute shelf life horizon
  horizon <- .compute_horizon(result_df, threshold, min_slope_for_projection,
                              max_extrapolation_factor)

  result <- structure(result_df, class = c("et_shelf_life", "data.frame"))
  attr(result, "horizon") <- horizon
  attr(result, "threshold") <- threshold
  result
}

# ******************************************************************************
# Internal: derive horizon from a single-group shelf-life table
# ______________________________________________________________________________

.compute_horizon <- function(sl_df, threshold, min_slope,
                             max_extrapolation_factor = 10) {
  times <- sl_df$time
  ratios <- sl_df$ratio
  informative <- sl_df$informative

  # --- Mode 1: threshold already crossed ---
  if (any(!informative)) {
    first_cross  <- min(times[!informative])
    last_ok      <- if (any(informative)) max(times[informative]) else NA_real_
    return(list(
      value = first_cross,
      type = "observed",
      last_informative = last_ok,
      description = paste0(
        "Threshold (ratio >= ", threshold, ") first exceeded at ",
        first_cross, "."
      )
    ))
  }

  # --- Mode 2: all informative -- try linear extrapolation ---
  last_ok <- max(times)

  if (is.numeric(times) && length(times) >= 3) {
    lm_fit <- suppressWarnings(lm(ratios ~ times))
    slope <- coef(lm_fit)[["times"]]
    b0 <- coef(lm_fit)[["(Intercept)"]]

    if (!is.na(slope) && slope > min_slope) {
      proj <- (threshold - b0) / slope

      # SE of t* via delta method: t* = (tau - a) / b
      # d(t*)/d(a) = -1/b,  d(t*)/d(b) = -(tau - a)/b^2 = -t*/b
      vc   <- stats::vcov(lm_fit)
      va   <- vc["(Intercept)", "(Intercept)"]
      vb   <- vc["times", "times"]
      cab  <- vc["(Intercept)", "times"]
      se_t_star <- sqrt(max(0, (va + proj^2 * vb + 2 * proj * cab) / slope^2))

      # Cap extrapolation: if projection exceeds last_ok + factor * window,
      # the trend is too flat to give a meaningful estimate -- report lower bound.
      window   <- max(times) - min(times)
      cap_time <- max(times) + max_extrapolation_factor * window

      if (is.finite(max_extrapolation_factor) && proj > cap_time) {
        return(list(
          value = NA_real_,
          type = "lower_bound",
          last_informative = last_ok,
          description = paste0(
            "All ", nrow(sl_df), " forecast periods informative. ",
            "Linear trend (slope = ", round(slope, 5), " per time unit) ",
            "projects threshold crossing at ~", round(proj, 1),
            ", but this exceeds the extrapolation cap (",
            round(cap_time, 1), "). Shelf life > ", last_ok, "."
          )
        ))
      }

      return(list(
        value = proj,
        type = "projected",
        last_informative = last_ok,
        se_t_star = se_t_star,
        slope = slope,
        description = paste0(
          "All ", nrow(sl_df), " forecast periods informative. ",
          "Linear trend (slope = ", round(slope, 5), " per time unit) ",
          "projects threshold crossing at ~", round(proj, 1),
          " (SE = ", round(se_t_star, 2), ")."
        )
      ))
    }
  }

  # --- Mode 3: no upward trend -- lower bound only ---
  list(
    value = NA_real_,
    type = "lower_bound",
    last_informative = last_ok,
    description = paste0(
      "All ", nrow(sl_df), " forecast periods informative with no ",
      "upward trend in CI/range ratio. Shelf life > ", last_ok, "."
    )
  )
}

# ******************************************************************************
# Internal: format a horizon for printing
# ______________________________________________________________________________

.format_horizon <- function(hor) {
  if (is.null(hor)) return(NULL)
  switch(hor$type,
    observed  = paste0("~", hor$value,
                         " (observed -- threshold first exceeded)"),
    projected = paste0("~", round(hor$value, 1),
                         " (projected -- extrapolated beyond forecast window)"),
    lower_bound = paste0("> ", hor$last_informative,
                         " (lower bound -- all periods informative, no trend)")
  )
}

# ******************************************************************************
# S3 methods for et_shelf_life
# ______________________________________________________________________________

#' @export
print.et_shelf_life <- function(x, ...) {
  thr <- attr(x, "threshold")
  cat("ErrorTracer shelf life analysis\n")
  cat("  Observations     :", nrow(x), "\n")
  cat("  Plausible range  :", unique(x$plausible_range), "\n")
  cat("  Threshold        :", if (is.null(thr)) 1.0 else thr, "\n")

  if ("group" %in% colnames(x)) {
    groups  <- unique(x$group)
    hor_grp <- attr(x, "horizon_by_group")

    for (g in groups) {
      sub <- x[x$group == g, ]
      pct <- round(100 * mean(sub$informative), 1)
      hor <- if (!is.null(hor_grp)) hor_grp[[g]] else NULL
      hor_str <- if (!is.null(hor)) .format_horizon(hor) else ""
      cat(sprintf(
        "  [%s]  %.1f%% informative  mean ratio = %.3f  shelf life %s\n",
        g, pct, mean(sub$ratio), hor_str
      ))
    }
  } else {
    cat("  Informative      :", sum(x$informative), "/", nrow(x), "\n")
    cat("  Mean CI/range    :", round(mean(x$ratio), 3), "\n")
    cat("  Max CI/range     :", round(max(x$ratio), 3), "\n")

    hor <- attr(x, "horizon")
    if (!is.null(hor)) {
      cat("  Shelf life       :", .format_horizon(hor), "\n")
      cat("  Detail           :", hor$description, "\n")
    }
  }
  invisible(x)
}

#' @export
summary.et_shelf_life <- function(object, ...) {
  print(object)
  cat("\n--- Full table ---\n")
  print(as.data.frame(object))
  invisible(object)
}
