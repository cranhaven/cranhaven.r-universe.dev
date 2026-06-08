# R/sensitivity.R -- et_sensitivity_profile(): noise-level sensitivity of
# decomposition and shelf life.

#' Sensitivity profile of the environmental uncertainty component
#'
#' Sweeps a grid of noise magnitudes, re-runs \code{\link{et_predict}} for
#' each, and summarises how the environmental variance component and the
#' forecast shelf life respond.  This turns the subjective choice of
#' \code{env_noise} into an auditable \emph{curve}: instead of reporting a
#' single shelf life at one (often hand-picked) measurement-error level, the
#' user sees how the horizon degrades as assumed noise grows.
#'
#' Three argument styles are supported for the grid:
#' \itemize{
#'   \item \code{fraction_grid}: scalar fractions of each predictor's SD in
#'     \code{newdata}.  These are passed straight to the scalar form of
#'     \code{\link{et_predict}}'s \code{env_noise}.  Use this for an apples-
#'     to-apples sweep that scales all predictors together.
#'   \item \code{absolute_grid}: a \code{list} of named numeric lists /
#'     vectors; each list element becomes a single \code{env_noise} argument
#'     (so you can sweep absolute SDs for one predictor while holding others
#'     fixed).
#'   \item Neither supplied (\code{NULL}): a default log-spaced fraction grid
#'     \code{c(0, 0.01, 0.025, 0.05, 0.1, 0.2, 0.4, 0.8)} is used.
#' }
#'
#' Each iteration calls \code{et_predict(..., env_noise = g)} then
#' \code{\link{shelf_life}} and records:
#' \itemize{
#'   \item Mean parameter / environmental / residual / total variance across
#'     forecast observations.
#'   \item The horizon description (\code{"observed"}, \code{"projected"}, or
#'     \code{"lower_bound"}) and numeric horizon if any.
#'   \item Mean / max CI width / plausible-range ratio.
#' }
#'
#' @param model An \code{et_model} from \code{\link{et_fit}}.
#' @param newdata \code{data.frame} passed to \code{et_predict}.
#' @param response_scale Two-element numeric vector \code{c(min, max)}, as in
#'   \code{\link{shelf_life}}.  The effective range is
#'   \code{diff(response_scale)}.
#' @param plausible_range Deprecated.  Use \code{response_scale} instead.
#' @param fraction_grid Optional numeric vector of scalar noise fractions.
#' @param absolute_grid Optional \code{list} of \code{env_noise} arguments
#'   (each a named list / vector).  If supplied together with
#'   \code{fraction_grid}, \code{fraction_grid} is ignored.
#' @param env_cov,env_dist Passed through to \code{et_predict}.
#' @param include_env_in_ci Logical.  If \code{TRUE} (default), credible
#'   intervals — and hence the shelf-life horizon — are computed from
#'   environmentally inflated draws (\code{et_predict(..., include_env_in_ci
#'   = TRUE)}).  Set \code{FALSE} to see how \code{env_var} evolves while
#'   holding the CI fixed at parameter + residual.
#' @param ci_level Numeric.  Used for shelf life and CI width tracking
#'   (default 0.90).  Must be in \code{ci_levels}.
#' @param ci_levels Numeric vector of CI levels computed per iteration
#'   (default \code{c(0.5, 0.8, 0.9, 0.95)}).
#' @param threshold Numeric.  Shelf-life threshold (default 1.0).
#' @param time_col Character.  Optional time column for shelf life.
#' @param n_draws,n_perturb Passed to \code{et_predict}.
#' @param max_extrapolation_factor Passed to \code{shelf_life}.
#' @param verbose Logical.  If \code{TRUE} (default), logs each iteration.
#'
#' @return An \code{et_sensitivity} object: a \code{data.frame} with one row
#'   per grid point and columns
#'   \describe{
#'     \item{\code{grid_id}}{1-indexed step.}
#'     \item{\code{label}}{Short descriptor of the env_noise used
#'       (e.g.\ \code{"fraction = 0.05"}).}
#'     \item{\code{fraction}}{Scalar fraction when applicable; \code{NA}
#'       for absolute-grid rows.}
#'     \item{\code{param_var, env_var, residual_var, total_var}}{Mean across
#'       forecast observations.}
#'     \item{\code{env_share}}{\code{env_var / (env_var + total_var)} ---
#'       the fraction of combined predictive variance attributable to
#'       predictor noise (bounded in \eqn{[0, 1]}).}
#'     \item{\code{ci_width_mean, ci_width_max}}{At \code{ci_level}.}
#'     \item{\code{ratio_mean, ratio_max}}{CI width / plausible range.}
#'     \item{\code{horizon_type}}{One of \code{"observed"}, \code{"projected"},
#'       \code{"lower_bound"}.}
#'     \item{\code{horizon}}{Numeric horizon (observed or projected) or
#'       \code{NA} for lower-bound rows.}
#'     \item{\code{horizon_description}}{The long description returned by
#'       \code{shelf_life}.}
#'   }
#'   The returned object carries the original call and \code{response_scale}
#'   as attributes for use by \code{\link{et_plot_sensitivity}}.
#'
#' @seealso \code{\link{et_predict}}, \code{\link{shelf_life}},
#'   \code{\link{et_plot_sensitivity}}
#' @export
et_sensitivity_profile <- function(model,
                                   newdata,
                                   response_scale,
                                   fraction_grid = NULL,
                                   absolute_grid = NULL,
                                   env_cov = NULL,
                                   env_dist = NULL,
                                   include_env_in_ci = TRUE,
                                   ci_level = 0.90,
                                   ci_levels = c(0.5, 0.8, 0.9, 0.95),
                                   threshold = 1.0,
                                   time_col = NULL,
                                   n_draws = 2000L,
                                   n_perturb = NULL,
                                   max_extrapolation_factor = 10,
                                   verbose = TRUE,
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
    response_scale <- plausible_range
  }

  if (!inherits(model, "et_model")) {
    stop("et_sensitivity_profile() expects an et_model object. ",
         "For grouped models, iterate over model$models yourself.")
  }
  if (!ci_level %in% ci_levels) {
    ci_levels <- sort(unique(c(ci_levels, ci_level)))
  }

  # Build grid (list of env_noise args and matching labels/fractions).
  grid <- .build_sensitivity_grid(fraction_grid, absolute_grid)

  rows <- vector("list", length(grid$noise))
  for (i in seq_along(grid$noise)) {
    if (verbose) {
      .et_info("Sensitivity step ", i, "/", length(grid$noise), ": ",
               grid$label[[i]])
    }

    pred <- et_predict(
      model             = model,
      newdata           = newdata,
      env_noise         = grid$noise[[i]],
      env_cov           = env_cov,
      env_dist          = env_dist,
      include_env_in_ci = include_env_in_ci,
      n_draws           = n_draws,
      ci_levels         = ci_levels,
      n_perturb         = n_perturb
    )
    sl <- shelf_life(
      predictions              = pred,
      response_scale           = response_scale,
      ci_level                 = ci_level,
      threshold                = threshold,
      time_col                 = time_col,
      max_extrapolation_factor = max_extrapolation_factor
    )
    hor <- attr(sl, "horizon")

    rows[[i]] <- data.frame(
      grid_id             = i,
      label               = grid$label[[i]],
      fraction            = grid$fraction[[i]],
      param_var           = mean(pred$decomposition$param_var),
      env_var             = mean(pred$decomposition$env_var),
      residual_var        = mean(pred$decomposition$residual_var),
      total_var           = mean(pred$decomposition$total_var),
      env_share           = mean(pred$decomposition$env_var /
                                   pmax(pred$decomposition$env_var +
                                        pred$decomposition$total_var, 1e-12)),
      ci_width_mean       = mean(sl$ci_width),
      ci_width_max        = max(sl$ci_width),
      ratio_mean          = mean(sl$ratio),
      ratio_max           = max(sl$ratio),
      horizon_type        = if (is.null(hor)) NA_character_ else hor$type,
      horizon             = if (is.null(hor) || is.na(hor$value))
                              NA_real_
                            else as.numeric(hor$value),
      horizon_description = if (is.null(hor)) NA_character_
                            else hor$description,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  out <- structure(out, class = c("et_sensitivity", "data.frame"))
  attr(out, "response_scale") <- response_scale
  attr(out, "ci_level")       <- ci_level
  attr(out, "threshold")      <- threshold
  out
}

# ******************************************************************************
# Internal: build noise-argument grid from user input
# ______________________________________________________________________________

.build_sensitivity_grid <- function(fraction_grid, absolute_grid) {
  if (!is.null(absolute_grid)) {
    if (!is.list(absolute_grid)) {
      stop("absolute_grid must be a list of env_noise arguments.")
    }
    labels <- vapply(seq_along(absolute_grid), function(i) {
      g <- absolute_grid[[i]]
      lab <- names(absolute_grid)[i]
      if (!is.null(lab) && nzchar(lab)) return(lab)
      if (is.list(g) || (is.numeric(g) && !is.null(names(g)))) {
        paste(paste(names(g), signif(unlist(g), 3), sep = "="), collapse = ", ")
      } else if (is.numeric(g) && length(g) == 1L) {
        paste0("fraction = ", signif(g, 3))
      } else {
        paste0("step_", i)
      }
    }, character(1))
    fractions <- vapply(absolute_grid, function(g) {
      if (is.numeric(g) && length(g) == 1L && is.null(names(g))) g
      else NA_real_
    }, numeric(1))
    return(list(noise = absolute_grid, label = labels, fraction = fractions))
  }

  if (is.null(fraction_grid)) {
    fraction_grid <- c(0, 0.01, 0.025, 0.05, 0.1, 0.2, 0.4, 0.8)
  }
  if (!is.numeric(fraction_grid) || any(fraction_grid < 0)) {
    stop("fraction_grid must be a numeric vector of non-negative values.")
  }

  list(
    noise    = as.list(fraction_grid),
    label    = paste0("fraction = ", signif(fraction_grid, 3)),
    fraction = fraction_grid
  )
}

# ******************************************************************************
# S3 methods
# ______________________________________________________________________________

#' @export
print.et_sensitivity <- function(x, ...) {
  cat("ErrorTracer sensitivity profile (et_sensitivity)\n")
  cat("  Grid steps        :", nrow(x), "\n")
  cat("  CI level          :", attr(x, "ci_level"), "\n")
  cat("  Response scale    :", diff(attr(x, "response_scale")), "\n")
  cat("  Threshold         :", attr(x, "threshold"), "\n")
  tab <- as.data.frame(x)[, c("label", "env_var", "env_share", "ratio_mean",
                              "horizon_type", "horizon"), drop = FALSE]
  print(tab, row.names = FALSE)
  invisible(x)
}
