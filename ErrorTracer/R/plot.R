# R/plot.R — et_plot_*(): ggplot2 visualizations (all return ggplot objects)

# ******************************************************************************
# et_plot_decomposition(): stacked bar of variance components
# ______________________________________________________________________________

#' Plot uncertainty decomposition
#'
#' Produces a stacked bar chart showing the relative contributions of
#' parameter, environmental, and residual variance for each observation,
#' plus a fourth temporal-autocorrelation component when present in
#' \code{decomp} (see \code{\link{decompose_uncertainty}}).
#'
#' @param decomp A \code{data.frame} from \code{\link{decompose_uncertainty}}
#'   or directly the \code{$decomposition} slot of an \code{et_prediction}.
#' @param proportional Logical.  If \code{TRUE} (default), bars are scaled to
#'   sum to 1 (proportional contribution).  If \code{FALSE}, raw variances
#'   are shown.
#' @param group_col Character.  Optional name of a grouping column in
#'   \code{decomp} (present for grouped predictions).
#' @return A \code{ggplot2} object.
#' @export
et_plot_decomposition <- function(decomp, proportional = TRUE,
                                   group_col = NULL) {
  components <- c("param_var", "env_var", "residual_var")
  if ("temporal_var" %in% colnames(decomp)) {
    components <- c(components, "temporal_var")
  }
  long <- .pivot_decomp(decomp, components)

  x_col <- if (!is.null(group_col) && group_col %in% colnames(decomp)) {
    group_col
  } else {
    "obs_id"
  }

  fill_values <- c(
    "Parameter"     = "#2166AC",
    "Environmental" = "#4DAC26",
    "Residual"      = "#D1E5F0",
    "Temporal"      = "#B2182B"
  )

  p <- ggplot2::ggplot(long, ggplot2::aes(
    x    = .data[[x_col]],
    y    = .data[["value"]],
    fill = .data[["component"]]
  )) +
    ggplot2::geom_col(
      position = if (proportional) "fill" else "stack"
    ) +
    ggplot2::scale_fill_manual(values = fill_values, name = "Component") +
    ggplot2::labs(
      title = "Uncertainty Decomposition",
      x     = if (x_col == "obs_id") "Observation" else x_col,
      y     = if (proportional) "Proportion of variance" else "Variance"
    ) +
    et_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  p
}

# ******************************************************************************
# et_plot_shelf_life(): CI width vs time
# ______________________________________________________________________________

#' Plot forecast shelf life
#'
#' Shows how credible interval width grows over the forecast horizon and
#' marks the threshold beyond which the forecast is uninformative.
#'
#' @param sl An \code{et_shelf_life} object from \code{\link{shelf_life}}.
#' @param show_ratio Logical.  If \code{TRUE} (default), plots the
#'   ratio (CI width / plausible range) rather than raw CI width.
#' @return A \code{ggplot2} object.
#' @export
et_plot_shelf_life <- function(sl, show_ratio = TRUE) {
  y_col  <- if (show_ratio) "ratio"    else "ci_width"
  hline  <- if (show_ratio) 1.0        else unique(sl$plausible_range)[1]
  y_lab  <- if (show_ratio) "CI width / plausible range" else "CI width"
  hline_lab <- if (show_ratio) "Uninformative threshold (ratio = 1)"
               else "Plausible range"

  has_group <- "group" %in% colnames(sl)

  p <- ggplot2::ggplot(sl, ggplot2::aes(
    x     = .data[["time"]],
    y     = .data[[y_col]],
    color = if (has_group) .data[["group"]] else NULL,
    group = if (has_group) .data[["group"]] else NULL
  )) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_hline(yintercept = hline, linetype = "dashed", color = "grey40") +
    ggplot2::annotate("text", x = min(sl$time), y = hline * 1.04,
                      label = hline_lab, hjust = 0, color = "grey40", size = 3) +
    ggplot2::labs(
      title = "Forecast Shelf Life",
      x     = "Time",
      y     = y_lab,
      color = if (has_group) "Group" else NULL
    ) +
    et_theme()

  if (has_group) p <- p + ggplot2::scale_color_viridis_d()

  p
}

# ******************************************************************************
# et_plot_calibration(): observed vs nominal coverage
# ______________________________________________________________________________

#' Plot calibration: observed vs nominal coverage
#'
#' A well-calibrated model produces points along the 1:1 diagonal.
#' Points above the diagonal indicate over-coverage (conservative);
#' below indicates under-coverage (anti-conservative).
#'
#' @param cal A \code{data.frame} from \code{\link{et_calibrate}}.
#' @param group_col Optional character.  Name of a column in \code{cal} that
#'   identifies sub-groups (e.g.\ \code{"species"}, \code{"cluster_id"}).
#'   When \code{NULL} (default), \code{et_plot_calibration} uses the
#'   \code{group} column if present; otherwise it auto-detects any single
#'   non-canonical column with more than one unique value and treats it as
#'   the grouping.  Set to \code{NA} to force a single un-grouped series.
#' @return A \code{ggplot2} object.
#' @export
et_plot_calibration <- function(cal, group_col = NULL) {
  canonical <- c("ci_level", "nominal", "observed_coverage",
                 "n_obs", "calibration_error", "sharpness")

  if (is.null(group_col)) {
    if ("group" %in% colnames(cal)) {
      group_col <- "group"
    } else {
      extras <- setdiff(colnames(cal), canonical)
      extras <- extras[vapply(extras,
                              function(x) length(unique(cal[[x]])) > 1,
                              logical(1))]
      if (length(extras) == 1L) group_col <- extras
    }
  } else if (is.na(group_col)) {
    group_col <- NULL
  }

  has_group <- !is.null(group_col) && group_col %in% colnames(cal)

  p <- ggplot2::ggplot(cal, ggplot2::aes(
    x = .data[["nominal"]],
    y = .data[["observed_coverage"]]
  )) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed", color = "grey50") +
    ggplot2::labs(
      title = "Calibration: Observed vs Nominal Coverage",
      x     = "Nominal CI level",
      y     = "Observed coverage"
    ) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    et_theme()

  if (has_group) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(
        color = .data[[group_col]], group = .data[[group_col]]
      ), alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(color = .data[[group_col]]), size = 3) +
      ggplot2::scale_color_viridis_d(name = group_col)
  } else {
    p <- p +
      ggplot2::geom_line(alpha = 0.7) +
      ggplot2::geom_point(size = 3, color = "steelblue")
  }

  p
}

# ******************************************************************************
# et_plot_forecast(): fan chart of posterior predictive intervals
# ______________________________________________________________________________

#' Plot posterior predictive fan chart
#'
#' Overlays nested credible interval ribbons on the median forecast, with
#' optional observed values for calibration assessment.
#'
#' @param predictions An \code{et_prediction} object.
#' @param observed Optional \code{data.frame} with true response values.
#'   If provided, points are overlaid.
#' @param response_col Character.  Name of the response column in
#'   \code{observed}.
#' @param time_col Character.  Column in \code{predictions$newdata} used
#'   as the x-axis.  Defaults to observation index.
#' @return A \code{ggplot2} object.
#' @export
et_plot_forecast <- function(predictions, observed = NULL,
                              response_col = NULL, time_col = NULL) {
  ci_df <- predictions$credible_intervals

  time_vals <- if (!is.null(time_col) &&
                   time_col %in% colnames(predictions$newdata)) {
    rep(predictions$newdata[[time_col]],
        times = length(unique(ci_df$ci_level)))
  } else {
    ci_df$row_id
  }
  ci_df$time <- time_vals

  p <- ggplot2::ggplot(ci_df, ggplot2::aes(x = .data[["time"]])) +
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin  = .data[["lower"]],
      ymax  = .data[["upper"]],
      alpha = .data[["ci_level"]],
      group = .data[["ci_level"]]
    ), fill = "steelblue") +
    ggplot2::geom_line(
      data = ci_df[ci_df$ci_level == max(ci_df$ci_level), ],
      ggplot2::aes(y = .data[["median"]]),
      color = "darkblue", linewidth = 0.9
    ) +
    ggplot2::scale_alpha_continuous(range = c(0.12, 0.45), guide = "none") +
    ggplot2::labs(
      title = "Forecast Fan Chart",
      x     = if (!is.null(time_col)) time_col else "Observation",
      y     = "Response"
    ) +
    et_theme()

  if (!is.null(observed) && !is.null(response_col)) {
    obs_time <- if (!is.null(time_col) &&
                    time_col %in% colnames(observed)) {
      observed[[time_col]]
    } else {
      seq_len(nrow(observed))
    }
    obs_df <- data.frame(
      time     = obs_time,
      response = observed[[response_col]],
      stringsAsFactors = FALSE
    )
    p <- p + ggplot2::geom_point(
      data = obs_df,
      ggplot2::aes(x = .data[["time"]], y = .data[["response"]]),
      color = "firebrick", size = 2.5, inherit.aes = FALSE
    )
  }

  p
}

# ******************************************************************************
# et_plot_prior_posterior(): density overlay
# ______________________________________________________________________________

#' Plot prior vs posterior distributions for model coefficients
#'
#' Overlays prior and posterior density for each predictor coefficient,
#' visualising how much the data update the priors.
#'
#' @param model An \code{et_model} object.
#' @param max_preds Integer.  Maximum number of predictors to show
#'   (default 8).  Predictors are shown in the order they appear in the
#'   prior specification.
#' @param n_prior_draws Integer.  Number of random draws for the prior
#'   density (default 4000).
#' @return A \code{ggplot2} object.
#' @export
et_plot_prior_posterior <- function(model, max_preds = 8L,
                                     n_prior_draws = 4000L) {
  if (!inherits(model, "et_model")) {
    stop("model must be an et_model object.")
  }
  if (is.null(model$prior_spec)) {
    stop("No et_prior_spec attached to model. ",
         "Prior/posterior plot requires a model fitted via et_fit() with ",
         "priors from extract_priors().")
  }

  fit     <- model$fit
  spec    <- model$prior_spec
  preds   <- head(spec$pred_names, max_preds)

  post_draws <- as.data.frame(fit)
  beta_cols  <- paste0("b_", preds)
  avail      <- intersect(beta_cols, colnames(post_draws))
  if (length(avail) == 0) stop("No beta columns found in posterior draws.")

  post_long <- do.call(rbind, lapply(avail, function(bc) {
    pname <- sub("^b_", "", bc)
    data.frame(
      parameter    = pname,
      value        = post_draws[[bc]],
      distribution = "Posterior",
      stringsAsFactors = FALSE
    )
  }))

  prior_long <- do.call(rbind, lapply(avail, function(bc) {
    pname <- sub("^b_", "", bc)
    idx   <- match(pname, spec$pred_names)
    mu    <- if (!is.null(spec$coefs)) spec$coefs[idx] else 0
    sd    <- max(spec$multiplier * abs(mu), spec$min_sd)
    data.frame(
      parameter    = pname,
      value        = stats::rnorm(n_prior_draws, mu, sd),
      distribution = "Prior",
      stringsAsFactors = FALSE
    )
  }))

  combined <- rbind(post_long, prior_long)
  combined$distribution <- factor(combined$distribution, levels = c("Prior", "Posterior"))

  ggplot2::ggplot(combined,
                  ggplot2::aes(x = .data[["value"]], fill = .data[["distribution"]])) +
    ggplot2::geom_density(alpha = 0.45) +
    ggplot2::facet_wrap(~ .data[["parameter"]], scales = "free") +
    ggplot2::scale_fill_manual(
      values = c("Prior" = "grey60", "Posterior" = "steelblue"),
      name   = NULL
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "grey30") +
    ggplot2::labs(
      title = "Prior vs Posterior: Regression Coefficients",
      x     = "Value",
      y     = "Density"
    ) +
    et_theme()
}

# ******************************************************************************
# et_plot_coefficients(): forest plot, Bayesian vs regularized
# ______________________________________________________________________________

#' Forest plot of regression coefficients
#'
#' Compares Bayesian posterior estimates (95\% CI) with the regularized
#' coefficient values used as prior means (shown as crosses).
#'
#' @param model An \code{et_model} or \code{et_model_list}.
#' @return A \code{ggplot2} object.
#' @export
et_plot_coefficients <- function(model) {
  UseMethod("et_plot_coefficients")
}

#' @export
et_plot_coefficients.et_model <- function(model) {
  coef_df <- .extract_coef_df(model, group_label = NULL)
  .forest_plot(coef_df, facet_by_group = FALSE)
}

#' @export
et_plot_coefficients.et_model_list <- function(model) {
  parts <- lapply(names(model$models), function(g) {
    m <- model$models[[g]]
    if (is.null(m)) return(NULL)
    .extract_coef_df(m, group_label = g)
  })
  coef_df <- do.call(rbind, Filter(Negate(is.null), parts))
  .forest_plot(coef_df, facet_by_group = TRUE)
}

.extract_coef_df <- function(model, group_label) {
  fit  <- model$fit
  spec <- model$prior_spec
  post <- brms::fixef(fit)
  fe   <- rownames(post)
  fe   <- fe[fe != "Intercept"]

  rows <- lapply(fe, function(pname) {
    enet_val <- if (!is.null(spec) && !is.null(spec$coefs) &&
                    pname %in% names(spec$coefs)) {
      spec$coefs[pname]
    } else {
      NA_real_
    }
    df <- data.frame(
      predictor  = pname,
      post_mean  = post[pname, "Estimate"],
      post_lower = post[pname, "Q2.5"],
      post_upper = post[pname, "Q97.5"],
      enet_coef  = enet_val,
      stringsAsFactors = FALSE
    )
    if (!is.null(group_label)) df$group <- group_label
    df
  })
  do.call(rbind, rows)
}

.forest_plot <- function(coef_df, facet_by_group) {
  p <- ggplot2::ggplot(coef_df,
    ggplot2::aes(y = .data[["predictor"]])) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        x    = .data[["post_mean"]],
        xmin = .data[["post_lower"]],
        xmax = .data[["post_upper"]]
      ),
      color = "steelblue", linewidth = 0.6
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::labs(
      title = "Coefficient Estimates: Posterior (blue) vs Regularized (red \u00d7)",
      x     = "Coefficient",
      y     = NULL
    ) +
    et_theme()

  # Add enet estimates if present
  has_enet <- any(!is.na(coef_df$enet_coef))
  if (has_enet) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = .data[["enet_coef"]]),
      color = "firebrick", shape = 4, size = 3, na.rm = TRUE
    )
  }

  if (facet_by_group && "group" %in% colnames(coef_df)) {
    p <- p + ggplot2::facet_wrap(~ .data[["group"]], scales = "free")
  }

  p
}

# ******************************************************************************
# et_plot_sensitivity(): noise fraction vs horizon / env_share
# ______________________________________________________________________________

#' Plot a sensitivity profile
#'
#' Visualises the output of \code{\link{et_sensitivity_profile}}: for each
#' noise grid point, shows how the \strong{environmental share} of total
#' variance and the \strong{forecast horizon} respond.  Observed horizons
#' are drawn as solid points; projected horizons are hollow points;
#' lower-bound rows are drawn as upward arrows at the last informative time.
#'
#' The x-axis is the noise fraction (when the grid was built from
#' \code{fraction_grid}) or the grid step label otherwise.  When both a
#' numeric fraction and a descriptive label exist, the fraction is preferred
#' for continuous x-positioning.
#'
#' @param sens An \code{et_sensitivity} object from
#'   \code{\link{et_sensitivity_profile}}.
#' @param show \code{"horizon"} (default) shows the shelf-life horizon;
#'   \code{"env_share"} shows \code{env_var / total_var}; \code{"ratio"}
#'   shows the mean CI width / plausible range.
#' @return A \code{ggplot2} object.
#' @seealso \code{\link{et_sensitivity_profile}}
#' @export
et_plot_sensitivity <- function(sens, show = c("horizon", "env_share", "ratio")) {
  if (!inherits(sens, "et_sensitivity")) {
    stop("et_plot_sensitivity() expects an et_sensitivity object.")
  }
  show <- match.arg(show)

  df <- as.data.frame(sens)
  x_is_numeric <- !all(is.na(df$fraction))
  df$x <- if (x_is_numeric) df$fraction else seq_len(nrow(df))
  x_lab <- if (x_is_numeric) "Noise fraction of predictor SD" else "Grid step"

  if (show == "horizon") {
    df$y <- df$horizon
    df$point_shape <- factor(
      ifelse(df$horizon_type == "observed", "observed",
      ifelse(df$horizon_type == "projected", "projected", "lower bound")),
      levels = c("observed", "projected", "lower bound")
    )
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
      ggplot2::geom_line(na.rm = TRUE, colour = "#2166AC") +
      ggplot2::geom_point(ggplot2::aes(shape = .data[["point_shape"]]),
                          size = 3, na.rm = TRUE) +
      ggplot2::scale_shape_manual(
        values = c("observed" = 16, "projected" = 21, "lower bound" = 2),
        drop = FALSE, name = "Horizon type"
      ) +
      ggplot2::labs(x = x_lab, y = "Forecast horizon",
                    title = "Shelf life vs. assumed measurement noise")
  } else if (show == "env_share") {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]],
                                          y = .data[["env_share"]])) +
      ggplot2::geom_line(colour = "#4DAC26") +
      ggplot2::geom_point(colour = "#4DAC26", size = 3) +
      ggplot2::scale_y_continuous(labels = scales_percent_safe,
                                  limits = c(0, NA)) +
      ggplot2::labs(x = x_lab,
                    y = "Environmental share of total variance",
                    title = "How much of the forecast variance is environmental?")
  } else {
    thr <- attr(sens, "threshold")
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]],
                                          y = .data[["ratio_mean"]])) +
      ggplot2::geom_line(colour = "#B2182B") +
      ggplot2::geom_point(colour = "#B2182B", size = 3) +
      ggplot2::geom_hline(yintercept = if (is.null(thr)) 1 else thr,
                          linetype = "dashed") +
      ggplot2::labs(x = x_lab,
                    y = "Mean CI width / plausible range",
                    title = "Forecast informativeness vs. noise")
  }

  p + et_theme()
}

# Permissive wrapper for scales::percent — fall back to identity if the
# scales package isn't installed so the plot still renders.
scales_percent_safe <- function(x) {
  if (requireNamespace("scales", quietly = TRUE)) scales::percent(x)
  else formatC(100 * x, format = "f", digits = 0, flag = "#")
}

# ******************************************************************************
# Internal helpers
# ______________________________________________________________________________

# Pivot variance decomposition to long format for ggplot stacking
.pivot_decomp <- function(decomp, cols) {
  rows <- lapply(cols, function(col) {
    label <- switch(col,
      param_var    = "Parameter",
      env_var      = "Environmental",
      residual_var = "Residual",
      temporal_var = "Temporal",
      col
    )
    df <- decomp[, c(setdiff(colnames(decomp), cols), col), drop = FALSE]
    df$component <- label
    df$value     <- decomp[[col]]
    df[, c(setdiff(colnames(decomp), cols), "component", "value")]
  })
  do.call(rbind, rows)
}
