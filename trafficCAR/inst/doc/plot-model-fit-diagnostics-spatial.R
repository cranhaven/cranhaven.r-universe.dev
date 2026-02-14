## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(trafficCAR)
library(sf)
library(ggplot2)

data("roads_small", package = "trafficCAR")
roads <- roads_small

segments <- roads_to_segments(
  roads,
  crs_m = 3857,
  split_at_intersections = TRUE
)

# Keep the example lightweight for vignette builds.
if (nrow(segments) > 200) {
  segments <- segments[seq_len(200), ]
}

adjacency <- build_adjacency(segments, crs_m = 3857)

# Drop isolated segments to keep the example compatible with a proper CAR model.
if (any(adjacency$isolates)) {
  segments <- segments[!adjacency$isolates, ]
  adjacency <- build_adjacency(segments, crs_m = 3857)
}

## -----------------------------------------------------------------------------
set.seed(123)

segment_length <- segments$length_m
segment_length <- scale(segment_length)[, 1]

speed <- 40 + 6 * segment_length + rnorm(nrow(segments), sd = 3)

traffic_data <- data.frame(
  segment_id = segments$seg_id,
  speed = speed
)

X <- cbind(
  intercept = 1,
  length = segment_length
)

fit <- fit_car(
  y = traffic_data$speed,
  A = adjacency$A,
  X = X,
  type = "proper",
  rho = 0.9,
  tau = 1,
  n_iter = 300,
  burn_in = 150,
  thin = 2
)

## -----------------------------------------------------------------------------
make_plot_fit <- function(base_fit, X, outcome_col, outcome_label) {
  x_draws <- base_fit$draws$x
  beta_draws <- base_fit$draws$beta

  if (is.null(beta_draws) || ncol(beta_draws) == 0) {
    mu_draws <- x_draws
  } else {
    mu_draws <- beta_draws %*% t(X) + x_draws
  }

  plot_fit <- list(
    draws = list(
      mu = mu_draws,
      x = x_draws,
      beta = beta_draws,
      sigma2 = base_fit$draws$sigma2
    ),
    outcome_col = outcome_col,
    outcome_label = outcome_label
  )

  class(plot_fit) <- "traffic_fit"
  plot_fit
}

plot_fit <- make_plot_fit(
  fit,
  X = X,
  outcome_col = "speed",
  outcome_label = "Speed (mph)"
)

## ----fig.width=7, fig.height=5------------------------------------------------
plot_observed_fitted(plot_fit, data = traffic_data)

## ----fig.width=7, fig.height=4------------------------------------------------
diag <- plot_mcmc_diagnostics(plot_fit)

if (is.list(diag) && !is.null(diag$plot)) {
  diag$plot
} else {
  ess <- vapply(plot_fit$draws, posterior::ess_basic, numeric(1))
  ess_df <- data.frame(
    parameter = names(ess),
    ess = as.numeric(ess),
    row.names = NULL
  )

  ggplot2::ggplot(ess_df, ggplot2::aes(parameter, ess)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Effective sample size by parameter",
      x = NULL,
      y = "ESS"
    )
}

## -----------------------------------------------------------------------------
if (is.list(diag) && !is.null(diag$summary)) {
  head(diag$summary)
} else {
  ess <- vapply(plot_fit$draws, posterior::ess_basic, numeric(1))
  head(data.frame(parameter = names(ess), ess = as.numeric(ess), row.names = NULL))
}

## ----fig.width=7, fig.height=6------------------------------------------------
plot_predicted(plot_fit, segments)
plot_relative_congestion(plot_fit, segments)

