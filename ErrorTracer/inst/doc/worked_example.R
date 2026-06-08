## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  eval      = FALSE,   # Stan/brms chunks are too slow for automated builds
  fig.width = 7,
  fig.height = 4.5
)

## -----------------------------------------------------------------------------
# library(ErrorTracer)
# library(ggplot2)
# library(patchwork)

## -----------------------------------------------------------------------------
# set.seed(20260523L)
# n_train       <- 12L      # 12 training years
# true_beta     <- 4.0      # days of phenology shift per SD of temperature
# true_sigma    <- 1.5      # residual SD (days)
# true_alpha    <- 0.0      # intercept
# warming_slope <- 0.15     # SD of standardised temperature per year
# year0         <- 2000L    # year at which the trend is referenced to zero
# 
# years_train <- year0 + seq_len(n_train) - 1L  # 2000..2011
# train <- data.frame(
#   year = years_train,
#   x    = warming_slope * (years_train - year0) +
#          rnorm(n_train, 0, 0.5)               # noisy observed temperature
# )
# train$y <- true_alpha + true_beta * train$x +
#            rnorm(n_train, 0, true_sigma)

## -----------------------------------------------------------------------------
# lm_fit  <- lm(y ~ x, data = train)
# summary(lm_fit)$coefficients
# priors  <- extract_priors(lm_fit, multiplier = 2.0, min_sd = 0.1)
# priors

## -----------------------------------------------------------------------------
# bfit <- et_fit(
#   formula = y ~ x,
#   data    = train,
#   priors  = priors,
#   chains  = 4L, iter = 2000L, warmup = 1000L,
#   cores   = 4L, seed = 1L, silent = 2L, refresh = 0
# )
# print(bfit)

## -----------------------------------------------------------------------------
# beta_draws <- as.matrix(bfit$fit, variable = "b_x")[, 1]
# c(mean = mean(beta_draws), sd = sd(beta_draws))

## -----------------------------------------------------------------------------
# et_plot_prior_posterior(bfit)

## -----------------------------------------------------------------------------
# year_min     <- year0
# year_max     <- year0 + 45L
# years_grid   <- seq(year_min, year_max, by = 1L)
# forecast_df  <- data.frame(
#   year = years_grid,
#   x    = warming_slope * (years_grid - year0)
# )
# 
# pred <- et_predict(
#   model             = bfit,
#   newdata           = forecast_df,
#   env_noise         = list(x = 0.3),
#   n_draws           = 2000L,
#   n_perturb         = 500L,
#   ci_levels         = 0.90,
#   include_env_in_ci = TRUE
# )

## -----------------------------------------------------------------------------
# decomp <- decompose_uncertainty(pred)
# decomp$year <- forecast_df$year
# head(decomp)

## -----------------------------------------------------------------------------
# mid <- which(decomp$year == 2020L)
# decomp[mid, ]

## -----------------------------------------------------------------------------
# sl <- shelf_life(
#   predictions    = pred,
#   response_scale = c(-4, 4),
#   ci_level       = 0.90,
#   threshold      = 1.0,
#   time_col       = "year"
# )
# attr(sl, "horizon")

## ----eval = FALSE-------------------------------------------------------------
# # Reproduce the four-panel Box 1 figure used in the methods paper.
# source(system.file("extdata", "box1_worked_example.R",
#                    package = "ErrorTracer"))
# run_box1_worked_example(figures_dir = tempdir(),
#                         tex_path    = tempfile(fileext = ".tex"))

## ----eval = TRUE--------------------------------------------------------------
sessionInfo()

