## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6.5, fig.height = 4.5,
                      dev.args = list(pointsize = 14))
set.seed(1)
library(timesift)

## ----data---------------------------------------------------------------------
hours <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 120)
ids <- sprintf("p%03d", 1:60)
warmth <- rnorm(60)

series <- data.frame(
  plot = rep(ids, each = length(hours)),
  t = rep(hours, times = 60),
  temp = as.numeric(vapply(warmth, function(w) {
    1.5 * w + 6 * sin(seq_along(hours) / (24 * 40)) + rnorm(length(hours), sd = 12)
  }, numeric(length(hours))))
)

sign <- rep(c(1, -1), length.out = 6)
targets <- data.frame(plot = ids, elevation = 2000 + 300 * rnorm(60))
targets[paste0("sp", 1:6)] <- lapply(sign, function(s) rbinom(60, 1, plogis(3 * s * warmth)))
str(targets[1:4], give.attr = FALSE)

## ----fit----------------------------------------------------------------------
fit <- timesift(
  targets, series,
  y = starts_with("sp"),
  id = plot,
  time = t,
  static = elevation,
  sift = grains("day", "week", "month"),
  resampling = cv(v = 5),
  verbose = FALSE
)
fit

## ----estimate-----------------------------------------------------------------
fit$estimate[c("arm", "metric", "score", "se", "lower", "upper")]
fit$selected[c("fold", "candidate", "inner_score")]

## ----candidates---------------------------------------------------------------
fit$candidates[c("candidate", "representation", "bins", "channels", "status")]

## ----plot-run, fig.alt = "Mean AUC against representation, with the level the combination reached drawn across it."----
plot(fit)

## ----predict------------------------------------------------------------------
p <- predict(fit, targets, series)
round(p[1:3, 1:4], 3)

## ----representations----------------------------------------------------------
native()
grain("week", stats = c("cold_day", "mean", "warm_day"))
multigrain(c("week", "month"))
lookback("30 days", bins = 3)

## ----pinned-------------------------------------------------------------------
elasticnet(data = grain("month"))

## ----compatibility, error = TRUE----------------------------------------------
try({
timesift(targets, series, y = starts_with("sp"), id = plot, time = t,
         models = elasticnet(), sift = native())
})

## ----folds--------------------------------------------------------------------
fit$folds

## ----cells--------------------------------------------------------------------
fit$cells

## ----control------------------------------------------------------------------
train_control(epochs = 200L, device = "cpu")
cnn(channels = c(16L, 32L), epochs = 300L)

## ----custom-------------------------------------------------------------------
flat <- function(x) matrix(as.numeric(x), nrow = dim(x)[1])

nearest_neighbour <- learner(
  "1nn",
  fit = function(x, y, ...) list(x = flat(x), y = y),
  predict = function(model, x) {
    d <- as.matrix(dist(rbind(flat(x), model$x)))[seq_len(dim(x)[1]), -seq_len(dim(x)[1])]
    model$y[apply(d, 1, which.min), , drop = FALSE]
  }
)

both <- timesift(targets, series, y = starts_with("sp"), id = plot, time = t,
                 models = c(elasticnet(), nearest_neighbour),
                 sift = grains("week", "month"), resampling = cv(v = 5), verbose = FALSE)
summary(both)

## ----weights------------------------------------------------------------------
ensemble_weights(fit)

## ----inflation----------------------------------------------------------------
tss_inflation(fit$y, fit$folds, skill = c(0.6, 0.9), replicates = 100)

## ----representation-----------------------------------------------------------
x <- grain_matrix(series, plot, t, temp, grain = "week",
                  stats = c("cold_day", "mean", "warm_day"))
x

## ----calendar-----------------------------------------------------------------
attr(grain_matrix(series, plot, t, temp, grain = "month"), "bin_n")[1, ]

## ----statistics---------------------------------------------------------------
week <- grain_matrix(series, plot, t, temp, grain = "week",
                     stats = c("min", "mean_daily_min", "cold_day", "mean",
                               "warm_day", "mean_daily_max", "max"))
round(week[1, 1, ], 2)

## ----channels-----------------------------------------------------------------
week_mean <- grain_matrix(series, plot, t, temp, grain = "week")
dimnames(bind_channels(week_mean, calendar_channels(week_mean)))[[3]]

## ----ladder-------------------------------------------------------------------
set <- grain_matrix(series, plot, t, temp, grain = c("day", "week", "month"))
lad <- grain_ladder(set, fit$y, elasticnet(), folds = fit$folds, verbose = FALSE)
summary(lad)

## ----contrast-----------------------------------------------------------------
paired_contrast(lad, "month|elasticnet", "day|elasticnet")

## ----grain-contrasts, eval = all(vapply(c("lme4", "lmerTest", "emmeans"), requireNamespace, logical(1), quietly = TRUE))----
grain_contrasts(lad)

## ----occlusion----------------------------------------------------------------
kept <- timesift(targets, series, y = starts_with("sp"), id = plot, time = t,
                 sift = grains("month"), resampling = cv(v = 5), ensemble = FALSE,
                 keep_fits = TRUE, verbose = FALSE)
weight <- occlusion(kept, "elasticnet / month", permutations = 5)
head(aggregate(weight ~ part, weight, mean), 4)

