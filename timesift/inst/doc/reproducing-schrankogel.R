## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)
library(timesift)

## ----contract-----------------------------------------------------------------
# spe <- read.csv(file.path(deposit, "spe_wide.csv"), check.names = FALSE)
# rownames(spe) <- as.character(spe$logger_ID)
# counts <- colSums(spe[setdiff(names(spe), "logger_ID")])
# keep <- setdiff(names(counts)[counts >= 25],
#                 c("Alchemilla vulgaris agg.", "Taraxacum sp.", "Festuca halleri agg.",
#                   "Euphrasia sp.", "Phleum alpinum agg."))
# y <- as.matrix(spe[, keep, drop = FALSE])
# dim(y)
# #> [1] 894 101

## ----folds--------------------------------------------------------------------
# f <- read.csv(system.file("reproduce", "folds.csv", package = "timesift"))
# folds <- setNames(as.integer(f$fold), as.character(f$logger_ID))
# 
# cells <- scorable_cells(y, folds)
# sum(cells$scorable)
# #> [1] 1003

## ----own-folds----------------------------------------------------------------
# folds <- fold_map(y, v = 10, seed = 1, strata = 5)

## ----seasons------------------------------------------------------------------
# astronomical_seasons <- function(path) {
#   labels <- read.csv(path)
#   key <- paste(labels$season, format(as.Date(labels$day), "%Y"))
#   edges <- sort(as.POSIXct(paste0(labels$day[!duplicated(key)], " 00:00:00"), tz = "UTC"))
#   function(when) edges[findInterval(as.numeric(when), as.numeric(edges))]
# }
# 
# binning <- list(native = "native", halfday = "halfday", day = "day", week = "week",
#                 month = "month",
#                 season = astronomical_seasons(file.path(deposit, "seasons.csv")),
#                 year = "year")

## ----stats--------------------------------------------------------------------
# mean_reading <- grain_matrix(readings, logger_ID, date, temp, grain = "week")
# reported <- grain_matrix(readings, logger_ID, date, temp, grain = "week",
#                           stats = c("cold_day", "mean", "warm_day"))

## ----channels-----------------------------------------------------------------
# input <- bind_channels(reported, calendar_channels(reported))
# hourly <- grain_matrix(readings, logger_ID, date, temp, grain = "native")
# hourly_input <- bind_channels(hourly, calendar_channels(hourly, cycles = c("year", "day")))

## ----arms---------------------------------------------------------------------
# agg <- read.csv(file.path(deposit, "output_temperature_variables_scaled.csv"), check.names = FALSE)
# rownames(agg) <- as.character(agg$logger_ID)
# features <- feature_matrix(as.matrix(agg[rownames(y), setdiff(names(agg), "logger_ID")]),
#                            label = "aggregates")
# 
# elastic_net <- grain_ladder(
#   features, y, list(elastic_net = elasticnet(alpha = 0.5, n_inner = 5, squares = TRUE)),
#   folds = folds, metric = "tss")

## ----stepwise-----------------------------------------------------------------
# register_response("presence_absence_unweighted", list(
#   prepare = function(y) y, activation = "sigmoid", loss = "binary_cross_entropy",
#   metric = "roc_auc", cells = scorable_cells))
# stepwise_arm <- grain_ladder(
#   features, y, list(stepwise = stepwise(max_terms = 3, degree = 2)),
#   folds = folds, metric = "tss", response = "presence_absence_unweighted")

## ----encoders-----------------------------------------------------------------
# encoders <- list(mlp = mlp(batch_size = 64), cnn = cnn(batch_size = 32),
#                  rescnn = rescnn(dropout = 0.2, batch_size = 32))

## ----control------------------------------------------------------------------
# study <- train_control(val_frac = 0.15, early_stopping = 10L)

## ----ensemble-----------------------------------------------------------------
# members <- data.frame(
#   architecture = c(rep("cnn", 7), rep("rescnn", 4)),
#   window_mean = c("day", "day", "day", "day", "week", "week", "halfday", "day", "day", "day",
#                   "week"),
#   window_extremeday = c("week", "week", "week", "week", "month", "month", "week", "week", "week",
#                         "week", "month"),
#   kernel = c(7, 7, 7, 11, 7, 7, 7, 5, 7, 11, 7),
#   dropout = c(rep(0.3, 7), rep(0.2, 4)),
#   seed = c(1234, 11, 22, 33, 44, 66, 77, 88, 99, 101, 111))
# members$channels <- list(c(16, 32, 64, 128, 128), c(16, 32, 64, 128), c(32, 64, 128, 256),
#                          c(16, 32, 64, 128), c(16, 32, 64, 128), c(32, 64, 128, 256),
#                          c(16, 32, 64, 128), c(32, 64, 128, 256), c(32, 64, 128, 256),
#                          c(32, 64, 128, 256), c(32, 64, 128, 256))
# member <- function(i) {
#   arch <- if (members$architecture[i] == "cnn") cnn else rescnn
#   arch(channels = members$channels[[i]], kernel = members$kernel[i],
#        dropout = members$dropout[i], batch_size = 32, swa = TRUE, seed = members$seed[i])
# }

## ----grid---------------------------------------------------------------------
# ladder_input <- timesift_set(setNames(lapply(names(binning), function(w) {
#   x <- grain_matrix(readings, logger_ID, date, temp, grain = binning[[w]])
#   bind_channels(x, calendar_channels(x, cycles = if (w == "native") c("year", "day") else "year"))
# }), names(binning)))
# 
# grid <- grain_ladder(ladder_input, y, encoders, folds = folds, metric = "tss", control = study)
# summary(grid)

## ----ensemble-arm-------------------------------------------------------------
# oof <- lapply(seq_len(nrow(members)), function(i) {
#   w <- members$window_mean[i]
#   lad <- grain_ladder(ladder_input[w], y, list(m = member(i)), folds = folds, metric = "tss",
#                       control = study)
#   attr(lad, "predictions")[[1]]
# })
# stack <- ensemble_fit(oof, y, scorable_cells(y, folds), folds, spec = ensemble("mean"))
# combined <- ensemble_combine(stack, oof)

## ----candidates---------------------------------------------------------------
# summaries <- list(mean = "mean", min = "min", max = "max",
#                   minmeanmax = c("min", "mean", "max"),
#                   dailyextreme = c("mean_daily_min", "mean", "mean_daily_max"),
#                   extremeday = c("cold_day", "mean", "warm_day"))
# candidates <- timesift_set(unlist(lapply(names(summaries), function(s) {
#   windows <- if (s == "mean") names(binning)
#              else if (s %in% c("dailyextreme", "extremeday")) c("week", "month", "season", "year")
#              else setdiff(names(binning), "native")
#   setNames(lapply(windows, function(w) {
#     x <- grain_matrix(readings, logger_ID, date, temp, grain = binning[[w]], stats = summaries[[s]])
#     bind_channels(x, calendar_channels(x))
#   }), paste(windows, s, sep = "."))
# }), recursive = FALSE))
# length(candidates)
# #> [1] 33

## ----inner--------------------------------------------------------------------
# inner <- read.csv(system.file("reproduce", "inner_folds.csv", package = "timesift"))
# inner$logger_ID <- as.character(inner$logger_ID)
# 
# inner_split <- function(y_train) {
#   # The fit is handed its training response and nothing else, so which outer fold it sits in is
#   # read off the plots it does not hold.
#   left_out <- unique(folds[setdiff(names(folds), rownames(y_train))])
#   rows <- inner[inner$outer_fold == left_out, ]
#   setNames(as.integer(rows$inner_fold), rows$logger_ID)[rownames(y_train)]
# }
# 
# selection <- select_grain(candidates, y, cnn(epochs = 60, batch_size = 32),
#                           folds = folds, inner = inner_split, metric = "roc_auc", control = study)
# selection$selected

## ----series-------------------------------------------------------------------
# weekly <- grain_matrix(readings, logger_ID, date, temp, grain = "week",
#                         stats = c("cold_day", "mean", "warm_day"))
# series <- grain_ladder(timesift_set(list(series = weekly)), y,
#                        list(elastic_net = elasticnet(alpha = 0.5, n_inner = 5, squares = TRUE)),
#                        folds = folds, metric = "tss")

## ----environment--------------------------------------------------------------
# torch::torch_config()$libtorch_version
# torch::cuda_is_available()

## ----contrasts----------------------------------------------------------------
# grid_auc <- do.call(rbind, lapply(names(attr(grid, "predictions")), function(arm) {
#   at <- strsplit(arm, "|", fixed = TRUE)[[1]]
#   cbind(grain = at[1], learner = at[2],
#         score_predictions(y, attr(grid, "predictions")[[arm]], folds, scorable_cells(y, folds),
#                           "roc_auc"))
# }))
# grid_auc <- structure(grid_auc, class = c("timesift_ladder", "data.frame"), metric = "roc_auc")
# grain_contrasts(grid_auc, learner = "cnn", reference = "week")

## ----script-------------------------------------------------------------------
# system.file("reproduce", "schrankogel.R", package = "timesift")

