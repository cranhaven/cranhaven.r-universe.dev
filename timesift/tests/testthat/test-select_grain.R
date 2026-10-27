# A candidate grid whose grains differ in what they let a learner see, so the selector has a real
# choice to get right or wrong.
selection_learner <- function(offset = 0) {
  learner(
    "linear",
    fit = function(x, y, ...) {
      m <- apply(x[, , 1, drop = FALSE], 1L, mean)
      list(coef = vapply(seq_len(ncol(y)), function(j) {
        stats::coef(stats::glm(y[, j] ~ m, family = stats::binomial()))
      }, numeric(2L)), offset = offset)
    },
    predict = function(model, x) {
      m <- apply(x[, , 1, drop = FALSE], 1L, mean)
      p <- vapply(seq_len(ncol(model$coef)), function(j) {
        stats::plogis(model$coef[1L, j] + model$coef[2L, j] * m + model$offset)
      }, numeric(length(m)))
      matrix(p, nrow = length(m))
    }
  )
}

selection_fixture <- function(v = 4L) {
  sim <- sim_series(n_unit = 56L, days = 90L, seed = 31L)
  y <- sim_response(sim, n_var = 4L, seed = 32L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = c("day", "week", "month"))
  list(x = x, y = y, folds = fold_map(y, v = v, seed = 6L))
}

test_that("a selection reports one winner per outer fold from the candidate set it searched", {
  f <- selection_fixture()
  sel <- suppressWarnings(select_grain(f$x, f$y, selection_learner(), folds = f$folds, inner = 3L,
                                       verbose = FALSE))
  expect_s3_class(sel, "timesift_selection")
  expect_equal(nrow(sel$selected), 4L)
  expect_setequal(sel$selected$fold, sort(unique(unclass(f$folds))))
  expect_equal(nrow(sel$candidates), 3L)
  expect_true(all(paste(sel$selected$grain, sel$selected$learner) %in%
                    paste(sel$candidates$grain, sel$candidates$learner)))
})

test_that("the estimate is reported under every registered metric on one set of predictions", {
  f <- selection_fixture()
  sel <- suppressWarnings(select_grain(f$x, f$y, selection_learner(), folds = f$folds, inner = 3L,
                                       verbose = FALSE))
  expect_setequal(sel$estimate$metric, metrics())
  expect_true(all(is.finite(sel$estimate$score)))
  expect_true(all(sel$estimate$n_variable <= ncol(f$y)))
  # The selection metric's estimate is the mean of the same per-cell scores the object carries.
  own <- sel$estimate$score[sel$estimate$metric == attr(sel, "metric")]
  cells <- sel$scores[!is.na(sel$scores$score), , drop = FALSE]
  expect_equal(own, mean(tapply(cells$score, cells$variable, mean)))
})

test_that("no outer test unit reaches the selector or the refit of its own fold", {
  seen <- new.env(parent = emptyenv())
  seen$fitted <- list()
  spy <- learner(
    "spy",
    fit = function(x, y, ...) {
      seen$fitted[[length(seen$fitted) + 1L]] <- dimnames(x)[[1L]]
      list(rate = colMeans(y))
    },
    predict = function(model, x) {
      m <- apply(x[, , 1, drop = FALSE], 1L, mean)
      outer(rank(m) / length(m), model$rate, function(a, b) stats::plogis(a - 0.5 + b))
    }
  )
  f <- selection_fixture()
  sel <- suppressWarnings(select_grain(f$x, f$y, spy, folds = f$folds, inner = 3L, verbose = FALSE))
  outer_fold <- stats::setNames(as.integer(f$folds), names(f$folds))
  # Every unit a model was fitted on during outer fold k, across the inner ladder and the refit,
  # must have come from outside fold k.
  per_outer <- length(seen$fitted) / nrow(sel$selected)
  expect_equal(per_outer %% 1, 0)
  for (i in seq_len(nrow(sel$selected))) {
    k <- sel$selected$fold[i]
    held <- names(outer_fold)[outer_fold == k]
    block <- seen$fitted[seq_len(per_outer) + (i - 1L) * per_outer]
    for (fitted_on in block) {
      expect_length(intersect(fitted_on, held), 0L)
    }
  }
})

test_that("the summary counts how often each candidate won and print stays terse", {
  f <- selection_fixture()
  sel <- suppressWarnings(select_grain(f$x, f$y, selection_learner(), folds = f$folds, inner = 3L,
                                       verbose = FALSE))
  s <- summary(sel)
  expect_equal(nrow(s), nrow(sel$candidates))
  expect_named(s, c("grain", "learner", "n_selected", "share", "inner_score"))
  expect_equal(sum(s$n_selected), nrow(sel$selected))
  expect_equal(sum(s$share), 1)
  expect_output(print(sel), "timesift selection")
})

test_that("the plot draws the inner scores and returns them", {
  f <- selection_fixture()
  sel <- suppressWarnings(select_grain(f$x, f$y, selection_learner(), folds = f$folds, inner = 3L,
                                       verbose = FALSE))
  path <- tempfile(fileext = ".png")
  grDevices::png(path)
  drawn <- plot(sel)
  grDevices::dev.off()
  expect_true(file.exists(path))
  expect_equal(nrow(drawn), nrow(sel$candidates) * nrow(sel$selected))
  expect_named(drawn, c("fold", "grain", "learner", "score", "se", "n_variable", "at",
                        "selected"))
  # Every inner score sits on its candidate's place on the axis, and each fold circles the one
  # candidate it selected.
  expect_false(anyNA(drawn$at))
  expect_equal(drawn$at, match(paste(drawn$grain, drawn$learner),
                               paste(sel$candidates$grain, sel$candidates$learner)))
  expect_equal(as.vector(tapply(drawn$selected, drawn$fold, sum)), rep(1L, nrow(sel$selected)))
  circled <- drawn[drawn$selected, ]
  expect_equal(paste(circled$fold, circled$grain, circled$learner),
               paste(sel$selected$fold, sel$selected$grain, sel$selected$learner))
  unlink(path)
})

test_that("the contrast against a ladder runs through paired_contrast on matched cells", {
  f <- selection_fixture()
  lad <- suppressWarnings(grain_ladder(f$x, f$y, selection_learner(), folds = f$folds,
                                        verbose = FALSE))
  sel <- suppressWarnings(select_grain(f$x, f$y, selection_learner(), folds = f$folds, inner = 3L,
                                       compare = lad, verbose = FALSE))
  expect_equal(nrow(sel$contrast), 3L)
  expect_true(all(sel$contrast$a == "selected|selected"))
  expect_setequal(sel$contrast$b, paste(lad$grain, lad$learner, sep = "|"))
  expect_true(all(sel$contrast$lower <= sel$contrast$diff &
                    sel$contrast$diff <= sel$contrast$upper))
})

test_that("a comparator scored by another metric is refused", {
  f <- selection_fixture()
  lad <- suppressWarnings(grain_ladder(f$x, f$y, selection_learner(), folds = f$folds,
                                        metric = "tss", verbose = FALSE))
  expect_error(select_grain(f$x, f$y, selection_learner(), folds = f$folds, inner = 3L,
                            compare = lad, verbose = FALSE),
               "same metric")
})

test_that("a candidate set with nothing to choose between is refused", {
  f <- selection_fixture()
  expect_error(select_grain(f$x[1L], f$y, selection_learner(), folds = f$folds, inner = 3L,
                            verbose = FALSE),
               "at least two candidates")
  expect_error(select_grain(f$x, f$y, selection_learner(), folds = f$folds, inner = 1L,
                            verbose = FALSE),
               "at least 2")
})

test_that("adding a grain to the set widens the search with no other change", {
  sim <- sim_series(n_unit = 48L, days = 90L, seed = 33L)
  y <- sim_response(sim, n_var = 3L, seed = 34L)
  narrow <- grain_matrix(sim$readings, plot, t, temp, grain = c("week", "month"))
  wide <- timesift_set(c(as.list(narrow),
                          list(week_extreme = grain_matrix(sim$readings, plot, t, temp,
                                                            grain = "week",
                                                            stats = c("cold_day", "mean",
                                                                      "warm_day")))))
  folds <- fold_map(y, v = 3L, seed = 9L)
  a <- suppressWarnings(select_grain(narrow, y, selection_learner(), folds = folds, inner = 3L,
                                     verbose = FALSE))
  b <- suppressWarnings(select_grain(wide, y, selection_learner(), folds = folds, inner = 3L,
                                     verbose = FALSE))
  expect_equal(nrow(a$candidates), 2L)
  expect_equal(nrow(b$candidates), 3L)
  expect_true("week_extreme" %in% b$candidates$grain)
})


# Recovery. The generating grain is planted, so the selector can be asked whether it finds it and
# whether the level it reports is honest about how it was chosen.

# Units differ only in a slow component. Hourly noise buries it, and averaging over a month recovers
# it, so the monthly grain is the grain the response was generated at.
planted_grain <- function(n_unit = 72L, days = 168L, noise = 20, seed = 81L) {
  set.seed(seed)
  t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24L * days)
  units <- sprintf("p%03d", seq_len(n_unit))
  warmth <- stats::rnorm(n_unit)
  value <- as.numeric(vapply(warmth, function(w) {
    w * 1.5 + stats::rnorm(length(t), sd = noise)
  }, numeric(length(t))))
  list(units = units, warmth = warmth,
       readings = data.frame(plot = rep(units, each = length(t)), t = rep(t, times = n_unit),
                             temp = value, stringsAsFactors = FALSE))
}

planted_response <- function(sim, n_var = 8L, strength = 3, seed = 82L) {
  set.seed(seed)
  sign <- rep(c(1, -1), length.out = n_var)
  matrix(stats::rbinom(length(sim$warmth) * n_var, 1L,
                       stats::plogis(strength * as.numeric(outer(sim$warmth, sign)))),
         ncol = n_var, dimnames = list(sim$units, paste0("sp", seq_len(n_var))))
}

test_that("the grain the response was generated at is selected above chance", {
  sim <- planted_grain()
  y <- planted_response(sim)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = c("day", "week", "month"))
  sel <- suppressWarnings(select_grain(x, y, elasticnet(),
                                       folds = fold_map(y, v = 5L, seed = 7L),
                                       inner = 4L, seed = 3L, verbose = FALSE))
  picked <- table(factor(sel$selected$grain, levels = names(x)))
  # Chance over three candidates is a third of the five outer folds; the planted grain has to beat
  # that, and the finest grain, where the signal is buried, must not win outright.
  expect_gt(picked[["month"]], nrow(sel$selected) / 3)
  expect_gte(picked[["month"]], max(picked[["day"]], picked[["week"]]))
})

test_that("the nested estimate stays under what choosing on the held-out units would have paid", {
  sim <- planted_grain(seed = 83L)
  y <- planted_response(sim, seed = 84L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = c("day", "week", "month"))
  folds <- fold_map(y, v = 5L, seed = 7L)
  lad <- suppressWarnings(grain_ladder(x, y, elasticnet(), folds = folds, verbose = FALSE))
  sel <- suppressWarnings(select_grain(x, y, elasticnet(), folds = folds, inner = 4L,
                                       seed = 3L, compare = lad, verbose = FALSE))

  # The bound the nested estimate must respect is the oracle: the same candidates, the same fits,
  # but the grain for each cell picked with the held-out score itself. The procedure picks one of
  # those candidates without seeing them, so cell for cell it cannot come out above the oracle, and
  # the gap is what selecting on the test units would have bought.
  best_cell <- tapply(lad$score, paste(lad$variable, lad$fold), function(v) {
    if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
  })
  keep <- !is.na(best_cell)
  variable <- sub(" .*$", "", names(best_cell)[keep])
  oracle <- mean(tapply(as.numeric(best_cell[keep]), variable, mean))
  own <- sel$estimate$score[sel$estimate$metric == attr(sel, "metric")]
  expect_lte(own, oracle)
  expect_lt(own, oracle)

  # Against the finest grain, where the planted signal is buried, the procedure must still win.
  against_day <- sel$contrast[sel$contrast$b == "day|elasticnet", ]
  expect_gt(against_day$diff, 0)
  expect_gt(against_day$lower, 0)
})

test_that("a fold's held-out predictions are those of the candidate it selected", {
  sim <- planted_grain(n_unit = 48L, days = 90L, seed = 87L)
  y <- planted_response(sim, n_var = 4L, seed = 88L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = c("week", "month"))
  folds <- fold_map(y, v = 3L, seed = 7L)
  lad <- suppressWarnings(grain_ladder(x, y, elasticnet(), folds = folds, verbose = FALSE))
  sel <- suppressWarnings(select_grain(x, y, elasticnet(), folds = folds, inner = 3L,
                                       seed = 3L, verbose = FALSE))
  # The refit is the ladder's own fit on the same units at the same grain, so every cell of the
  # selected procedure is a cell of the ladder rather than a number from a second fitting path. It
  # is what makes the oracle a bound rather than a comparison of two different pipelines.
  f <- stats::setNames(as.integer(folds), names(folds))
  for (i in seq_len(nrow(sel$selected))) {
    k <- sel$selected$fold[i]
    arm <- paste(sel$selected$grain[i], sel$selected$learner[i], sep = "|")
    held <- names(f)[f == k]
    expect_equal(attr(sel$scores, "predictions")[["selected|selected"]][held, ],
                 attr(lad, "predictions")[[arm]][held, ])
  }
})

test_that("with no signal at any grain the procedure scores at the design's own floor", {
  sim <- planted_grain(seed = 85L)
  set.seed(86)
  y <- matrix(stats::rbinom(length(sim$units) * 4L, 1L, 0.35), ncol = 4L,
              dimnames = list(sim$units, paste0("sp", 1:4)))
  x <- grain_matrix(sim$readings, plot, t, temp, grain = c("week", "month"))
  folds <- fold_map(y, v = 5L, seed = 7L)
  sel <- suppressWarnings(select_grain(x, y, elasticnet(), folds = folds, inner = 4L,
                                       seed = 3L, verbose = FALSE))
  # TSS read at the cut that maximises it is biased upward on cells this small, so the floor is what
  # a design with no signal reports rather than zero.
  floor <- tss_inflation(y, folds, skill = 0, replicates = 60L, seed = 12L)$reported
  own <- sel$estimate$score[sel$estimate$metric == "tss"]
  expect_lt(own, floor + 0.12)
})

test_that("a selection hands its control to the inner search and to the refit alike", {
  f <- selection_fixture(v = 2L)
  seen <- new.env(parent = emptyenv())
  seen$epochs <- integer()
  trained <- learner(
    "trained",
    fit = function(x, y, control, ...) {
      seen$epochs <- c(seen$epochs, control$epochs)
      list(coef = colMeans(y))
    },
    predict = function(model, x) {
      outer(rank(apply(x[, , 1, drop = FALSE], 1L, mean)) / dim(x)[1L], model$coef,
            function(a, b) a)
    },
    multi = "joint")
  suppressWarnings(select_grain(f$x, f$y, trained, folds = f$folds, inner = 2L,
                                control = train_control(epochs = 7L), verbose = FALSE))
  # Two outer folds, each running an inner ladder over three grains at two inner folds and one
  # refit: nothing in that chain may reach a learner without the control the caller gave.
  expect_equal(length(seen$epochs), 2L * (3L * 2L + 1L))
  expect_true(all(seen$epochs == 7L))
})

test_that("the one-standard-error rule takes the coarsest candidate inside the band", {
  grid <- data.frame(grain = c("day", "week", "month", "year"), learner = "l",
                     score = c(0.70, 0.69, 0.66, 0.55), se = c(0.02, 0.03, 0.01, 0.01))
  size <- data.frame(bins = c(365, 52, 12, 1), channels = 1)
  expect_equal(.choose_candidate(grid, size, "argmax"), 1L)
  # The band is the best candidate's own standard error: 0.70 - 0.02 admits the week and not the
  # month, however small the month's own error is.
  expect_equal(.choose_candidate(grid, size, "coarsest_adequate"), 2L)
  grid$score[3L] <- 0.685
  expect_equal(.choose_candidate(grid, size, "coarsest_adequate"), 3L)
  # Between two candidates with the same bins, fewer channels is coarser.
  size2 <- data.frame(bins = c(52, 52), channels = c(3, 1))
  two <- data.frame(grain = c("week.mmm", "week.mean"), learner = "l", score = c(0.70, 0.695),
                    se = c(0.01, 0.01))
  expect_equal(.choose_candidate(two, size2, "coarsest_adequate"), 2L)
  # A standard error that could not be computed leaves only the ties with the best.
  grid$se[1L] <- NA_real_
  expect_equal(.choose_candidate(grid, size, "coarsest_adequate"), 1L)
})

test_that("the coarsest adequate grain is the generating grain or coarser where the profile is flat", {
  # The grain-invariant control of simulate_records(): the driver is the unit's constant offset,
  # which every grain reports exactly, so no candidate is better than another inside the training
  # data beyond noise. The mechanism is anchored on one season, which is the generating grain.
  sim <- simulate_records(n = 240L, mechanism = "season", variables = 4L, prevalence = 0.3,
                          auc = 0.85, step_hours = 6, anomaly_sd = 0.1, offset_effect = 1,
                          seed = 11L)
  expect_equal(sim$grain, "season")
  x <- grain_matrix(sim$readings, unit, time, reading,
                    grain = c("month", "season", "year"))
  sel <- select_grain(x, sim$y, elasticnet(), folds = fold_map(sim$y, v = 4L, seed = 5L),
                      inner = 4L, rule = "coarsest_adequate", seed = 2L, verbose = FALSE)
  expect_equal(attr(sel, "rule"), "coarsest_adequate")
  bins <- vapply(x, function(m) dim(m)[2L], numeric(1L))
  expect_true(all(bins[sel$selected$grain] <= bins[["season"]]))
  expect_true(all(sel$selected$inner_score >= sel$selected$inner_best - sel$selected$inner_se))
  # The choice is the rule applied to the inner scores the object carries, fold by fold.
  for (k in sel$selected$fold) {
    g <- sel$inner[sel$inner$fold == k, , drop = FALSE]
    best <- which.max(g$score)
    ok <- g$grain[g$score >= g$score[best] - g$se[best]]
    expect_equal(sel$selected$grain[sel$selected$fold == k], ok[which.min(bins[ok])])
  }
})

test_that("where one candidate clearly separates, the coarsest adequate rule is the argmax", {
  # The season mechanism on the anomaly alone: one season bin carries the driver, and the year bin,
  # which averages four seasons over a unit offset the driver does not read, carries almost
  # nothing. The year is the coarser candidate and the rule must not take it.
  sim <- simulate_records(n = 240L, mechanism = "season", variables = 4L, prevalence = 0.3,
                          auc = 0.85, step_hours = 6, seed = 13L)
  x <- grain_matrix(sim$readings, unit, time, reading, grain = c("season", "year"))
  folds <- fold_map(sim$y, v = 4L, seed = 5L)
  coarse <- select_grain(x, sim$y, elasticnet(), folds = folds, inner = 4L,
                         rule = "coarsest_adequate", seed = 2L, verbose = FALSE)
  top <- select_grain(x, sim$y, elasticnet(), folds = folds, inner = 4L, seed = 2L,
                      verbose = FALSE)
  year <- coarse$inner[coarse$inner$grain == "year", ]
  best <- coarse$selected$inner_best[match(year$fold, coarse$selected$fold)]
  se <- coarse$selected$inner_se[match(year$fold, coarse$selected$fold)]
  expect_true(all(year$score < best - se))
  expect_equal(coarse$selected$grain, top$selected$grain)
  expect_true(all(coarse$selected$grain == "season"))
})

test_that("tss at a given cut is sensitivity plus specificity minus one there", {
  y <- c(0, 0, 0, 1, 1, 1)
  p <- c(0.1, 0.4, 0.6, 0.3, 0.7, 0.9)
  expect_equal(tss(y, p, threshold = 0.5), 2 / 3 - 1 / 3)
  expect_equal(tss(y, p, threshold = 0.95), 0)
  expect_true(is.na(tss(y, p, threshold = NA_real_)))
  expect_true(is.na(tss(c(0, 0, 0), c(0.1, 0.2, 0.3), threshold = 0.15)))
  expect_gte(tss(y, p), tss(y, p, threshold = 0.5))
})

# A binormal design with a known answer. Each variable's score is N(delta, 1) on a presence and
# N(0, 1) on an absence, so the population skill at the best cut, delta / 2, is 2 * pnorm(delta / 2)
# - 1. A learner that reads the score and fits nothing makes the inner out-of-fold predictions the
# score itself, so the cut each outer fold learns is decision_threshold() on its training units.
binormal_cut_design <- function(n = 3000L, v = 4L, skill = 0.6, seed = 21L) {
  set.seed(seed)
  delta <- 2 * stats::qnorm((skill + 1) / 2)
  units <- sprintf("u%04d", seq_len(n))
  y <- matrix(stats::rbinom(n * v, 1L, 0.3), nrow = n,
              dimnames = list(units, sprintf("sp%d", seq_len(v))))
  score <- y * delta + matrix(stats::rnorm(n * v), nrow = n, dimnames = dimnames(y))
  noisy <- score + matrix(stats::rnorm(n * v, sd = 2), nrow = n, dimnames = dimnames(y))
  reader <- learner("reader", fit = function(x, y, ...) list(),
                    predict = function(model, x) matrix(x[, , 1], nrow = dim(x)[1L]))
  list(y = y, score = score, skill = skill, learner = reader,
       x = timesift_set(list(good = feature_matrix(score, "good"),
                             noisy = feature_matrix(noisy, "noisy"))))
}

test_that("a cut learned on the inner folds reads the population skill a maximised cut overstates", {
  d <- binormal_cut_design()
  folds <- fold_map(d$y, v = 5L, seed = 3L)
  sel <- select_grain(d$x, d$y, d$learner, folds = folds, inner = 5L, threshold = "youden",
                      verbose = FALSE)
  expect_true(all(sel$selected$grain == "good"))
  expect_equal(attr(sel, "threshold"), "youden")

  # Every cut is the Youden cut of the outer training units' own scores: the inner out-of-fold
  # predictions of this learner are the score, and no unit of the outer test fold enters it.
  f <- stats::setNames(as.integer(folds), names(folds))
  for (r in seq_len(nrow(sel$thresholds))) {
    k <- sel$thresholds$fold[r]
    v <- sel$thresholds$variable[r]
    train <- names(f)[f != k]
    expect_equal(sel$thresholds$threshold[r],
                 decision_threshold(d$y[train, v], d$score[train, v], "youden"))
  }

  # The held-out cells read at that cut average to the planted skill within Monte Carlo error,
  # while the maximum over cuts on the same cells is never below it and averages above the truth.
  cut <- sel$cut_scores[!is.na(sel$cut_scores$score), ]
  max_tss <- sel$scores[!is.na(sel$scores$score), ]
  expect_equal(nrow(cut), nrow(max_tss))
  expect_true(all(max_tss$score >= cut$score))
  level <- sel$estimate[sel$estimate$metric == "tss_inner_cut", ]
  expect_lt(abs(level$score - d$skill), 3 * level$se + 0.01)
  expect_gt(sel$estimate$score[sel$estimate$metric == "tss"], level$score)
  expect_output(print(sel), "cut learned on the inner folds")
})

test_that("without a threshold rule no cut is learned and a rule of another name is refused", {
  d <- binormal_cut_design(n = 300L, v = 2L)
  folds <- fold_map(d$y, v = 3L, seed = 3L)
  sel <- select_grain(d$x, d$y, d$learner, folds = folds, inner = 3L, verbose = FALSE)
  expect_null(sel$thresholds)
  expect_null(sel$cut_scores)
  expect_false("tss_inner_cut" %in% sel$estimate$metric)
  expect_error(select_grain(d$x, d$y, d$learner, folds = folds, inner = 3L, threshold = "median",
                            verbose = FALSE), "should be one of")
})

test_that("the inner fold count is checked before it is coerced", {
  expect_error(.inner_splitter("five"), 'got "five"')
  expect_error(.inner_splitter(1L), "got 1")
  expect_error(.inner_splitter(2.5), "got 2.5")
  expect_type(.inner_splitter(3), "closure")
})
