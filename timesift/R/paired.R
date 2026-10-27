#' Compare two arms cell by cell
#'
#' Two arms scored on the same held-out units do not necessarily have the same set of defined
#' cells, so a difference of two marginal means is not a difference between the arms. This takes
#' the difference inside each `(variable, fold)` cell both arms scored, averages it within a
#' variable over its folds, and summarises those per-variable means, the variables being the
#' independent replicates.
#'
#' Pairing removes the variation between variables and the part of a threshold-selected metric's
#' bias that the design sets, but not the part that belongs to each arm. TSS read at the threshold
#' that maximises it is biased upward where presences are thin, and how far depends on how an arm's
#' predictions are distributed as well as on how many presences the cell holds, so two arms of
#' equal skill on the same cell can carry different biases. A difference in TSS can therefore favour
#' one arm with no difference in skill behind it. A threshold-free metric such as [roc_auc()] has no
#' cut to choose, and a TSS contrast is best read beside the same contrast under it.
#'
#' An arm is named whole, by its grain and its learner. A learner named alone would have to take
#' its best grain, and that grain is chosen on the held-out scores the contrast is then read off:
#' the difference becomes one between two maxima, favouring whichever learner ran across more
#' grains, and neither the interval nor the p-value accounts for the choice. It is the mechanism
#' [tss_inflation()] measures one level down, and here pairing does not cancel it.
#' [select_grain()] chooses a grain on inner folds instead, and its `compare` argument contrasts
#' the selection with the arms of a ladder on matched cells.
#'
#' @param ladder A [grain_ladder()] result.
#' @param a,b The two arms, each named `"grain|learner"`.
#' @param interval Which interval the row carries. `"variables"` is the spread across the response
#'   variables of this dataset. `"nested_cv"` is an interval for the difference in the two arms'
#'   risk on a new sample, by the nested cross-validation of Bates, Hastie and Tibshirani (2024)
#'   read on the difference of the two arms' cell scores, which needs a ladder fitted with
#'   `grain_ladder(interval = "nested_cv")`. The paper gives the estimator for one procedure's
#'   error and names the difference between two as an extension; the identity its estimator rests
#'   on is on the difference of two losses as much as on one.
#'
#' @references Bates, S., Hastie, T. and Tibshirani, R. (2024). Cross-validation: what does it
#'   estimate and how well does it do it? *Journal of the American Statistical Association*
#'   **119**(546), 1434-1445. \doi{10.1080/01621459.2023.2197686}
#'
#' @return A one-row data frame: the mean per-variable difference; the centre of the interval and
#'   its bounds, on Student's t across variables or on nested cross-validation as `interval`
#'   names, which the row carries; the number of variables the difference favours; the paired
#'   cells and variables it rests on; a Wilcoxon signed-rank p-value; and `p_method`, `"exact"` where the p-value is
#'   read off the exact distribution and `"normal"` where it is the normal approximation with
#'   continuity and tie corrections, which it is when the per-variable differences hold a zero or
#'   a tie or number fifty or more.
#'
#' @examples
#' set.seed(1)
#' t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 200)
#' units <- sprintf("p%02d", 1:60)
#' warmth <- rnorm(60)
#' d <- data.frame(
#'   plot = rep(units, each = length(t)), t = rep(t, length(units)),
#'   temp = as.numeric(vapply(warmth, function(w) w + sin(seq_along(t) / 300) + rnorm(length(t)),
#'                            numeric(length(t)))))
#' y <- matrix(rbinom(120, 1, plogis(c(warmth, -warmth))), nrow = 60,
#'             dimnames = list(units, c("sp1", "sp2")))
#' x <- grain_matrix(d, plot, t, temp, grain = c("week", "month"))
#' lad <- grain_ladder(x, y, elasticnet(), folds = fold_map(y, v = 3), verbose = FALSE)
#' paired_contrast(lad, "week|elasticnet", "month|elasticnet")
#'
#' @export
paired_contrast <- function(ladder, a, b, interval = c("variables", "nested_cv")) {
  interval <- .check_interval(interval[1L])
  ra <- .arm_rows(ladder, a)
  rb <- .arm_rows(ladder, b)
  key_a <- paste(ra$variable, ra$fold)
  key_b <- paste(rb$variable, rb$fold)
  shared <- intersect(key_a[!is.na(ra$score)], key_b[!is.na(rb$score)])
  if (!length(shared)) {
    stop("the two arms share no cell both scored.", call. = FALSE)
  }
  va <- ra$score[match(shared, key_a)]
  vb <- rb$score[match(shared, key_b)]
  variable <- ra$variable[match(shared, key_a)]
  per_variable <- tapply(va - vb, variable, mean)

  n <- length(per_variable)
  d <- mean(per_variable)
  centre <- d
  half <- .t_margin(stats::sd(per_variable) / sqrt(n), n)
  lower <- d - half
  upper <- d + half
  if (interval == "nested_cv") {
    scorer <- attr(ladder, "scorer") %||% .metrics_reg$get(attr(ladder, "metric"))
    iv <- .ncv_read(.ncv_of(ladder), c(attr(ra, "label"), attr(rb, "label")),
                    attr(ladder, "response") %||% "presence_absence", scorer)
    centre <- iv$center
    lower <- iv$lower
    upper <- iv$upper
  }
  test <- .signed_rank(per_variable)
  data.frame(a = attr(ra, "label"), b = attr(rb, "label"), diff = d, center = centre,
             lower = lower, upper = upper,
             n_variable = n, n_cell = length(shared), n_favour = sum(per_variable > 0),
             p_value = test$p, p_method = test$method, interval = interval,
             stringsAsFactors = FALSE)
}

# The half-width of a 95 percent interval on a mean of `n` independent replicates with standard
# error `se`, on Student's t with n - 1 degrees of freedom. The replicates are response variables,
# and a design with a handful of them is where the normal quantile is too narrow.
.t_margin <- function(se, n) {
  ifelse(n > 1, stats::qt(0.975, pmax(n - 1, 1)) * se, NA_real_)
}

# Wilcoxon's signed-rank test of the per-variable differences against zero, and the method it is
# read by. The exact distribution holds below fifty values with no zero and no tie among their
# absolute values; otherwise the p-value is the normal approximation with continuity and tie
# corrections. The choice is made here, so wilcox.test() is never left to fall back on its own.
.signed_rank <- function(x) {
  nonzero <- x[x != 0]
  if (length(x) < 2L || !length(nonzero)) {
    return(list(p = NA_real_, method = NA_character_))
  }
  exact <- length(nonzero) < 50L && length(nonzero) == length(x) &&
    !anyDuplicated(abs(nonzero))
  list(p = stats::wilcox.test(x, exact = exact, correct = TRUE)$p.value,
       method = if (exact) "exact" else "normal")
}

.arm_rows <- function(ladder, arm) {
  if (!inherits(ladder, "timesift_ladder")) {
    stop("expected a grain_ladder() result, got ", class(ladder)[1L], ".", call. = FALSE)
  }
  # An arm is matched whole against the labels the ladder carries rather than split at a `|`, so
  # a grain or a learner whose own name holds one is still found.
  label <- paste(ladder$grain, ladder$learner, sep = "|")
  if (!arm %in% label) {
    if (arm %in% ladder$learner) {
      stop("\"", arm, "\" names a learner and no grain. Its best grain would be chosen on the ",
           "scores the contrast is read off, so name the arm whole, as \"",
           label[match(arm, ladder$learner)], "\", or let select_grain() choose the grain on ",
           "inner folds and contrast the selection through its `compare`.", call. = FALSE)
    }
    stop("no arm called \"", arm, "\" in this ladder.", call. = FALSE)
  }
  rows <- ladder[label == arm, , drop = FALSE]
  structure(rows, label = arm, grain = rows$grain[1L], learner = rows$learner[1L])
}

# The arm a learner named alone is read at: its best grain on the ladder's own scores. That is the
# natural arm for describing a fitted model, and never the arm a contrast is computed on.
.best_arm <- function(ladder, arm) {
  label <- paste(ladder$grain, ladder$learner, sep = "|")
  if (arm %in% label || !arm %in% ladder$learner) {
    return(arm)
  }
  s <- summary(ladder)
  s <- s[s$learner == arm & !is.na(s$score), , drop = FALSE]
  if (!nrow(s)) {
    return(arm)
  }
  paste(s$grain[which.max(s$score)], arm, sep = "|")
}

#' How much a self-selected threshold inflates the reported level
#'
#' The true skill statistic is read at the threshold that maximises it, chosen on the same held-out
#' units the score is then read on. That selection inflates the level, and by more the fewer
#' presences a cell holds. Most code carries the inflation silently; this measures it for the
#' presence counts of a given design.
#'
#' Predictions are simulated under a normal model in which the population skill is exactly `skill`,
#' at the cell sizes and presence counts of the response and fold map supplied, and the level is
#' read back exactly as [grain_ladder()] reports it. The gap between what comes back and the truth
#' planted is the inflation.
#'
#' The inflation is an expectation. A level read on one design is optimistic on average, not a
#' bound every reading sits above: a single level can fall below the population skill. The model
#' planted here is one distribution of predictions, and another at the same skill inflates by a
#' different amount, which is also why a paired difference [paired_contrast()] takes is not free of
#' it.
#'
#' @inheritParams scorable_cells
#' @param skill Population skill values to plant.
#' @param replicates Replicates per value.
#' @param seed Random seed.
#'
#' @return A data frame with one row per planted value: the truth, the mean level read back, the
#'   inflation, and its interval across replicates.
#'
#' @examples
#' set.seed(1)
#' y <- matrix(rbinom(1200, 1, 0.15), nrow = 200,
#'             dimnames = list(sprintf("p%03d", 1:200), paste0("sp", 1:6)))
#' tss_inflation(y, fold_map(y, v = 5), skill = c(0.6, 0.9), replicates = 40)
#'
#' @export
tss_inflation <- function(y, folds, skill = c(0.6, 0.7, 0.9), replicates = 200L, seed = 1L) {
  y <- .as_response(y)
  f <- .as_folds(folds, rownames(y))
  cells <- scorable_cells(y, stats::setNames(f, rownames(y)))
  cells <- cells[cells$scorable, , drop = FALSE]
  if (!nrow(cells)) {
    stop("no cell of this design is scorable, so there is no level to measure.", call. = FALSE)
  }

  old <- .seed_state()
  on.exit(.restore_seed(old), add = TRUE)
  set.seed(seed)

  out <- lapply(skill, function(target) {
    # Under a binormal model with unit variances the maximum of sensitivity + specificity - 1 is
    # 2 * Phi(delta / 2) - 1, so a separation is what plants a population skill exactly.
    delta <- 2 * stats::qnorm((target + 1) / 2)
    # The variables are visited in the order the mask carries them, which is C collation, and not
    # in the order tapply() would impose by turning them into a factor: that follows LC_COLLATE,
    # and the draws are consumed variable by variable, so a session in another locale would read a
    # different number off the same design. Same order as the Python side, for the same reason.
    variables <- unique(cells$variable)
    per_replicate <- vapply(seq_len(replicates), function(r) {
      by_variable <- vapply(variables, function(v) {
        mean(vapply(which(cells$variable == v), function(k) {
          n_pos <- cells$pres_test[k]
          n_neg <- cells$abs_test[k]
          tss(c(rep(1L, n_pos), rep(0L, n_neg)),
              c(stats::rnorm(n_pos, delta), stats::rnorm(n_neg)))
        }, numeric(1L)))
      }, numeric(1L))
      mean(by_variable)
    }, numeric(1L))
    data.frame(skill = target, reported = mean(per_replicate),
               inflation = mean(per_replicate) - target,
               lower = unname(stats::quantile(per_replicate, 0.025)),
               upper = unname(stats::quantile(per_replicate, 0.975)),
               replicates = replicates)
  })
  do.call(rbind, out)
}

#' What population skill a reported level is consistent with
#'
#' [tss_inflation()] maps a population skill to the level a design reports for it. This inverts
#' that map: given a level actually read off a ladder, it solves for the population skill whose
#' expected reported level equals it.
#'
#' It answers the question a level raises once the inflation is known, under the distribution of
#' predictions [tss_inflation()] plants; a model whose predictions are distributed otherwise is
#' inflated by a different amount. It does not correct a difference between two arms, whose
#' inflations need not be equal.
#'
#' @inheritParams tss_inflation
#' @param observed Reported levels to invert.
#' @param grid Population skills the forward map is measured on before interpolating between them.
#'
#' @return A data frame of one row per observed level: the level, the population skill it is
#'   consistent with, and whether that sits inside the grid the map was measured on.
#'
#' @examples
#' set.seed(1)
#' y <- matrix(rbinom(1200, 1, 0.15), nrow = 200,
#'             dimnames = list(sprintf("p%03d", 1:200), paste0("sp", 1:6)))
#' implied_skill(y, fold_map(y, v = 5), observed = 0.71, replicates = 40)
#'
#' @export
implied_skill <- function(y, folds, observed, grid = seq(0, 0.95, by = 0.05),
                          replicates = 200L, seed = 1L) {
  map <- tss_inflation(y, folds, skill = grid, replicates = replicates, seed = seed)
  map <- map[order(map$reported), , drop = FALSE]
  inside <- observed >= min(map$reported) & observed <= max(map$reported)
  data.frame(observed = observed,
             skill = stats::approx(map$reported, map$skill, xout = observed, rule = 2,
                                   ties = "ordered")$y,
             within_grid = inside)
}
