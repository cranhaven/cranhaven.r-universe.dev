#' Penalised regression on the flattened representation
#'
#' One elastic net per variable, over every bin-by-channel column of the representation and, by
#' default, their squares. There is no discrete selection step: the penalty path uses every column
#' and shrinks, and the penalty itself is chosen by an inner cross-validation on the fitting units,
#' so nothing about the model is decided outside the fold it is fitted in.
#'
#' The family is the response head's: a binary cross-entropy loss fits a logistic model and a
#' squared-error loss a linear one, so the learner is the same under a presence-absence head and
#' under a continuous one. So are the case weights: the head's `weights`, [positive_weights()]
#' for presence-absence, are what every learner that ships fits under.
#'
#' The inner folds are dealt for each response and stratified on it, so a rare outcome is spread
#' over them as evenly as its count allows. A presence-absence response whose inner training sets
#' cannot each hold two of each outcome, the fewest a logistic path is fitted to, has too few of
#' one outcome to choose a penalty on. It is predicted its share among the fitting units, as a
#' response holding one outcome is, and the fit names every such response in `unfitted`.
#'
#' A reweighted fit that does not settle at a penalty ends its path there and keeps the points
#' before it, which is what glmnet does with the same event, and the penalty is chosen over the
#' points fitted. It happens where a rare outcome is nearly separable at the small end of the
#' path. The fit names every response whose path on the fitting units, or on any inner fold, ended
#' that way in `stopped`.
#'
#' This is the aggregate-feature side of the comparison the package was built for, and it is the
#' fair opponent for a network: a per-fold discrete selector pays selection variance a network
#' never pays, so beating that one is not a matched result.
#'
#' The path is fitted by the same core the Python package calls, so the two return the same
#' coefficients for the same input. Its conventions are glmnet's, which is what the arm is
#' measured against: weights normalised to sum to one, columns centred and scaled by their
#' weighted mean and weighted standard deviation, a hundred penalties down from the smallest that
#' leaves every coefficient at zero, and the held-out deviance read fold by fold.
#'
#' @param data A representation the learner is pinned to, or `NULL` to run across every
#'   representation of the run.
#' @param alpha Elastic-net mixing, `1` lasso and `0` ridge.
#' @param n_inner Folds of the inner cross-validation that chooses the penalty.
#' @param squares Add the square of every column, giving the same quadratic capacity a
#'   second-order polynomial term would.
#' @param s Which penalty of the inner path to predict at: `"lambda.min"`, `"lambda.1se"`, or a
#'   penalty of its own, which is interpolated between the two points of the path around it.
#' @param n_lambda Points of the penalty path.
#' @param thresh Where the coordinate descent stops, read off the largest coefficient move of a
#'   pass. The default leaves the fit as close to the optimum as glmnet's own default does; a
#'   looser one is faster and a tighter one costs time roughly in proportion.
#' @param threads How many fits of one response's inner cross-validation run at once. The path on
#'   every fitting unit and the path of each inner fold are one independent fit each, so they
#'   parallelise without sharing anything, and `n_inner + 1` threads is as many as a response can
#'   use. The default is serial, because a package does not take a machine's cores without being
#'   asked. What comes back does not depend on it.
#' @param seed Seed for the inner cross-validation's fold draw, which is random and would otherwise
#'   make the fit irreproducible.
#'
#' @return A [learner()].
#'
#' @examples
#' elasticnet(alpha = 0.5)
#'
#' @export
elasticnet <- function(data = NULL, alpha = 0.5, n_inner = 5L, squares = TRUE, s = "lambda.min",
                       n_lambda = 100L, thresh = 1e-8, threads = 1L, seed = 1L) {
  learner(
    name = "elasticnet",
    data = data, reads = "tabular", multi = "separate",
    params = list(alpha = alpha, n_inner = n_inner, squares = squares, s = s,
                  n_lambda = n_lambda, thresh = thresh, threads = threads, seed = seed),
    fit = function(x, y, alpha, n_inner, squares, s, n_lambda, thresh, threads, seed, head,
                   group = NULL, ...) {
      family <- .head_family(head)
      m <- .design(x, squares)
      if (ncol(m) < 2L) {
        stop("the elastic net needs at least two columns to penalise over, and this ",
             "representation flattens to ", ncol(m), ". Give it more bins or channels, or ",
             "leave `squares = TRUE`.", call. = FALSE)
      }
      old <- .seed_state()
      on.exit(.restore_seed(old), add = TRUE)
      seeds <- .variable_seeds(seed, y)
      weights <- .head_weights(head, y)
      models <- lapply(seq_len(ncol(y)), function(j) {
        yj <- y[, j]
        if (length(unique(yj)) < 2L) {
          return(mean(yj))
        }
        # The inner folds are dealt here rather than inside the path, so a grouping the outer
        # folds keep whole stays whole where the penalty is chosen, and a rare outcome is spread
        # over them rather than left to a plain deal.
        inner <- .inner_folds(yj, n_inner, seeds[j], group)
        if (identical(family, "binomial") && !.inner_fittable(yj, inner)) {
          return(mean(yj))
        }
        labels <- sort(unique(inner))
        .penalised_cv(m, yj, weights[, j], family, alpha, match(inner, labels) - 1L,
                      length(labels), n_lambda = n_lambda, thresh = thresh, threads = threads)
      })
      unfitted <- colnames(y)[vapply(models, is.numeric, logical(1L))]
      stopped <- colnames(y)[vapply(models, .penalised_stopped, logical(1L))]
      list(models = models, squares = squares, s = s, columns = colnames(m),
           unfitted = unfitted, stopped = stopped)
    },
    predict = function(model, x) {
      m <- .design(x, model$squares)
      .as_predictions(vapply(model$models, function(f) {
        if (is.numeric(f)) rep(f, nrow(m))
        else .penalised_predict(f, m, model$s)
      }, numeric(nrow(m))), nrow(m))
    }
  )
}

#' Forward selection by AIC on the flattened representation
#'
#' One generalised linear model per variable, its predictors chosen by forward selection over every
#' bin-by-channel column, admitting a column while it lowers AIC and stopping at a fixed budget.
#' Each candidate enters as an orthogonal polynomial, so a term can be non-monotone in the reading
#' the way a niche optimum is. The family is the response head's: logistic under a binary
#' cross-entropy loss, Gaussian under a squared-error one. So are the case weights, so a rare
#' response weighs here what it weighs in every other learner.
#'
#' Selection happens inside whichever units the learner is handed, so under [grain_ladder()] it is
#' redone in every fold. That is the footing the other learners are fitted on. Reported beside a
#' penalised fit it also prices discrete selection: choosing a handful of columns out of hundreds
#' is high variance, and that variance is a cost of the selector rather than of the features.
#'
#' @inheritParams elasticnet
#' @param max_terms Predictors admitted before selection stops.
#' @param degree Polynomial degree each admitted column enters at.
#'
#' @return A [learner()].
#'
#' @examples
#' stepwise(max_terms = 3)
#'
#' @export
stepwise <- function(data = NULL, max_terms = 3L, degree = 2L) {
  learner(
    name = "stepwise",
    data = data, reads = "tabular", multi = "separate",
    params = list(max_terms = max_terms, degree = degree),
    fit = function(x, y, max_terms, degree, head, ...) {
      family <- .head_family(head)
      m <- .flatten(x)
      weights <- .head_weights(head, y)
      models <- lapply(seq_len(ncol(y)), function(j) {
        .forward_aic(m, y[, j], max_terms, degree, family, weights[, j])
      })
      list(models = models, columns = colnames(m), degree = degree, family = family)
    },
    predict = function(model, x) {
      m <- .flatten(x)
      .as_predictions(vapply(model$models, function(f) .predict_forward(f, m), numeric(nrow(m))),
                      nrow(m))
    }
  )
}

# Forward selection by AIC, one column admitted at a time. The polynomial basis is stored with the
# fit rather than rebuilt, because an orthogonal basis refitted on new units is a different basis.
.forward_aic <- function(m, y, max_terms, degree, family, w = rep(1, length(y))) {
  if (length(unique(y)) < 2L) {
    return(list(constant = mean(y)))
  }
  link <- .glm_family(family)
  chosen <- integer(0)
  bases <- list()
  best_aic <- .glm_aic(stats::glm(y ~ 1, family = link, weights = w), family)
  # A column holding one value has no polynomial basis to enter as, so it is not a candidate. It
  # is the intercept the search already starts from, and offering it is what makes an orthogonal
  # basis divide by a norm of zero.
  offered <- which(vapply(seq_len(ncol(m)), function(j) length(unique(m[, j])) > 1L, logical(1L)))
  repeat {
    if (length(chosen) >= max_terms) {
      break
    }
    gains <- rep(NA_real_, ncol(m))
    fits <- vector("list", ncol(m))
    for (j in setdiff(offered, chosen)) {
      b <- .poly_basis(m[, j], degree)
      d <- .design_frame(c(bases, list(b)))
      # A candidate whose fit separates the response, or does not settle, is refused rather than
      # admitted: those are the states the criterion cannot be read off, and admitting one would
      # let the search prefer a column for having no answer. Anything the fitter raises as an
      # error is a fault rather than a verdict on the candidate, and propagates.
      fit <- tryCatch(stats::glm(y ~ ., data = d, family = link, weights = w),
                      warning = function(cond) NULL)
      if (!is.null(fit) && is.finite(.glm_aic(fit, family))) {
        gains[j] <- .glm_aic(fit, family)
        fits[[j]] <- list(fit = fit, basis = b)
      }
    }
    if (!any(is.finite(gains)) || min(gains, na.rm = TRUE) >= best_aic) {
      break
    }
    j <- which.min(gains)
    best_aic <- gains[j]
    chosen <- c(chosen, j)
    bases <- c(bases, list(fits[[j]]$basis))
    current <- fits[[j]]$fit
  }
  if (!length(chosen)) {
    return(list(constant = mean(y)))
  }
  list(columns = chosen, bases = bases, fit = current)
}

.predict_forward <- function(f, m) {
  if (!is.null(f$constant)) {
    return(rep(f$constant, nrow(m)))
  }
  b <- lapply(seq_along(f$columns), function(k) .apply_basis(f$bases[[k]], m[, f$columns[k]]))
  as.numeric(stats::predict(f$fit, .design_frame(b), type = "response"))
}

# An orthogonal polynomial basis, kept with the coefficients it was fitted beside so that new units
# are mapped through the same basis rather than through one re-derived from themselves.
.poly_basis <- function(v, degree) {
  degree <- min(degree, length(unique(v)) - 1L)
  if (degree < 1L) {
    stop("a column holding one value has no polynomial basis to enter as.", call. = FALSE)
  }
  b <- stats::poly(v, degree = degree)
  list(degree = degree, coefs = attr(b, "coefs"), values = b)
}

.apply_basis <- function(basis, v) {
  list(degree = basis$degree, coefs = basis$coefs,
       values = stats::poly(v, degree = basis$degree, coefs = basis$coefs))
}

.design_frame <- function(bases) {
  cols <- list()
  for (k in seq_along(bases)) {
    b <- bases[[k]]$values
    for (p in seq_len(ncol(b))) {
      cols[[sprintf("t%d_%d", k, p)]] <- as.numeric(b[, p])
    }
  }
  as.data.frame(cols)
}

.design <- function(x, squares) {
  m <- .flatten(x)
  if (!squares) {
    return(m)
  }
  out <- cbind(m, m^2)
  colnames(out) <- c(colnames(m), paste0(colnames(m), "^2"))
  out
}

# vapply drops to a vector when there is one unit to predict, which would reach the caller as one
# row per variable instead of one column. The shape is restored here so a single new site predicts
# the same way a thousand do.
.as_predictions <- function(p, units) {
  matrix(p, nrow = units)
}

