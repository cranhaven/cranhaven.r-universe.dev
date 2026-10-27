#' Random forest on the flattened representation
#'
#' One forest per response, over every bin-by-channel column of the representation: a probability
#' forest under a presence-absence head and a regression forest under a head with a squared-error
#' loss. Trees split on one column at a time and pay nothing for columns that carry nothing, so a
#' forest reads a wide tabular representation without a penalty path and without a selection step,
#' and it finds an interaction between two bins that a linear model would need the product term
#' for.
#'
#' The case weights are the response head's, [positive_weights()] under presence-absence, and
#' weight the bootstrap draw: a rare response is not fitted away here for a reason the other
#' learners do not share, because every learner that ships reads the same weights.
#'
#' @inheritParams elasticnet
#' @param trees Trees in the forest.
#' @param mtry Columns tried at each split, or `NULL` for the square root of the column count.
#' @param min_node Smallest node a split is made on.
#' @param seed Seed for the bootstrap draw and the split sampling, which are random and would
#'   otherwise make the fit irreproducible.
#'
#' @return A [learner()].
#'
#' @examples
#' forest(trees = 200L)
#'
#' @export
forest <- function(data = NULL, trees = 500L, mtry = NULL, min_node = 1L, seed = 1L) {
  learner(
    name = "forest",
    data = data, reads = "tabular", multi = "separate",
    needs = "ranger",
    params = list(trees = as.integer(trees), mtry = mtry, min_node = as.integer(min_node),
                  seed = as.integer(seed)),
    fit = function(x, y, trees, mtry, min_node, seed, head, ...) {
      family <- .head_family(head)
      m <- .flatten(x)
      try_columns <- if (is.null(mtry)) max(1L, floor(sqrt(ncol(m)))) else as.integer(mtry)
      seeds <- .variable_seeds(seed, y)
      weights <- .head_weights(head, y)
      models <- lapply(seq_len(ncol(y)), function(j) {
        yj <- y[, j]
        if (length(unique(yj)) < 2L) {
          return(mean(yj))
        }
        if (family == "binomial") {
          ranger::ranger(x = m, y = factor(yj, levels = c(0, 1)), num.trees = trees,
                         mtry = try_columns, min.node.size = min_node, probability = TRUE,
                         case.weights = weights[, j], num.threads = 1L, seed = seeds[j])
        } else {
          ranger::ranger(x = m, y = yj, num.trees = trees, mtry = try_columns,
                         min.node.size = min_node, case.weights = weights[, j],
                         num.threads = 1L, seed = seeds[j])
        }
      })
      list(models = models, columns = colnames(m), family = family)
    },
    predict = function(model, x) {
      m <- .flatten(x)
      .as_predictions(vapply(model$models, function(f) {
        if (is.numeric(f)) {
          return(rep(f, nrow(m)))
        }
        p <- stats::predict(f, data = m, num.threads = 1L)$predictions
        as.numeric(if (model$family == "binomial") p[, "1"] else p)
      }, numeric(nrow(m))), nrow(m))
    }
  )
}
