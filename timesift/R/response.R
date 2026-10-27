#' Which cells a score is defined on
#'
#' A per-variable score needs both classes among the held-out units, and a per-variable model needs
#' both classes among the units it was fitted on, so a `(variable, fold)` cell where either side of
#' the split is one-class carries no score. The mask says which cells those are.
#'
#' It is computed from the response and the fold map alone, with no model involved. Every learner
#' in a ladder is then restricted to the same cells, so their means share one denominator and every
#' paired difference runs on matched cells. Computing it from a model instead would let a joint
#' multi-label learner, which emits a number for every cell whether or not it could be fitted per
#' variable, be scored on cells its opponents were never fitted on.
#'
#' @inheritParams fold_map
#' @param folds A fold map from [fold_map()], or any named integer vector of the same shape.
#'
#' @return A data frame of one row per `(variable, fold)` cell, of class `timesift_cells`, with
#'   the counts on each side of the split and a `scorable` flag.
#'
#' @examples
#' set.seed(1)
#' y <- matrix(rbinom(600, 1, 0.2), nrow = 100,
#'             dimnames = list(sprintf("p%03d", 1:100), paste0("sp", 1:6)))
#' cells <- scorable_cells(y, fold_map(y, v = 5))
#' cells
#'
#' @export
scorable_cells <- function(y, folds) {
  y <- .as_response(y)
  f <- .as_folds(folds, rownames(y))
  levels <- sort(unique(f))
  vars <- colnames(y)

  pres_test <- vapply(levels, function(k) colSums(y[f == k, , drop = FALSE]), numeric(ncol(y)))
  n_test <- vapply(levels, function(k) sum(f == k), numeric(1L))
  pres_test <- matrix(pres_test, nrow = ncol(y), dimnames = list(vars, NULL))
  n_occ <- colSums(y)

  out <- data.frame(
    variable = rep(vars, times = length(levels)),
    fold = rep(levels, each = length(vars)),
    n_occ = rep(n_occ, times = length(levels)),
    pres_test = as.vector(pres_test),
    abs_test = rep(n_test, each = length(vars)) - as.vector(pres_test),
    stringsAsFactors = FALSE
  )
  out$pres_train <- out$n_occ - out$pres_test
  out$abs_train <- (nrow(y) - out$n_occ) - out$abs_test
  # Counts, so integers: a mask read back from a file carries integers, and a mask that compared
  # unequal to it on storage mode alone would report a difference there is none of.
  for (nm in c("n_occ", "pres_train", "abs_train", "pres_test", "abs_test")) {
    out[[nm]] <- as.integer(out[[nm]])
  }
  out$scorable <- out$pres_train >= 1L & out$abs_train >= 1L &
    out$pres_test >= 1L & out$abs_test >= 1L
  # C collation for the variable names, so the cell order is the same on every machine and the
  # same as the one the Python side builds.
  keep <- c("variable", "fold", "n_occ", "pres_train", "abs_train", "pres_test", "abs_test",
            "scorable")
  out <- out[order(out$variable, out$fold, method = "radix"), keep]
  rownames(out) <- NULL
  structure(out, class = c("timesift_cells", "data.frame"))
}

#' @export
print.timesift_cells <- function(x, ...) {
  keep <- tapply(x$scorable, x$variable, any)
  cat("<timesift cells>", .plural(nrow(x), "cell"), "over",
      .plural(length(keep), "variable"), "\n")
  cat(sprintf("scorable: %d (%.1f%%); variables with at least one scorable fold: %d of %d\n",
              sum(x$scorable), 100 * mean(x$scorable), sum(keep), length(keep)))
  invisible(x)
}

# The response reaches everything downstream as a numeric matrix with unit identifiers in its row
# names, whether it arrived as a matrix, as a data frame with an identifier column, or as a bare
# vector for a single variable.
.as_response <- function(y) {
  if (is.vector(y) && !is.list(y)) {
    y <- matrix(y, ncol = 1L, dimnames = list(names(y), "y"))
  }
  if (is.data.frame(y)) {
    id <- vapply(y, function(col) is.character(col) || is.factor(col), logical(1L))
    if (sum(id) > 1L) {
      stop("the response holds ", sum(id), " non-numeric columns (",
           paste(names(y)[id], collapse = ", "),
           "). One may be the unit identifier; the rest cannot be a response.", call. = FALSE)
    }
    if (any(id)) {
      rn <- as.character(y[[which(id)]])
      y <- y[, !id, drop = FALSE]
      rownames(y) <- rn
    }
    rn <- rownames(y)
    y <- as.matrix(y)
    rownames(y) <- rn
  }
  if (!is.matrix(y)) {
    stop("the response must be a matrix, a data frame or a vector, got ", class(y)[1L], ".",
         call. = FALSE)
  }
  if (is.null(rownames(y))) {
    rownames(y) <- as.character(seq_len(nrow(y)))
  }
  if (is.null(colnames(y))) {
    colnames(y) <- if (ncol(y) == 1L) "y" else paste0("v", seq_len(ncol(y)))
  }
  storage.mode(y) <- "double"
  if (anyNA(y)) {
    stop("the response holds missing values. Fill or drop them before fitting.", call. = FALSE)
  }
  y
}

#' Case weights that balance a rare response
#'
#' The weight every learner that ships fits a presence-absence response under: each presence of a
#' response weighs the ratio of absences to presences among the fitting units, capped, and each
#' absence weighs one. A response with a presence in one target of a hundred is otherwise fitted
#' away by any learner that minimises a mean loss, and the encoders, the penalised fit, the forest
#' and the forward search would each have to decide that for themselves.
#'
#' The ratio is read off the units the model is fitted on and the weight applies to every unit
#' handed in. An encoder holds part of its units back as an inner validation set and reads the
#' loss it stops on under the same weights, so the loss that stops the fit is the loss the fit
#' minimises; a unit held back that way is not in the count the ratio is made from.
#'
#' The weights are the response head's: the shipped presence-absence head carries this function
#' as its `weights`, and a head registered with `weights = function(y, fitting)
#' positive_weights(y, cap = 20, fitting = fitting)` weights every learner by that cap instead. A
#' head without `weights` is fitted unweighted.
#'
#' @param y The response matrix, `[unit, variable]`, 0/1.
#' @param cap Ceiling on the weight a presence is given, at least one.
#' @param fitting A logical vector over the rows of `y`, `TRUE` for the units the model is fitted
#'   on, or `NULL` for all of them. The ratio is read off those rows alone.
#'
#' @return A numeric `[unit, variable]` matrix of case weights, one per cell of `y`.
#'
#' @examples
#' y <- cbind(rare = c(1, 0, 0, 0, 0, 0), common = c(1, 1, 1, 0, 0, 0))
#' positive_weights(y)
#' positive_weights(y, fitting = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
#'
#' @export
positive_weights <- function(y, cap = 50, fitting = NULL) {
  y <- .as_response(y)
  if (!is.numeric(cap) || length(cap) != 1L || is.na(cap) || cap < 1) {
    stop("`cap` is a single number of at least 1, got ", .describe(cap), ".", call. = FALSE)
  }
  counted <- .fitting_rows(fitting, nrow(y))
  pos <- colSums(y[counted, , drop = FALSE] == 1)
  neg <- colSums(y[counted, , drop = FALSE] == 0)
  w <- ifelse(pos > 0, pmin(pmax(neg / pmax(pos, 1), 1), cap), 1)
  out <- matrix(1, nrow(y), ncol(y), dimnames = dimnames(y))
  present <- y == 1
  out[present] <- rep(w, each = nrow(y))[present]
  out
}

.presence_absence <- list(
  prepare = function(y) {
    y <- .as_response(y)
    if (!all(y %in% c(0, 1))) {
      stop("a presence-absence response must be 0/1 or logical.", call. = FALSE)
    }
    y
  },
  activation = "sigmoid",
  loss = "binary_cross_entropy",
  metric = "roc_auc",
  weights = function(y, fitting) positive_weights(y, fitting = fitting),
  cells = function(y, folds) scorable_cells(y, folds)
)

# The rows a head reads its counts off: every row where `fitting` is NULL, else the rows it marks.
.fitting_rows <- function(fitting, n) {
  if (is.null(fitting)) {
    return(rep(TRUE, n))
  }
  if (!is.logical(fitting) || length(fitting) != n || anyNA(fitting)) {
    stop("`fitting` is a logical vector with one entry per row of the response, got ",
         .describe(fitting), ".", call. = FALSE)
  }
  fitting
}
