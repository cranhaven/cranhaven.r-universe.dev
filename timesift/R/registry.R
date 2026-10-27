# One registry mechanism, used by the three things the package is meant to be extended with:
# learners, response heads and metrics. Adding any of them is a registration, never an edit to the
# code that fits or scores, so the fitting path has no list of names in it.

.new_registry <- function(what) {
  entries <- new.env(parent = emptyenv())
  list(
    set = function(name, value, overwrite = FALSE) {
      if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
        stop("a ", what, " needs a single non-empty name.", call. = FALSE)
      }
      if (!overwrite && exists(name, envir = entries, inherits = FALSE)) {
        stop(what, " \"", name, "\" is already registered. ",
             "Pass `overwrite = TRUE` to replace it.", call. = FALSE)
      }
      assign(name, value, envir = entries)
      invisible(value)
    },
    get = function(name) {
      if (!exists(name, envir = entries, inherits = FALSE)) {
        stop("unknown ", what, " \"", name, "\". Registered: ",
             paste(sort(ls(entries), method = "radix"), collapse = ", "), ".", call. = FALSE)
      }
      get(name, envir = entries, inherits = FALSE)
    },
    has = function(name) exists(name, envir = entries, inherits = FALSE),
    remove = function(name) {
      if (exists(name, envir = entries, inherits = FALSE)) rm(list = name, envir = entries)
      invisible(NULL)
    },
    # C collation, so a registry lists the same order on every machine and beside Python's.
    names = function() sort(ls(entries), method = "radix")
  )
}

.learners_reg <- .new_registry("learner")
.responses_reg <- .new_registry("response")
.metrics_reg <- .new_registry("metric")

#' Register a metric
#'
#' A metric scores one held-out cell: the observed response of the units in a fold and a model's
#' predictions for them. Registering one makes it available to [grain_ladder()] by name, with no
#' change to the fitting code.
#'
#' @param name Name the metric is asked for by.
#' @param fn A function of `(y, p)`, the observed values and the predictions for one cell, both
#'   vectors of the same length, returning one number or `NA` where the cell defines none.
#' @param overwrite Replace an existing registration.
#'
#' @return The registered function, invisibly.
#'
#' @examples
#' register_metric("hit_rate", function(y, p) mean((p >= 0.5) == (y == 1)), overwrite = TRUE)
#' "hit_rate" %in% metrics()
#'
#' @export
register_metric <- function(name, fn, overwrite = FALSE) {
  if (!is.function(fn)) {
    stop("a metric is a function of (y, p).", call. = FALSE)
  }
  .metrics_reg$set(name, fn, overwrite)
}

#' @rdname register_metric
#' @export
metrics <- function() .metrics_reg$names()

# A metric reaches a run either as a registered name or as a function of (y, p), and everything
# that rescores afterwards -- the ensemble row of a report, an occlusion profile -- needs the
# function rather than a name to look up again. Both travel with the fit: the function is what
# scores, the name is what the report prints. A function has no name to print, and reads as
# `<function>` on both sides rather than as whatever the language calls an anonymous one.
.as_metric <- function(metric, default = NULL) {
  metric <- metric %||% default
  if (is.function(metric)) {
    return(list(fn = metric, name = "<function>"))
  }
  if (!is.character(metric) || length(metric) != 1L) {
    stop("`metric` is the name of a registered metric or a function of (y, p), got ",
         class(metric)[1L], ".", call. = FALSE)
  }
  list(fn = .metrics_reg$get(metric), name = metric)
}

#' Register a response head
#'
#' A response head says what the values being predicted are, how they reach a learner, and which
#' cells of the (variable, fold) grid a score is defined on. Presence-absence with a joint
#' multi-label head is what ships; an abundance or phenology response is a registration rather than
#' a second fitting path.
#'
#' @param name Name the response is asked for by.
#' @param spec A list with elements `prepare(y)`, returning the numeric matrix a learner is fitted
#'   on; `activation`, the name of the output transform (`"sigmoid"` or `"identity"`); `loss`, the
#'   name of the training objective (`"binary_cross_entropy"` or `"squared_error"`); `metric`, the
#'   default metric name; and `cells(y, folds)`, returning the mask of scorable cells. Every
#'   learner that ships reads `loss` and `activation` from here: the encoders train under the loss
#'   and predict through the activation, and the learners fitting one model per response take the
#'   family the loss names, logistic or Gaussian. The combiner minimises the same loss. An
#'   optional `weights(y, fitting)` returns a `[unit, variable]` matrix of case weights every
#'   learner fits under, with `fitting` a logical vector marking the rows the model is fitted on:
#'   whatever the head reads off the response, it reads off those rows, and it weights every row,
#'   so an encoder's inner validation loss is weighted as its fit is. The shipped head's is
#'   [positive_weights()], and a head without one fits unweighted.
#' @param overwrite Replace an existing registration.
#'
#' @return The registered specification, invisibly.
#'
#' @examples
#' responses()
#'
#' @export
register_response <- function(name, spec, overwrite = FALSE) {
  need <- c("prepare", "activation", "loss", "metric", "cells")
  missing <- setdiff(need, names(spec))
  if (length(missing)) {
    stop("a response specification needs ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  .responses_reg$set(name, spec, overwrite)
}

#' @rdname register_response
#' @export
responses <- function() .responses_reg$names()
