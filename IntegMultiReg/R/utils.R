## Internal helpers.

#' @keywords internal
#' @noRd
.imr_abort <- function(message) {
  stop(message, call. = FALSE)
}

#' @keywords internal
#' @noRd
.imr_warn <- function(message) {
  warning(message, call. = FALSE)
}

#' @keywords internal
#' @noRd
.imr_is_integerish <- function(x) {
  is.numeric(x) && all(is.finite(x)) && all(x == as.integer(x))
}

#' @keywords internal
#' @noRd
.imr_check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .imr_abort(sprintf("`%s` must be TRUE or FALSE.", arg))
  }
  invisible(x)
}

#' @keywords internal
#' @noRd
.imr_check_integer_scalar <- function(x, arg, min = -Inf, max = Inf) {
  if (length(x) != 1L || !.imr_is_integerish(x) || x < min || x > max) {
    range <- if (is.finite(min) && is.finite(max)) {
      sprintf(" between %s and %s", min, max)
    } else if (is.finite(min)) {
      sprintf(" greater than or equal to %s", min)
    } else if (is.finite(max)) {
      sprintf(" less than or equal to %s", max)
    } else {
      ""
    }
    .imr_abort(sprintf("`%s` must be a whole number%s.", arg, range))
  }
  as.integer(x)
}

#' @keywords internal
#' @noRd
.imr_check_integer_vector <- function(x, arg, length, min = -Inf) {
  if (!is.numeric(x) || length(x) != length || !.imr_is_integerish(x) ||
      any(x < min)) {
    .imr_abort(sprintf(
      "`%s` must be a numeric vector of %d whole number(s).",
      arg, length
    ))
  }
  as.integer(x)
}

#' @keywords internal
#' @noRd
.imr_check_numeric_vector <- function(x, arg, length = NULL,
                                      positive = FALSE,
                                      nonnegative = FALSE) {
  if (!is.numeric(x) || any(!is.finite(x)) ||
      (!is.null(length) && length(x) != length) ||
      (positive && any(x <= 0)) ||
      (nonnegative && any(x < 0))) {
    suffix <- if (!is.null(length)) {
      sprintf(" of length %d", length)
    } else {
      ""
    }
    bound <- if (positive) {
      " positive"
    } else if (nonnegative) {
      " non-negative"
    } else {
      ""
    }
    .imr_abort(sprintf("`%s` must be a finite%s numeric vector%s.",
                       arg, bound, suffix))
  }
  x
}

#' @keywords internal
#' @noRd
.imr_check_id_frame <- function(x, arg, require_rows = TRUE,
                                require_features = TRUE) {
  if (!is.data.frame(x)) {
    .imr_abort(sprintf("`%s` must be a data frame.", arg))
  }
  if (!identical(names(x)[1], "id")) {
    .imr_abort(sprintf("`%s` must have `id` as its first column.", arg))
  }
  if (require_rows && nrow(x) == 0L) {
    .imr_abort(sprintf("`%s` must contain at least one row.", arg))
  }
  if (require_features && ncol(x) < 2L) {
    .imr_abort(sprintf("`%s` must contain at least one non-id column.", arg))
  }
  if (anyNA(x$id)) {
    .imr_abort(sprintf("`%s$id` must not contain missing values.", arg))
  }
  if (anyDuplicated(x$id)) {
    .imr_abort(sprintf("`%s$id` must contain unique subject identifiers.", arg))
  }
  invisible(x)
}

#' @keywords internal
#' @noRd
.imr_check_numeric_columns <- function(x, arg, columns = names(x)[-1]) {
  if (length(columns) == 0L) {
    return(invisible(x))
  }
  bad_type <- columns[!vapply(x[columns], is.numeric, logical(1))]
  if (length(bad_type) > 0L) {
    .imr_abort(sprintf(
      "All non-id columns in `%s` must be numeric; problem column: `%s`.",
      arg, bad_type[1]
    ))
  }
  vals <- as.matrix(x[columns])
  if (any(!is.finite(vals))) {
    .imr_abort(sprintf("All non-id values in `%s` must be finite.", arg))
  }
  invisible(x)
}

#' @keywords internal
#' @noRd
.imr_match_rows <- function(x, ids, arg) {
  rows <- match(ids, x$id)
  if (anyNA(rows)) {
    .imr_abort(sprintf("`%s` is missing rows for one or more subject ids.", arg))
  }
  x[rows, , drop = FALSE]
}

#' @keywords internal
#' @noRd
.imr_empty_predictions <- function(model_names) {
  out <- lapply(model_names, function(x) {
    data.frame(id = character(0), predict = numeric(0))
  })
  names(out) <- paste0("model:", model_names)
  out
}

## Evaluate `code` while optionally discarding everything it writes to the
## console (including Rprintf output from the compiled sampler).  When
## `verbose` is TRUE the output is shown; otherwise it is captured and dropped.
#' @keywords internal
#' @noRd
.quietly <- function(verbose, code) {
  if (isTRUE(verbose)) {
    return(eval.parent(substitute(code)))
  }
  expr <- substitute(code)
  env <- parent.frame()
  utils::capture.output(value <- eval(expr, env))
  value
}
