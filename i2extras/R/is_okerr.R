#' Error handling for incidence2_fit objects
#'
#' These functions are used to filter succesful model fits from those that
#' errored or gave warnings.
#'
#' @author Tim Taylor
#'
#' @param x The output of function `fit_curve()`.
#' @param ... Not currently used.
#' @param include_warnings Include results in output that triggered warnings but
#'   not errors.  Defaults to `FALSE`.
#'
#' @return
#'
#' * `is_ok()`: returns rows from an `<incidence2_fit>` object that did not
#'  error (and optionally produce a warning).
#'
#' * `is_error()`: returns rows from an `<incidence2_fit>` object that errored.
#'
#' * `is_warning()`: returns rows from an `<incidence2_fit>` object that
#'   produced warnings.
#'
#' @name is_okerr
NULL

#' @rdname is_okerr
#' @aliases is_ok
#' @export
is_ok <- function(x, ...) {
    UseMethod("is_ok")
}

#' @rdname is_okerr
#' @aliases is_ok.incidence2_fit
#' @export
is_ok.default <- function(x, ...) {
    not_implemented(x)
}


#' @rdname is_okerr
#' @aliases is_ok.incidence2_fit
#' @export
is_ok.incidence2_fit <- function(x, include_warnings = FALSE, ...) {
    error_vars <- attr(x, "error_vars")
    warning_vars <- attr(x, "warning_vars")

    if (!is.null(error_vars)) {
        e <- suppressMessages(
            lapply(
                x[error_vars],
                function(z) vapply(z, function(x) !is.null(x), logical(1))
            )
        )
        ok <- !do.call(`|`, e)
        idx <- !names(x) %in% error_vars
        attr(x, "error_vars") <- NULL
        x <- x[ok, idx]
    }

    if (!include_warnings) {
        if(!is.null(warning_vars)) {
            w <- suppressMessages(
                lapply(
                    x[warning_vars],
                    function(z) vapply(z, function(x) !is.null(x), logical(1))
                )
            )
            ok <- !do.call(`|`, w)
            idx <- !names(x) %in% warning_vars
            attr(x, "warning_vars") <- NULL
            x <- x[ok, idx]
        }
    }
    x
}


#' @rdname is_okerr
#' @aliases is_error
#' @export
is_error <- function(x, ...) {
    UseMethod("is_error")
}


#' @rdname is_okerr
#' @aliases is_error.default
#' @export
is_error.default <- function(x, ...) {
    not_implemented(x)
}


#' @rdname is_okerr
#' @aliases is_error.incidence2_fit
#' @export
is_error.incidence2_fit <- function(x, ...) {
    error_vars <- attr(x, "error_vars")
    if (!is.null(error_vars)) {
        e <- suppressMessages(
            lapply(
                x[error_vars],
                function(z) vapply(z, function(x) !is.null(x), logical(1))
            )
        )
        e <- do.call(`|`, e)
        x <- x[e, ]
    }
    x
}


#' @rdname is_okerr
#' @aliases is_warning
#' @export
is_warning <- function(x, ...) {
    UseMethod("is_warning")
}


#' @rdname is_okerr
#' @aliases is_warning.default
#' @export
is_warning.default <- function(x, ...) {
    not_implemented(x)
}


#' @rdname is_okerr
#' @aliases is_err.incidence2_fit
#' @export
is_warning.incidence2_fit <- function(x, ...) {
    warning_vars <- attr(x, "warning_vars")
    if (!is.null(warning_vars)) {
        w <- suppressMessages(
            lapply(
                x[warning_vars],
                function(z) vapply(z, function(x) !is.null(x), logical(1))
            )
        )
        w <- do.call(`|`, w)
        idx <- !names(x) %in% attr(x, "error_vars")
        x <- x[w, idx]
    }
    x
}
