#' Defunct functions in package i2extras
#'
#' The functions or variables listed here are now defunct, i.e. no longer
#' available.
#'
#' * The `fit_model()` generic and associated functions were removed in version
#'   0.2.0.
#'
#' @name i2extras-defunct
#' @keywords internal
NULL

#' @export
#' @rdname i2extras-defunct
#' @keywords internal
fit_model <- function(x, model, ...) {
    .Defunct(
        package = "i2extras",
        msg = "As of i2extras version 0.2.0, the 'fit_model' generic is defunct."
    )
}
