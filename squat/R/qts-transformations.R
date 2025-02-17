#' QTS Transformation To Distance Time Series
#'
#' This function computes a real-valued time series reporting the pointwise
#' geodesic distance between the two input QTS at each time point.
#'
#' The function currently expects that the two input QTS are evaluated on the
#' same time grid.
#'
#' @param x An object of class [qts].
#' @param y An object of class [qts].
#'
#' @return A time series stored as a [tibble::tibble] with columns `time` and
#'   `distance` in which `distance` measures the angular distance between the
#'   quaternions of both input QTS at a given time point.
#'
#' @export
#' @examples
#' qts2dts(vespa64$igp[[1]], vespa64$igp[[2]])
qts2dts <- function(x, y) {
  if (!is_qts(x))
    cli::cli_abort("The input argument {.arg x} should be of class {.cls qts}.")
  if (!is_qts(y))
    cli::cli_abort("The input argument {.arg y} should be of class {.cls qts}.")
  if (!all(x$time == y$time))
    cli::cli_abort("The two input QTS should be evaluated on the same time grid.")
  qts2dts_impl(x, y)
}

#' QTS Transformation To Norm Time Series
#'
#' This function computes a univariate time series representing the norm of the
#' quaternions.
#'
#' @param x An object of class [qts].
#' @param disable_normalization A boolean specifying whether quaternion
#'   normalization should be disabled. Defaults to `FALSE`.
#'
#' @return A time series stored as a [tibble::tibble] with columns `time` and
#'   `norm` in which `norm` measures the angular distance between the current
#'   quaternion and the identity.
#'
#' @export
#' @examples
#' qts2nts(vespa64$igp[[1]])
qts2nts <- function(x, disable_normalization = FALSE) {
  if (!is_qts(x))
    cli::cli_abort("The input argument {.arg x} should be of class {.cls qts}.")
  qts2nts_impl(x, disable_normalization = disable_normalization)
}

#' QTS Transformation To Angle Time Series
#'
#' This function computes a univariate time series representing the angle
#' between the first and other attitudes.
#'
#' @param x An object of class [qts].
#' @param disable_normalization A boolean specifying whether quaternion
#'   normalization should be disabled. Defaults to `FALSE`.
#'
#' @return A time series stored as a [tibble::tibble] with columns `time` and
#'   `angle` in which `angle` measures the angle between the current rotation
#'   and the first one.
#'
#' @export
#' @examples
#' qts2ats(vespa64$igp[[1]])
qts2ats <- function(x, disable_normalization = FALSE) {
  if (!is_qts(x))
    cli::cli_abort("The input argument {.arg x} should be of class {.cls qts}.")
  out <- qts2ats_impl(x, disable_normalization = disable_normalization)
  out$angle[out$angle < .Machine$double.eps] <- 0
  out
}

#' QTS Transformation to Angular Velocity Time Series
#'
#' This function projects a quaternion time series into the space of angular
#' velocities.
#'
#' @param x An object of class [qts].
#' @param body_frame A boolean specifying whether the fixed frame with respect
#'   to which coordinates of the angular velocity should be computed is the body
#'   frame or the global frame. Defaults to `FALSE`.
#'
#' @return A time series stored as a [tibble::tibble] with columns `time`, `x`,
#'   `y` and `z` containing the angular velocity at each time point.
#'
#' @export
#' @examples
#' qts2avts(vespa64$igp[[1]])
qts2avts <- function(x, body_frame = FALSE) {
  if (!is_qts(x))
    cli::cli_abort("The input argument {.arg x} should be of class {.cls qts}.")
  qts2avts_impl(x, body_frame = body_frame)
}

#' QTS Transformation to Angle-Axis Time Series
#'
#' This function converts a quaternion time series into its angle-axis
#' representation.
#'
#' @param x An object of class [qts].
#'
#' @return A time series stored as a [tibble::tibble] with columns `time`,
#'   `angle`, `ux`, `uy` and `uz` containing the angle-axis representation of
#'   the input quaternions.
#'
#' @export
#' @examples
#' qts2aats(vespa64$igp[[1]])
qts2aats <- function(x) {
  if (!is_qts(x))
    cli::cli_abort("The input argument {.arg x} should be of class {.cls qts}.")
  out <- qts2aats_impl(x)
  names(out) <- c("time", "angle", "ux", "uy", "uz")
  out
}
