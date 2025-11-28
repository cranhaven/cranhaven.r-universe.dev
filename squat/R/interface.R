#' QTS Differentiation
#'
#' This function computes the first derivative of quaternion time series with
#' respect to time.
#'
#' @param x An object of class [qts] or [qts_sample].
#'
#' @return An object of the same class as the input argument `x` in which
#'   quaternions measure the rotation to be applied to transform attitude at
#'   previous time point to attitude at current time point.
#'
#' @export
#' @examples
#' differentiate(vespa64$igp[[1]])
#' differentiate(vespa64$igp)
differentiate <- function(x) {
  UseMethod("differentiate")
}

#' @export
#' @rdname differentiate
differentiate.qts <- function(x) {
  derivative_qts(qts = x)
}

#' @export
#' @rdname differentiate
differentiate.qts_sample <- function(x) {
  res <- lapply(x, differentiate)
  as_qts_sample(res)
}

#' QTS Straightening
#'
#' This function straightens QTS so that the last point equals the first point.
#'
#' @param x An object of class [qts] or [qts_sample].
#'
#' @return An object of the same class as the input argument `x` storing the
#'   straightened QTS.
#'
#' @export
#' @examples
#' straighten(vespa64$igp[[1]])
#' straighten(vespa64$igp)
straighten <- function(x) {
  UseMethod("straighten")
}

#' @export
#' @rdname straighten
straighten.qts <- function(x) {
  straighten_qts(qts = x)
}

#' @export
#' @rdname straighten
straighten.qts_sample <- function(x) {
  res <- lapply(x, straighten)
  as_qts_sample(res)
}

#' QTS Logarithm
#'
#' This function computes the logarithm of quaternion time series as the time
#' series of the quaternion logarithms.
#'
#' @param x An object of class [qts] or [qts_sample].
#' @param ... Extra arguments to be passed on to next methods.
#'
#' @return An object of the same class as the input argument `x` in which
#'   quaternions have been replaced by their logarithm.
#'
#' @name log
#' @examples
#' log(vespa64$igp[[1]])
#' log(vespa64$igp)
NULL

#' @export
#' @rdname log
log.qts <- function(x, ...) {
  log_qts(x = x)
}

#' @export
#' @rdname log
log.qts_sample <- function(x, ...) {
  res <- lapply(x, log)
  as_qts_sample(res)
}

#' QTS Exponential
#'
#' This function computes the exponential of quaternion time series as the time
#' series of the quaternion exponentials.
#'
#' @param x An object of class [qts] or [qts_sample].
#' @param ... Extra arguments to be passed on to next methods.
#'
#' @return An object of the same class as the input argument `x` in which
#'   quaternions have been replaced by their exponential.
#'
#' @name exp
#' @examples
#' x <- log(vespa64$igp[[1]])
#' exp(x)
#' y <- log(vespa64$igp)
#' exp(y)
NULL

#' @export
#' @rdname exp
exp.qts <- function(x, ...) {
  exp_qts(x = x)
}

#' @export
#' @rdname exp
exp.qts_sample <- function(x, ...) {
  res <- lapply(x, exp)
  as_qts_sample(res)
}

#' QTS Reorientation
#'
#' This function reorients the quaternions in a QTS for representing attitude
#' with respect to the orientation of the sensor at the first time point.
#'
#' @param x An object of class [qts] or [qts_sample].
#' @param disable_normalization A boolean specifying whether quaternion
#'   normalization should be disabled. Defaults to `FALSE`.
#'
#' @return An object of the same class as the input argument `x` in which
#'   quaternions measure attitude with respect to the orientation of the sensor
#'   at the first time point.
#'
#' @export
#' @examples
#' reorient(vespa64$igp[[1]])
#' reorient(vespa64$igp)
reorient <- function(x, disable_normalization = FALSE) {
  UseMethod("reorient")
}

#' @export
#' @rdname reorient
reorient.qts <- function(x, disable_normalization = FALSE) {
  reorient_qts(x = x, disable_normalization = disable_normalization)
}

#' @export
#' @rdname reorient
reorient.qts_sample <- function(x, disable_normalization = FALSE) {
  res <- lapply(x, \(.x) reorient(.x, disable_normalization = disable_normalization))
  as_qts_sample(res)
}

#' QTS Normalization
#'
#' This function ensures that all quaternions in the time series are unit
#' quaternions.
#'
#' @param x An object of class [qts] or [qts_sample].
#'
#' @return An object of the same class as the input argument `x` in which
#'   quaternions are unit quaternions.
#'
#' @export
#' @examples
#' normalize(vespa64$igp[[1]])
#' normalize(vespa64$igp)
normalize <- function(x) {
  UseMethod("normalize")
}

#' @export
#' @rdname normalize
normalize.qts <- function(x) {
  normalize_qts(x = x)
}

#' @export
#' @rdname normalize
normalize.qts_sample <- function(x) {
  res <- lapply(x, normalize)
  as_qts_sample(res)
}

#' QTS Resampling
#'
#' This function performs uniform resampling using SLERP.
#'
#' @param x An object of class [qts] or [qts_sample].
#' @param tmin A numeric value specifying the lower bound of the time interval
#'   over which uniform resampling should take place. It must satisfy `tmin >=
#'   min(qts$time)`. Defaults to `NA` in which case it is set to
#'   `min(qts$time)`.
#' @param tmax A numeric value specifying the upper bound of the time interval
#'   over which uniform resampling should take place. It must satisfy `tmax <=
#'   max(qts$time)`. Defaults to `NA` in which case it is set to
#'   `max(qts$time)`.
#' @param nout An integer specifying the size of the uniform grid for time
#'   resampling. Defaults to `0L` in which case it uses the same grid size as
#'   the input QTS.
#' @param disable_normalization A boolean specifying whether quaternion
#'   normalization should be disabled. Defaults to `FALSE` in which case the
#'   function makes sure that quaternions are normalized prior to performing
#'   SLERP interpolation.
#'
#' @return An object of the same class as the input argument `x` in which
#'   quaternions are uniformly sampled in the range `[tmin, tmax]`.
#'
#' @export
#' @examples
#' resample(vespa64$igp[[1]])
#' resample(vespa64$igp)
resample <- function(x,
                     tmin = NA, tmax = NA, nout = 0L,
                     disable_normalization = FALSE) {
  UseMethod("resample")
}

#' @export
#' @rdname resample
resample.qts <- function(x,
                     tmin = NA, tmax = NA, nout = 0L,
                     disable_normalization = FALSE) {
  resample_qts(
    x = x,
    tmin = tmin,
    tmax = tmax,
    nout = nout,
    disable_normalization = disable_normalization
  )
}

#' @export
#' @rdname resample
resample.qts_sample <- function(x,
                                tmin = NA, tmax = NA, nout = 0L,
                                disable_normalization = FALSE) {
  res <- lapply(x, \(.x) {
    resample(
      x = .x,
      tmin = tmin,
      tmax = tmax,
      nout = nout,
      disable_normalization = disable_normalization
    )
  })
  as_qts_sample(res)
}

#' QTS Smoothing via SLERP Interpolation
#'
#' This function performs a smoothing of a QTS by SLERP interpolation.
#'
#' @param x An object of class [qts] or [qts_sample].
#' @inheritParams stats::smooth
#' @param alpha A numeric value in `[0,1]` specifying the amount of smoothing.
#'   The closer to one, the smoother the resulting QTS. Defaults to `0.5`.
#' @param ... Extra arguments passed on to next methods.
#'
#' @return An object of the same class as the input argument `x` which is a
#'   smooth version of the input QTS.
#'
#' @export
#' @examples
#' smooth(vespa64$igp[[1]])
#' smooth(vespa64$igp)
smooth <- function(x, ...) {
  UseMethod("smooth")
}

#' @export
#' @rdname smooth
smooth.default <- function(x,
                           kind = c("3RS3R", "3RSS", "3RSR", "3R", "3", "S"),
                           twiceit = FALSE,
                           endrule = c("Tukey", "copy"),
                           do.ends = FALSE,
                           ...) {
  stats::smooth(x = x, kind = kind, twiceit = twiceit, endrule = endrule, do.ends = do.ends)
}

#' @export
#' @rdname smooth
smooth.qts <- function(x, alpha = 0.5, ...) {
  smooth_qts(x = x, alpha = alpha)
}

#' @export
#' @rdname smooth
smooth.qts_sample <- function(x, alpha = 0.5, ...) {
  res <- lapply(x, \(.x) smooth(.x, alpha = alpha))
  as_qts_sample(res)
}

#' QTS Hemispherization
#'
#' This function ensures that there are no discontinuities in QTS due to
#' quaternion flips since two unit quaternions q and -q encode the same
#' rotation.
#'
#' @param x An object of class [`qts`] or [`qts_sample`].
#'
#' @return An object of the same class as the input argument `x` with no
#'   quaternion flip discontinuities.
#'
#' @export
#' @examples
#' hemispherize(vespa64$igp[[1]])
#' hemispherize(vespa64$igp)
hemispherize <- function(x) {
  UseMethod("hemispherize")
}

#' @export
#' @rdname hemispherize
hemispherize.qts <- function(x) {
  hemispherize_qts(x = x)
}

#' @export
#' @rdname hemispherize
hemispherize.qts_sample <- function(x) {
  res <- lapply(x, hemispherize)
  as_qts_sample(res)
}

#' QTS Moving Average
#'
#' This function performs QTS smoothing via moving average.
#'
#' @param x An object of class [`qts`] or [`qts_sample`].
#' @param window_size An integer value specifying the size of the sliding window
#'   used to compute the median value. Defaults to `0L`.
#'
#' @return An object of the same class as the input argument `x` storing the
#'   smoothed QTS.
#'
#' @export
#' @examples
#' moving_average(vespa64$igp[[1]], window_size = 5)
#' moving_average(vespa64$igp, window_size = 5)
moving_average <- function(x, window_size = 0) {
  UseMethod("moving_average")
}

#' @export
#' @rdname moving_average
moving_average.qts <- function(x, window_size = 0) {
  moving_average_qts(x = x, window_size = window_size)
}

#' @export
#' @rdname moving_average
moving_average.qts_sample <- function(x, window_size = 0) {
  res <- lapply(x, \(.x) moving_average(.x, window_size = window_size))
  as_qts_sample(res)
}
