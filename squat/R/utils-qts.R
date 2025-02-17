format_quaternion_component <- function(x, digits = 5) {
  tibble::num(
    x = round(x, digits = digits),
    digits = digits,
    notation = "dec"
  )
}

derivative_qts <- function(qts) {
  qts <- derivative_qts_impl(qts)
  as_qts(qts[-1, ])
}

straighten_qts <- function(qts) {
  P <- nrow(qts)
  time_values <- qts$time
  t1 <- time_values[1]
  tP <- time_values[P]
  qts <- log(qts)
  for (i in 3:5) {
    y1 <- qts[[i]][1]
    yP <- qts[[i]][P]
    a <- (yP - y1) / (tP - t1)
    qts[[i]] <- qts[[i]] - a * (time_values - t1)
  }
  qts$w <- rep(0, P)
  exp(qts)
}

log_qts <- function(x) {
  x <- log_qts_impl(x)
  as_qts(x)
}

exp_qts <- function(x) {
  x <- exp_qts_impl(x)
  as_qts(x)
}

reorient_qts <- function(x, disable_normalization = FALSE) {
  if (!disable_normalization) x <- normalize_qts(x)
  x <- reorient_qts_impl(x)
  as_qts(x)
}

normalize_qts <- function(x) {
  x <- normalize_qts_impl(x)
  as_qts(x)
}

resample_qts <- function(x,
                         tmin = NA, tmax = NA, nout = 0L,
                         disable_normalization = FALSE) {
  if (!disable_normalization)
    x <- normalize_qts(x)
  x <- resample_qts_impl(x, tmin, tmax, nout)
  as_qts(x)
}

smooth_qts <- function(x, alpha = 0.5) {
  x <- smooth_qts_impl(x, alpha)
  as_qts(x)
}

hemispherize_qts <- function(x) {
  x <- hemispherize_qts_impl(x)
  as_qts(x)
}

moving_average_qts <- function(x, window_size = 0) {
  x <- moving_average_qts_impl(x, window_size = window_size)
  as_qts(x)
}
