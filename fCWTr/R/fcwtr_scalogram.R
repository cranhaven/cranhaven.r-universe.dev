new_fcwtr_scalogram <- function(matrix, sample_freq, freq_begin, freq_end,
                                sigma, remove_coi) {
  stopifnot(is.matrix(matrix))

  if (remove_coi) {
    dim_t <- dim(matrix)[[1]] # Time dimension
    dim_f <- dim(matrix)[[2]] # Frequency dimension

    # The standard deviation Σ of a the Gauß like wave packet at frequency f
    # and sampling frequency f_s with given σ is given by
    # Σ = σ / sqrt(2) f_s / f
    # we choose 4Σ to define the support of a wave packet
    # (and so boundary effects are expected to occur until 2Σ)
    coi_pred <- \(f, t) t * f < sqrt(2) * sigma

    # express in dimensionless quantities
    t <- rep(1:dim_t, times = dim_f)
    f <-
      rep(
        seq(freq_end, freq_begin, length.out = dim_f) / sample_freq,
        each = dim_t
      )

    # check if points are inside / outside hyperbolic cone
    matrix[coi_pred(f, t) | coi_pred(f, dim_t - t)] <- NA_real_
  }

  obj <-
    structure(
      matrix,
      class = c("fcwtr_scalogram", class(matrix)),
      sample_freq = sample_freq,
      freq_begin = freq_begin,
      freq_end = freq_end,
      sigma = sigma
    )

  dimnames(obj) <-
    list(
      seq_along(matrix[, 1]) - 1,
      seq(freq_end, freq_begin, length.out = dim(matrix)[[2]])
    )

  obj
}

# perform aggregation, if possible.
# if it's not possible, be identity
agg <- function(x, n) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  poolsize <- floor(dim(x)[[1]] / n)

  if (poolsize <= 1) {
    # do nothing in case we cannot aggregate
    return(x)
  }

  x_new <- x[1:(poolsize * n), ]
  dim(x_new) <- c(poolsize, n, dim(x_new)[[2]])
  x_new <- colMeans(x_new, dims = 1, na.rm = TRUE)

  # replace NaN by NA
  x_new[is.nan(x_new)] <- NA_real_

  new_fcwtr_scalogram(
    x_new,
    attr(x, "sample_freq") / poolsize,
    attr(x, "freq_begin"), attr(x, "freq_end"),
    attr(x, "sigma"),
    remove_coi = FALSE
  )
}

tbind <- function(..., deparse.level = 1) {
  args <- list(...)
  stopifnot(length(args) >= 1)
  lapply(args, \(arg) stopifnot(inherits(arg, "fcwtr_scalogram")))

  # check if attributes are identical, otherwise combination
  # does not make sense
  if (length(unique(lapply(args, \(arg) attr(arg, "sample_freq")))) > 1) {
    stop("Sampling frequencies need to be identical.")
  }
  if (length(unique(lapply(args, \(arg) attr(arg, "freq_begin")))) > 1) {
    stop("Frequency ranges need to be identical.")
  }
  if (length(unique(lapply(args, \(arg) attr(arg, "freq_end")))) > 1) {
    stop("Frequency ranges need to be identical.")
  }
  if (length(unique(lapply(args, \(arg) attr(arg, "sigma")))) > 1) {
    stop("Sigma parameter needs to be identical.")
  }

  x_new <- do.call(rbind, c(lapply(args, unclass), list(deparse.level = deparse.level)))

  new_fcwtr_scalogram(
    x_new,
    attr(args[[1]], "sample_freq"),
    attr(args[[1]], "freq_begin"), attr(args[[1]], "freq_end"),
    attr(args[[1]], "sigma"),
    remove_coi = FALSE
  )
}

rm_na_time_slices <- function(x) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  rows_to_remove <- unique(which(is.na(x), arr.ind = TRUE)[, 1])

  new_fcwtr_scalogram(
    x[-rows_to_remove, ],
    attr(x, "sample_freq"),
    attr(x, "freq_begin"), attr(x, "freq_end"),
    attr(x, "sigma"),
    remove_coi = FALSE
  )
}

#' Coerce the scalogram matrix to a data frame
#'
#' Internally, the scalogram resulting from [fcwt()] is represented by
#' a numeric matrix. This method coerces this matrix into a reasonable
#' data frame. Note that this conversion has a significant run time cost.
#'
#' @param x
#'  An object resulting from [fcwt()].
#'
#' @return
#'  A [data.frame()] object representing the scalogram data with four columns:
#' \describe{
#'   \item{time_ind}{An integer index uniquely identifying time slices.}
#'   \item{time}{The time difference to the first time slice in physical units.
#'               The time unit is the inverse of the frequency unit chosen by the user
#'               for the `sample_freq` argument of [fcwt()].}
#'   \item{freq}{The frequency in the same units as the `sample_freq` argument
#'               of [fcwt()].}
#'   \item{value}{The fCWT result for the particular time-frequency combination.}
#' }
#'
#' @inheritParams base::as.data.frame
#' @examples
#' fcwt(
#'   sin((1:5000) * 2 * pi * 440 / 44100),
#'   sample_freq = 44100,
#'   n_freqs = 10
#' ) |>
#' as.data.frame() |>
#' head()
#'
#' @export
as.data.frame.fcwtr_scalogram <- function(x, ...) {
  df <- as.data.frame(as.table(x), stringsAsFactors = FALSE)
  names(df) <- c("time_ind", "freq", "value")
  df[["time_ind"]] <- as.integer(df[["time_ind"]])
  df[["freq"]] <- as.numeric(df[["freq"]])

  df[["time"]] <- df[["time_ind"]] / attr(x, "sample_freq")

  df[, c("time_ind", "time", "freq", "value")]
}

#' Scalogram plotting
#'
#' Plots the scalogram resulting from [fcwt()].
#' Requires [ggplot2](https://ggplot2.tidyverse.org/).
#'
#' @param x
#'  An object resulting from [fcwt()].
#'
#' @inheritParams autoplot.fcwtr_scalogram
#' @return No return value, called for side effects.
#'
#' @importFrom graphics plot
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' ts_sin_440 <- sin((1:4410) * 2 * pi * 440 / 44100)
#'
#' res <-
#'   fcwt(
#'     ts_sin_440,
#'     sample_freq = 44100,
#'     freq_begin = 50,
#'     freq_end = 1000,
#'     n_freqs = 10,
#'     sigma = 5
#'   )
#'
#' plot(res)
plot.fcwtr_scalogram <- function(x, n = 1000, ...) {
  print(autoplot.fcwtr_scalogram(x, n, ...))
}

#' Create a ggplot object resembling a scalogram
#'
#' @param n
#'  The plotting function reduces the time resolution by averaging
#'  to generate a reasonable graphics format. `n` is the number of time
#'  steps that are plotted. Defaults to `n = 1000`.
#' @param ...
#'  other arguments passed to specific methods
#' @return
#'  A ggplot object.
#'
#' @keywords internal
autoplot.fcwtr_scalogram <- function(object, n = 1000, ...) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  stopifnot(requireNamespace("viridis", quietly = TRUE))
  stopifnot(requireNamespace("rlang", quietly = TRUE))

  .data <- rlang::.data
  ggplot <- ggplot2::ggplot
  aes <- ggplot2::aes
  geom_raster <- ggplot2::geom_raster

  # first aggregate the time series,
  # since we cannot really see too much resolution anyways
  as.data.frame(agg(object, n)) |>
    ggplot(aes(x = .data$time, y = .data$freq, fill = .data$value)) +
    geom_raster() +
    viridis::scale_fill_viridis(discrete = FALSE) +
    ggplot2::scale_y_continuous(name = "Frequency") +
    ggplot2::scale_x_time(name = "Time") +
    ggplot2::theme_minimal()
}
