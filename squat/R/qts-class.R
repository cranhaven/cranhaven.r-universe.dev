#' QTS Class
#'
#' A collection of functions that implements the QTS class. It currently
#' provides the [`as_qts()`] function for QTS coercion of [`tibble::tibble`]s
#' and the [`is_qts()`] function for checking if an object is a QTS.
#'
#' A quaternion time series (QTS) is stored as a [`tibble::tibble`] with 5
#' columns:
#' - `time`: A first column specifying the time points at which quaternions were
#' collected;
#' - `w`: A second column specifying the first coordinate of the collected
#' quaternions;
#' - `x`: A third column specifying the second coordinate of the collected
#' quaternions;
#' - `y`: A fourth column specifying the third coordinate of the collected
#' quaternions;
#' - `z`: A fifth column specifying the fourth coordinate of the collected
#' quaternions.
#'
#' @param x A [`tibble::tibble`] with columns `time`, `w`, `x`, `y` and `z`.
#' @param digits An integer value specifying the number of digits to keep for
#'   printing. Defaults to `5L`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return An object of class [qts].
#' @name qts
#'
#' @examples
#' qts1 <- vespa64$igp[[1]]
#' qts2 <- as_qts(qts1)
#' is_qts(qts1)
#' is_qts(qts2)
NULL

#' @export
#' @rdname qts
as_qts <- function(x) {
  if (is_qts(x)) return(x)
  if (!tibble::is_tibble(x))
    cli::cli_abort("The input object should be of class {.cls tbl}.")
  if (!all(names(x) == c("time", "w", "x", "y", "z")))
    cli::cli_abort(
      "The input tibble should have exactly the 5 following columns in that order: {.code time}, {.code w}, {.code x}, {.code y} and {.code z}."
    )
  if (nrow(x) < 2L)
    cli::cli_abort(c(
      "The input tibble should have at least 2 rows.",
      "i" = "If you are analysing samples of rotations (without the time component), you should use the {.pkg rotations} package."
    ))
  class(x) <- c("qts", class(x))
  x
}

#' @export
#' @rdname qts
is_qts <- function(x) {
  "qts" %in% class(x)
}

#' @export
#' @rdname qts
format.qts <- function(x, digits = 5, ...) {
  x$w <- format_quaternion_component(x$w, digits = digits)
  x$x <- format_quaternion_component(x$x, digits = digits)
  x$y <- format_quaternion_component(x$y, digits = digits)
  x$z <- format_quaternion_component(x$z, digits = digits)
  NextMethod()
}

#' @export
#' @rdname qts
print.qts <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' QTS Centering and Standardization
#'
#' This function operates a centering of the QTS around the geometric mean of
#' its quaternions. This is effectively achieved by left-multiplying each
#' quaternion by the inverse of their geometric mean.
#'
#' @param x An object of class [`qts`].
#' @param standardize A boolean specifying whether to standardize the QTS in
#'   addition to centering it. Defaults to `FALSE`.
#' @param keep_summary_stats A boolean specifying whether the mean and standard
#'   deviation used for standardizing the data should be stored in the output
#'   object. Defaults to `FALSE` in which case only the centered
#'   \code{\link{qts}} is returned.
#'
#' @return If `keep_summary_stats = FALSE`, an object of class [`qts`] in which
#'   quaternions have been centered (and possibly standardized) around their
#'   geometric mean. If `keep_summary_stats = TRUE`, a list with three
#'   components:
#'   - `qts`: an object of class [`qts`] in which quaternions have been centered
#'   (and possibly standardized) around their geometric mean;
#' - `mean`: a numeric vector with the quaternion Fréchet mean;
#' - `sd`: a numeric value with the quaternion Fréchet standard deviation.
#'
#' @export
#' @examples
#' centring(vespa64$igp[[1]])
centring <- function(x, standardize = FALSE, keep_summary_stats = FALSE) {
  if (!is_qts(x))
    cli::cli_abort("The input argument {.arg x} should be of class {.cls qts}.")
  out <- centring_qts_impl(x, standardize = standardize)
  out$qts <- as_qts(out$qts)
  if (keep_summary_stats) return(out)
  out$qts
}

#' Plot for [`qts`] objects
#'
#' This function creates a visualization of a QTS and returns the corresponding
#' [ggplot2::ggplot] object which enable further customization of the plot.
#'
#' @param object An object of class [qts].
#' @param highlighted_points An integer vector specifying point indices to be
#'   highlighted. Defaults to `NULL`, in which case no point will be highlighted
#'   with respect to the others.
#' @param ... Further arguments to be passed on to next methods.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @importFrom ggplot2 autoplot .data
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' ggplot2::autoplot(vespa64$igp[[1]])
autoplot.qts <- function(object, highlighted_points = NULL, ...) {
  if (!is.null(highlighted_points)) {
    if (!all(highlighted_points %in% 1:nrow(object)))
      cli::cli_abort("The change point indices are out of bounds.")
    highlighted_points <- object$time[highlighted_points]
  }
  x <- tidyr::pivot_longer(object, cols = "w":"z")
  p <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = .data$time,
      y = .data$value
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(.data$name), ncol = 1, scales = "free") +
    ggplot2::theme_linedraw() +
    ggplot2::labs(
      title = "Quaternion Time Series",
      x = "Time",
      y = ""
    )

  if (!is.null(highlighted_points)) {
    p <- p +
      ggplot2::geom_point(
        data = subset(x, x$time %in% highlighted_points),
        color = "red"
      )
  }

  p
}

#' Plot for [`qts`] objects
#'
#' This function creates a visualization of a QTS **without** returning the plot
#' data as an object.
#'
#' @param x An object of class [qts].
#' @inheritParams autoplot.qts
#'
#' @return No return value, called for side effects.
#'
#' @importFrom graphics plot
#' @export
#' @examples
#' plot(vespa64$igp[[1]])
plot.qts <- function(x, highlighted_points = NULL, ...) {
  print(autoplot(x, highlighted_points = highlighted_points, ...))
}

#' Operator `+` for `qts` Objects
#'
#' This function implements the pointwise addition between two quaternion time
#' series.
#'
#' @param x An object of class [`qts`].
#' @param rhs Either an object of class [`qts`] or a numeric value.
#'
#' @return An object of class [`qts`] storing the addition of the two inputs.
#'
#' @export
#' @examples
#' vespa64$igp[[1]] + vespa64$igp[[2]]
"+.qts" <- function(x, rhs) {
  if (!is_qts(rhs)) {
    if (!is.numeric(rhs))
      cli::cli_abort(
        "The input argument {.arg rhs} should be of either of class {.cls qts} or of class {.cls numeric}."
      )
    if (length(rhs) != 1)
      cli::cli_abort(
        "When the input argument {.arg rhs} is numeric, it should be scalar."
      )
    out <- x
    out$w <- x$w + rhs
    out$x <- x$x + rhs
    out$y <- x$y + rhs
    out$z <- x$z + rhs
    return(as_qts(out))
  }

  if (!all.equal(x$time, rhs$time))
    cli::cli_abort("The time indices of the two QTS are not equal.")

  out <- x
  out$w <- x$w + rhs$w
  out$x <- x$x + rhs$x
  out$y <- x$y + rhs$y
  out$z <- x$z + rhs$z
  as_qts(out)
}

#' Operator `-` for `qts` Objects
#'
#' This function implements the pointwise subtraction between two quaternion
#' time series.
#'
#' @param x An object of class [`qts`].
#' @param rhs Either an object of class [`qts`] or a numeric value.
#'
#' @return An object of class [`qts`] storing the subtraction of the two
#'   inputs.
#'
#' @export
#' @examples
#' vespa64$igp[[1]] - vespa64$igp[[2]]
"-.qts" <- function(x, rhs) {
  if (!is_qts(rhs)) {
    if (!is.numeric(rhs))
      cli::cli_abort(
        "The input argument {.arg rhs} should be of either of class {.cls qts} or of class {.cls numeric}."
      )
    if (length(rhs) != 1)
      cli::cli_abort(
        "When the input argument {.arg rhs} is numeric, it should be scalar."
      )
    out <- x
    out$w <- x$w - rhs
    out$x <- x$x - rhs
    out$y <- x$y - rhs
    out$z <- x$z - rhs
    return(as_qts(out))
  }

  if (!all.equal(x$time, rhs$time))
    cli::cli_abort("The time indices of the two QTS are not equal.")

  out <- x
  out$w <- x$w - rhs$w
  out$x <- x$x - rhs$x
  out$y <- x$y - rhs$y
  out$z <- x$z - rhs$z
  as_qts(out)
}

#' Operator `*` for `qts` Objects
#'
#' This function implements the pointwise quaternion Hamilton multiplication
#' between two quaternion time series.
#'
#' @param x An object of class [`qts`].
#' @param rhs Either an object of class [`qts`] or a numeric value.
#'
#' @return An object of class [`qts`] storing the multiplication of the two
#'   inputs.
#'
#' @export
#' @examples
#' vespa64$igp[[1]] * vespa64$igp[[2]]
"*.qts" <- function(x, rhs) {
  if (!is_qts(rhs)) {
    if (!is.numeric(rhs))
      cli::cli_abort(
        "The input argument {.arg rhs} should be of either of class {.cls qts} or of class {.cls numeric}."
      )
    if (length(rhs) != 1)
      cli::cli_abort(
        "When the input argument {.arg rhs} is numeric, it should be scalar."
      )
    out <- x
    out$w <- x$w * rhs
    out$x <- x$x * rhs
    out$y <- x$y * rhs
    out$z <- x$z * rhs
    return(as_qts(out))
  }

  if (!all.equal(x$time, rhs$time))
    cli::cli_abort("The time indices of the two QTS are not equal.")

  out <- multiply_qts_impl(x, rhs)
  as_qts(out)
}

#' Inverse Operator for `qts` Objects
#'
#' This function implements the pointwise inverse of a quaternion time series.
#'
#' @param x An object of class [`qts`].
#'
#' @return An object of class [`qts`] storing the inverse of `x`.
#'
#' @export
#' @examples
#' inverse_qts(vespa64$igp[[1]])
inverse_qts <- function(x) {
  if (!is_qts(x))
    cli::cli_abort("The input argument {.arg x} should be of class {.cls qts}.")
  out <- inverse_qts_impl(x)
  as_qts(out)
}
