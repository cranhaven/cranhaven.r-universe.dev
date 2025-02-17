#' QTS Sample Class
#'
#' A collection of functions that implements the QTS sample class. It currently
#' provides the [`as_qts_sample()`] function for QTS sample coercion of lists of
#' [`qts`] objects, the [`is_qts_sample()`] function for checking if an object
#' is a QTS sample and the subset operator.
#'
#' A QTS sample is a collection of quaternion time series (QTS), each of which
#' is stored as a [`tibble::tibble`] with 5 columns:
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
#' @param x A list of [`tibble::tibble`]s, each of which with columns
#'   `time`, `w`, `x`, `y` and `z`.
#' @param i A valid expression to subset observations from a QTS sample.
#' @param simplify A boolean value specifying whether the resulting subset
#'   should be turned into a single QTS in case the subset is of size 1.
#'   Defaults to `FALSE`.
#'
#' @return An object of class [`qts_sample`].
#' @name qts_sample
#'
#' @examples
#' x <- vespa64$igp
#' y <- as_qts_sample(x)
#' is_qts_sample(x)
#' is_qts_sample(y)
#' x[1]
#' x[1, simplify = TRUE]
NULL

#' @export
#' @rdname qts_sample
as_qts_sample <- function(x) {
  if (is_qts_sample(x)) return(x)
  if (is_qts(x)) x <- list(x)
  if (!is.list(x))
    cli::cli_abort("The input {.arg x} should be a list.")
  x <- purrr::map_if(x, ~ !is_qts(.x), as_qts)
  class(x) <- c("qts_sample", class(x))
  x
}

#' @export
#' @rdname qts_sample
is_qts_sample <- function(x) {
  "qts_sample" %in% class(x)
}

#' @export
#' @rdname qts_sample
"[.qts_sample" <- function(x, i, simplify = FALSE) {
  if (missing(i)) return(x)
  class(x) <- "list"
  x <- x[i]
  if (simplify && length(x) == 1) return(as_qts(x[[1]]))
  as_qts_sample(x)
}

#' QTS Sample Concatenation
#'
#' @param x Either a numeric vector or an object of class [`qts_sample`].
#' @inheritParams base::append
#' @param y Either a numeric vector or an object of class [`qts_sample`] or an
#'   object of class [`qts`].
#' @param ... Extra arguments to be passed on to next methods.
#'
#' @return If `x` is a numeric vector, the output is a numeric vector containing
#'   the values in `x` with the elements of `values` appended after the
#'   specified element of `x`. If `x` is of class [`qts_sample`], the output is
#'   another object of class [`qts_sample`] containing the elements in `x` and
#'   the ones in `y` appended after the last element of `x`.
#'
#' @export
#' @examples
#' append(vespa64$igp, vespa64$igp[1])
#' append(vespa64$igp, vespa64$igp[[1]])
append <- function(x, ...) {
  UseMethod("append")
}

#' @export
#' @rdname append
append.default <- function(x, values, after = length(x), ...) {
  base::append(x = x, values = values, after = after)
}

#' @export
#' @rdname append
append.qts_sample <- function(x, y, ...) {
  if (missing(y)) return(x)
  if (is_qts(y)) y <- as_qts_sample(y)
  if (!is_qts_sample(y))
    cli::cli_abort("The rhs object should be of class either {.cls qts} or {.cls qts_sample}.")
  x[(length(x) + 1):(length(x) + length(y))] <- y
  as_qts_sample(x)
}

#' QTS Random Sampling
#'
#' This function adds uncorrelated Gaussian noise to the logarithm QTS using an
#' exponential covariance function.
#'
#' See \code{\link[roahd]{exp_cov_function}} for details about the roles of
#' `alpha` and `beta` in the definition of the covariance operator.
#'
#' @param n An integer specifying how many QTS should be generated.
#' @param mean_qts An object of class \code{\link{qts}} specifying the mean QTS.
#' @param alpha A positive scalar specifying the variance of each component of
#'   the log-QTS. Defaults to `0.01`.
#' @param beta A positive scalar specifying the exponential weight. Defaults to
#'   `0.001`.
#'
#' @return A list of `n` objects of class \code{\link{qts}} with added noise as
#'   specified by parameters `alpha` and `beta`.
#' @export
#'
#' @examples
#' rnorm_qts(1, vespa64$igp[[1]])
rnorm_qts <- function(n, mean_qts, alpha = 0.01, beta = 0.001) {
  if (!is_qts(mean_qts))
    cli::cli_abort("The {.arg mean_qts} parameter should be of class {.cls qts}.")
  mean_qts <- log(mean_qts)
  time_grid <- mean_qts$time
  C1 <- roahd::exp_cov_function(time_grid, alpha = alpha, beta = beta)
  C2 <- roahd::exp_cov_function(time_grid, alpha = alpha, beta = beta)
  C3 <- roahd::exp_cov_function(time_grid, alpha = alpha, beta = beta)
  centerline <- rbind(mean_qts$x, mean_qts$y, mean_qts$z)
  CholC1 <- chol(C1)
  CholC2 <- chol(C2)
  CholC3 <- chol(C3)
  mean_qts <- roahd::generate_gauss_mfdata(
    N = n,
    L = 3,
    centerline = centerline,
    correlations = rep(0, 3),
    listCholCov = list(CholC1, CholC2, CholC3)
  )
  qts_list <- purrr::map(1:n, ~ {
    as_qts(tibble::tibble(
      time = time_grid,
      w = 0,
      x = mean_qts[[1]][.x, ],
      y = mean_qts[[2]][.x, ],
      z = mean_qts[[3]][.x, ]
    ))
  })
  out <- purrr::map(qts_list, exp_qts)
  as_qts_sample(out)
}

#' QTS Sample Centering and Standardization
#'
#' @param x An object coercible into a numeric matrix or an object of class
#'   [`qts_sample`] representing a sample of observed QTS.
#' @param center A boolean specifying whether to center the sample. If set to
#'   `FALSE`, the original sample is returned, meaning that no standardization
#'   is performed regardless of whether argument `scale` was set to `TRUE` or
#'   not. Defaults to `TRUE`.
#' @param scale A boolean specifying whether to standardize the sample once it
#'   has been centered. Defaults to `TRUE`.
#' @param by_row A boolean specifying whether the QTS scaling should happen for
#'   each data point (`by_row = TRUE`) or for each time point (`by_row =
#'   FALSE`). Defaults to `FALSE`.
#' @param keep_summary_stats A boolean specifying whether the mean and standard
#'   deviation used for standardizing the data should be stored in the output
#'   object. Defaults to `FALSE` in which case only the list of properly
#'   rescaled QTS is returned.
#' @param ... Extra arguments passed on to next methods.
#'
#' @return A list of properly rescaled QTS stored as an object of class
#'   [`qts_sample`] when `keep_summary_stats = FALSE`. Otherwise a list with
#'   three components:
#' - `rescaled_sample`: a list of properly rescaled QTS stored as an object of
#' class [`qts_sample`];
#' - `mean`: a list of numeric vectors storing the corresponding quaternion
#' Fréchet means;
#' - `sd`: a numeric vector storing the corresponding quaternion Fréchet
#' standard deviations.
#'
#' @export
#' @examples
#' x <- scale(vespa64$igp)
#' x[[1]]
scale <- function(x, center = TRUE, scale = TRUE, ...) {
  UseMethod("scale")
}

#' @export
#' @rdname scale
scale.default <- function(x, center = TRUE, scale = TRUE, ...) {
  base::scale(x = x, center = center, scale = scale)
}

#' @export
#' @rdname scale
scale.qts_sample <- function(x,
                             center = TRUE,
                             scale = TRUE,
                             by_row = FALSE,
                             keep_summary_stats = FALSE,
                             ...) {
  if (!center) {
    if (!keep_summary_stats) return(x)
    return(list(
      rescaled_sample = x,
      mean_values = NA,
      sd_values = NA
    ))
  }

  if (!by_row) {
    x <- x |>
      purrr::map(purrr::array_tree, margin = 1) |>
      purrr::transpose() |>
      purrr::map(purrr::reduce, rbind) |>
      purrr::map(tibble::as_tibble) |>
      purrr::map(as_qts)
  }

  std_data <- purrr::map(x, centring, standardize = scale, keep_summary_stats = TRUE)
  x <- purrr::map(std_data, "qts")

  if (!by_row) {
    x <- x |>
      purrr::map(purrr::array_tree, margin = 1) |>
      purrr::transpose() |>
      purrr::map(purrr::reduce, rbind) |>
      purrr::map(tibble::as_tibble) |>
      purrr::map(as_qts)
  }

  if (!keep_summary_stats) return(as_qts_sample(x))

  list(
    rescaled_sample = as_qts_sample(x),
    mean_values = purrr::map(std_data, "mean"),
    sd_values = purrr::map_dbl(std_data, "sd")
  )
}

#' QTS Geometric Mean
#'
#' This function computes the pointwise geometric mean of a QTS sample.
#'
#' @param x An object of class \code{\link{qts_sample}}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return An object of class \code{\link{qts}} in which quaternions are the
#'   pointwise geometric mean of the input QTS sample.
#'
#' @export
#' @examples
#' mean(vespa64$igp)
mean.qts_sample <- function(x, ...) {
  if (!is_qts_sample(x)) x <- as_qts_sample(x)
  mean_qts_impl(x)
}

#' QTS Geometric Median
#'
#' This function computes the pointwise geometric median of a QTS sample.
#'
#' @param x An object of class \code{\link{qts_sample}}.
#' @param na.rm A logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return An object of class \code{\link{qts}} in which quaternions are the
#'   pointwise geometric median of the input QTS sample.
#'
#' @importFrom stats median
#' @export
#' @examples
#' median(vespa64$igp)
median.qts_sample <- function(x, na.rm = FALSE, ...) {
  if (!is_qts_sample(x)) x <- as_qts_sample(x)
  median_qts_impl(x)
}

#' Plot for [`qts_sample`] objects
#'
#' This function creates a visualization of a sample of QTS and returns the
#' corresponding [ggplot2::ggplot] object which enable further customization of
#' the plot.
#'
#' @param object An object of class [`qts_sample`].
#' @param memberships A vector coercible as factor specifying a group membership
#'   for each QTS in the sample. Defaults to `NULL`, in which case no grouping
#'   structure is displayed.
#' @param highlighted A boolean vector specifying whether each QTS in the sample
#'   should be hightlighted. Defaults to `NULL`, in which case no QTS is
#'   hightlighted w.r.t. the others.
#' @param with_animation A boolean value specifying whether to create a an
#'   animated plot or a static [ggplot2::ggplot] object. Defaults to `FALSE`
#'   which will create a static plot.
#' @param ... Further arguments to be passed to methods.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @importFrom ggplot2 autoplot .data
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' ggplot2::autoplot(vespa64$igp)
autoplot.qts_sample <- function(object,
                                memberships = NULL,
                                highlighted = NULL,
                                with_animation = FALSE,
                                ...) {
  if (!is.null(memberships)) {
    if (length(memberships) != length(object))
      cli::cli_abort("The length of the {.arg memberships} argument should match
                     the number of QTS in the sample.")
    memberships <- as.factor(memberships)
  }
  if (!is.null(highlighted)) {
    if (length(highlighted) != length(object))
      cli::cli_abort("The length of the {.arg highlighted} argument should match
                     the number of QTS in the sample.")
    if (!is.logical(highlighted))
      cli::cli_abort("The {.arg highlighted} argument should be a logical vector.")
  }

  n <- length(object)
  if (is.null(highlighted)) highlighted <- rep(FALSE, n)
  use_memberships <- FALSE
  if (is.null(memberships))
    memberships <- as.factor(seq_len(n))
  else
    use_memberships <- TRUE

  data <- tibble::tibble(object, id = as.factor(seq_len(n)), highlighted, memberships) |>
    tidyr::unnest("object")

  if (with_animation) {
    if (!requireNamespace("gganimate", quietly = TRUE))
      cli::cli_abort("You first need to install the {.pkg gganimate} package to
                     create animation plots.")
    return(
      data |>
        tidyr::pivot_longer(cols = "x":"z") |>
        ggplot2::ggplot(ggplot2::aes(
          x = .data$w, y = .data$value,
          group = .data$id,
          colour = .data$memberships
        )) +
        ggplot2::geom_point() +
        ggplot2::geom_line(alpha = 0.25) +
        ggplot2::facet_wrap(ggplot2::vars(.data$name), nrow = 1) +
        ggplot2::labs(
          title = "QTS Sample",
          x = expression(cos(theta / 2)),
          y = expression(sin(theta / 2) %*% component),
          colour = "Group"
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          aspect.ratio = 1,
          legend.position = if (use_memberships) "top" else "none",
          axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
        ) +
        gganimate::transition_reveal(.data$time) +
        gganimate::ease_aes()
    )
  }

  data <- tidyr::pivot_longer(data, cols = "w":"z")
  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = .data$time,
    y = .data$value,
    group = .data$id,
    colour = .data$memberships
  ) ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(.data$name), ncol = 1, scales = "free") +
    ggplot2::theme_linedraw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = "Quaternion Time Series",
      x = "Time",
      y = ""
    )

  if (sum(highlighted) > 0) {
    if (!requireNamespace("gghighlight", quietly = TRUE))
      cli::cli_alert_info("You need to install the {.pkg gghighlight} package to enable highlighting QTS.")
    else {
      p <- p +
        gghighlight::gghighlight(
          any(.data$highlighted),
          unhighlighted_params = list(colour = NULL, alpha = 0.1),
          calculate_per_facet = TRUE,
          label_key = memberships,
          label_params = list(fill = "white", seed = 1234)
        )
    }
  }

  p
}

#' Plot for [`qts_sample`] objects
#'
#' This function creates a visualization of a sample of QTS **without**
#' returning the corresponding [ggplot2::ggplot] object
#'
#' @param x An object of class [`qts_sample`].
#' @inheritParams autoplot.qts_sample
#'
#' @return No return value, called for side effects.
#'
#' @importFrom graphics plot
#' @export
#' @examples
#' plot(vespa64$igp)
plot.qts_sample <- function(x,
                            memberships = NULL,
                            highlighted = NULL,
                            with_animation = FALSE,
                            ...) {
  print(autoplot(
    x,
    memberships = memberships,
    highlighted = highlighted,
    with_animation = with_animation,
    ...
  ))
}
