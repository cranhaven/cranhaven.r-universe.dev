#' PCA for QTS Sample
#'
#' This is the `S3` specialization of the function [stats::prcomp()] for QTS
#' samples.
#'
#' The `mean_qts` component of the resulting object is the QTS used for
#' centering. It it part of the `prcomp_qts` object because it is needed to
#' reconstruct the sample from the retained PCs. The `prcomp_qts` object also
#' contains the total variance of the sample and the percentage of variance
#' explained by each PC.
#'
#' @param x An object of class [qts_sample].
#' @param M An integer value specifying the number of principal component to
#'   compute. Defaults to `5L`.
#' @param fit A boolean specifying whether the resulting `prcomp_qts` object
#'   should store a reconstruction of the sample from the retained PCs. Defaults
#'   to `FALSE`.
#' @param ... Arguments passed to or from other methods.
#'
#' @return An object of class `prcomp_qts` which is a list with the following
#'   components:
#' - `tpca`: An object of class `MFPCAfit` as produced by the function
#' [MFPCA::MFPCA()],
#' - `var_props`: A numeric vector storing the percentage of variance explained
#' by each PC,
#' - `total_variance`: A numeric value storing the total variance of the sample,
#' - `mean_qts`: An object of class [qts] containing the mean QTS (used for
#' centering the QTS sample before projecting it to the tangent space),
#' - `principal_qts`: A list of [qts]s containing the required principal
#' components.
#'
#' @importFrom stats prcomp
#' @export
#'
#' @examples
#' res_pca <- prcomp(vespa64$igp[1:16])
prcomp.qts_sample <- function(x, M = 5, fit = FALSE, ...) {
  # Extract common evaluation grid
  x <- .prepare_sample_for_pca(x)
  common_grid <- x[[1]]$time

  # Project QTS sample into tangent space
  qts_c <- scale(x, scale = FALSE, keep_summary_stats = TRUE)
  mean_rotations <- qts_c$mean_values |>
    purrr::transpose() |>
    purrr::simplify_all() |>
    purrr::set_names(c("w", "x", "y", "z"))
  mean_rotations <- as_qts(tibble::tibble(
    time = common_grid,
    w = mean_rotations$w,
    x = mean_rotations$x,
    y = mean_rotations$y,
    z = mean_rotations$z
  ))
  qts_c <- qts_c$rescaled_sample
  qts_log <- log(qts_c)

  # Compute total variance and maximum number of components
  mfd_roahd <- roahd::mfData(common_grid, list(
    qts_log |> purrr::map("x") |> do.call(rbind, args = _),
    qts_log |> purrr::map("y") |> do.call(rbind, args = _),
    qts_log |> purrr::map("z") |> do.call(rbind, args = _)
  ))
  sample_cov <- roahd::cov_fun(mfd_roahd)
  eigen_spectra <- sample_cov[c("1_1", "2_2", "3_3")] |>
    purrr::map("values") |>
    purrr::map(\(.x) eigen(.x, symmetric = TRUE, only.values = TRUE)$values)
  tot_var <- eigen_spectra |>
    purrr::map(\(.x) .x[.x > .Machine$double.eps]) |>
    purrr::reduce(sum)
  M_max <- eigen_spectra |>
    purrr::map(\(.x) .x > .Machine$double.eps) |>
    purrr::map_int(sum) |>
    min()
  cli::cli_alert_info("The maximum number of principal component is {M_max}.")
  if (M > M_max)
    cli::cli_abort("The maximum number of principal component is {M_max}. Please choose a value of {.arg M} smaller or equal to that value.")

  # Store log-QTS sample into multiFunData object
  fd_x <- funData::funData(
    argvals = common_grid,
    X = qts_log |>
      purrr::map("x") |>
      purrr::reduce(rbind)
  )
  fd_y <- funData::funData(
    argvals = common_grid,
    X = qts_log |>
      purrr::map("y") |>
      purrr::reduce(rbind)
  )
  fd_z <- funData::funData(
    argvals = common_grid,
    X = qts_log |>
      purrr::map("z") |>
      purrr::reduce(rbind)
  )
  mfd <- funData::multiFunData(fd_x, fd_y, fd_z)

  # Perform multivariate functional PCA
  uniExpansions <- purrr::map(1:3, \(.x) list(type = "splines1Dpen", k = M))
  tpca <- MFPCA::MFPCA(mfd, M = M, uniExpansions = uniExpansions, fit = fit)

  # Consolidate output
  mean_qts <- tpca$meanFunction |>
    purrr::map(\(.x) as.numeric(.x@X)) |>
    purrr::set_names(c("x", "y", "z")) |>
    tibble::as_tibble()
  mean_qts$time <- common_grid
  mean_qts$w <- 0
  mean_qts <- mean_rotations * exp(as_qts(mean_qts[c(4, 5, 1:3)]))
  out <- list(
    tpca = tpca,
    var_props = tpca$values / tot_var,
    total_variance = tot_var,
    mean_qts = mean_rotations,
    principal_qts = tpca$functions |>
      purrr::map(\(.x) purrr::array_tree(.x@X, margin = 1)) |>
      purrr::transpose() |>
      purrr::map(\(.x) as_qts(tibble::tibble(
        time = common_grid,
        w = 0,
        x = .x[[1]],
        y = .x[[2]],
        z = .x[[3]]
      ))) |>
      purrr::map(exp) |>
      purrr::map(\(qts) mean_rotations * qts)
  )
  class(out) <- "prcomp_qts"
  out
}

.prepare_sample_for_pca <- function(x, x_ref = NULL) {
  if (is.null(x_ref)) x_ref <- x

  lower_bounds <- x_ref |>
    purrr::map("time") |>
    purrr::map_dbl(min)
  if (stats::var(lower_bounds) > 0)
    cli::cli_abort("All input QTS should be evaluated on the same grid.")
  upper_bounds <- x_ref |>
    purrr::map("time") |>
    purrr::map_dbl(max)
  if (stats::var(upper_bounds) > 0)
    cli::cli_abort("All input QTS should be evaluated on the same grid.")

  grid_sizes <- purrr::map_int(x_ref, nrow)
  common_grid_size <- max(grid_sizes)
  common_grid <- seq(
    from = min(lower_bounds),
    to = max(upper_bounds),
    length.out = common_grid_size
  )

  if (any(lower_bounds != min(common_grid)) || any(upper_bounds != max(common_grid)))
    cli::cli_abort("The sample stored in {.arg newdata} should contain QTSs defined on the same domain as those used to perform the PCA.")

  as_qts_sample(purrr::map_if(x, \(.x) nrow(.x) < common_grid_size, \(.x) {
    resample(.x, nout = common_grid_size)
  }))
}

#' Predict QTS from PCA decomposition
#'
#' This function predicts the QTS of a new sample from the PCA decomposition of
#' a previous sample.
#'
#' @param object An object of class `prcomp_qts` as produced by the
#'   [prcomp.qts_sample()] method.
#' @param newdata An object of class [`qts`] or [`qts_sample`] specifying a QTS
#'   or a sample of QTS. The QTS should be evaluated on the same grid as the one
#'   used to fit the PCA model. If the evaluation grids map the same domain but
#'   with different sampling frequenciesa, the QTS will be linearly interpolated
#'   (in the Lie algebra) to the common grid used to fit the PCA model.
#' @param ... Additional arguments. Not used here.
#'
#' @return An object of class [`qts_sample`] containing the predicted QTS.
#'
#' @importFrom stats predict
#' @export
#'
#' @examples
#' # Fit PCA model
#' pr <- prcomp(vespa64$igp, M = 5)
#'
#' # Predict QTS
#' new_qts <- predict(pr)
predict.prcomp_qts <- function(object, newdata, ...) {
  if (missing(newdata)) {
    score_matrix <- object$tpca$scores
  } else {
    if (is_qts(newdata)) {
      newdata <- as_qts_sample(list(newdata))
    }
    newdata <- .prepare_sample_for_pca(newdata, x_ref = object$x)

    log_newdata <- newdata |>
      purrr::map(\(.x) inverse_qts(object$mean_qts) * .x) |>
      as_qts_sample() |>
      log()

    score_matrix <- log_newdata |>
      purrr::map(\(.x) {
        1:3 |>
          purrr::map(\(l) as.numeric(object$tpca$functions[[l]]@X %*% .x[[2 + l]])) |>
          purrr::transpose() |>
          purrr::simplify_all() |>
          purrr::map_dbl(sum)
      }) |>
      do.call(rbind, args = _)
  }

  X <- score_matrix %*% object$tpca$functions[[1]]@X
  Y <- score_matrix %*% object$tpca$functions[[2]]@X
  Z <- score_matrix %*% object$tpca$functions[[3]]@X
  N <- dim(X)[1]
  common_time <- object$mean_qts$time
  out <- purrr::map(1:N, \(.n) {
    res <- tibble::tibble(time = common_time, w = 0)
    res$x <- as.numeric(X[.n, ])
    res$y <- as.numeric(Y[.n, ])
    res$z <- as.numeric(Z[.n, ])
    as_qts(res)
  }) |>
    as_qts_sample() |>
    exp()
  as_qts_sample(purrr::map(out, \(.qts) object$mean_qts * .qts))
}

#' Plot for `prcomp_qts` objects
#'
#' This function creates a visualization of the results of the PCA applied on a
#' sample of QTS and returns the corresponding [ggplot2::ggplot] object which
#' enable further customization of the plot.
#'
#' @param object An object of class `prcomp_qts` as produced by the
#'   [prcomp.qts_sample()] method.
#' @param what A string specifying what kind of visualization the user wants to
#'   perform. Choices are words starting with `PC` and ending with a PC number
#'   (in which case the mean QTS is displayed along with its perturbations due
#'   to the required PC) or `scores` (in which case individuals are projected on
#'   the required plane). Defaults to `PC1`.
#' @param ... If `what = "PC?"`, the user can specify whether to plot the QTS in
#'   the tangent space or in the original space by providing a boolean argument
#'   `original_space` which defaults to `TRUE`. If `what = "scores"`, the user
#'   can specify the plane onto which the individuals will be projected by
#'   providing a length-2 integer vector argument `plane` which defaults to
#'   `1:2`.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @importFrom ggplot2 autoplot .data
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' df <- as_qts_sample(vespa64$igp[1:16])
#' res_pca <- prcomp(df)
#'
#' # Plot the data points in a PC plane
#' # And color points according to a categorical variable
#' p <- ggplot2::autoplot(res_pca, what = "scores")
#' p + ggplot2::geom_point(ggplot2::aes(color = vespa64$V[1:16]))
autoplot.prcomp_qts <- function(object, what = "PC1", ...) {
  dots <- list(...)
  if (substr(what, 1, 2) == "PC") {
    component <- as.numeric(substr(what, 3, nchar(what)))
    if ("original_space" %in% names(dots))
      plot_tpca_component(object, component = component, original_space = dots$original_space)
    else {
      cli::cli_inform("The {.code original_space} boolean argument is not specified. Defaulting to {.field TRUE}.")
      plot_tpca_component(object, component = component, original_space = TRUE)
    }
  } else if (what == "scores") {
    if ("plane" %in% names(dots))
      plot_tpca_scores(object, plane = dots$plane)
    else {
      cli::cli_inform("The {.code plane} length-2 integer vector argument is not specified. Defaulting to {.field 1:2}.")
      plot_tpca_scores(object, plane = 1:2)
    }
  } else if (what == "variance") {
    screeplot(object)
  } else
    cli::cli_abort("The {.arg what} argument should be either {.field scores} or {.field variance} or a principal component specified starting with {.field PC}.")
}

#' Plot for `prcomp_qts` objects
#'
#' This function creates a visualization of the results of the PCA applied on a
#' sample of QTS **without** returning the plot data as an object.
#'
#' @param x An object of class `prcomp_qts` as produced by the
#'   [prcomp.qts_sample()] method.
#' @inheritParams autoplot.prcomp_qts
#'
#' @return No return value, called for side effects.
#'
#' @importFrom graphics plot
#' @export
#' @examples
#' df <- as_qts_sample(vespa64$igp[1:16])
#' res_pca <- prcomp(df)
#'
#' # You can plot the effect of a PC on the mean
#' plot(res_pca, what = "PC1")
#'
#' # You can plot the data points in a PC plane
#' plot(res_pca, what = "scores")
plot.prcomp_qts <- function(x, what = "PC1", ...) {
  print(autoplot(x, what = what, ...))
}

#' @importFrom stats screeplot
#' @export
#' @rdname plot.prcomp_qts
screeplot.prcomp_qts <- function(x, ...) {
  plot_data <- tibble::tibble(
    lambda = x$var_props,
    m = seq_along(.data$lambda)
  )

  plot_data |>
    ggplot2::ggplot(ggplot2::aes(.data$m, .data$lambda)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = "Screeplot of QTS PCA",
      x = "Principal Component Index",
      y = "Percentage of variance explained"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_linedraw()
}

plot_tpca_component <- function(tpca, component = 1, original_space = TRUE) {
  plot_mean <- log(inverse_qts(tpca$mean_qts) * tpca$mean_qts)
  plot_cp <- log(inverse_qts(tpca$mean_qts) * tpca$principal_qts[[component]])
  K <- stats::median(abs(tpca$tpca$scores[, component]))
  plot_lb <- plot_mean - plot_cp * K
  plot_ub <- plot_mean + plot_cp * K
  if (original_space) {
    plot_mean <- tpca$mean_qts * exp(plot_mean)
    plot_lb <- tpca$mean_qts * exp(plot_lb)
    plot_ub <- tpca$mean_qts * exp(plot_ub)
  } else {
    plot_mean$w <- NULL
    plot_lb$w <- NULL
    plot_ub$w <- NULL
  }
  plot_mean$col <- "mean"
  plot_lb$col <- cli::pluralize("mean - med(|scores|) * PC{component}")
  plot_ub$col <- cli::pluralize("mean + med(|scores|) * PC{component}")

  plot_data <- rbind(plot_mean, plot_lb, plot_ub) |>
    tidyr::pivot_longer(-c("time", "col"))

  plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$time, y = .data$value, color = .data$col)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(.data$name), ncol = 1, scales = "free") +
    ggplot2::theme_linedraw() +
    ggplot2::labs(
      title = cli::pluralize("Mean QTS perturbed by PC{component}"),
      subtitle = cli::pluralize("Percentage of variance explained: {round(tpca$var_props[component] * 100, digits = 1)}%"),
      x = "Time (%)",
      y = "",
      color = ""
    )
}

plot_tpca_scores <- function(tpca, plane = 1:2) {
  if (length(plane) != 2)
    cli::cli_abort("The {.code plane} argument should be of length two.")
  scores <- tpca$tpca$scores[, plane]
  n <- nrow(scores)

  plot_data <- tibble::tibble(x = scores[, 1], y = scores[, 2])

  plot_data |>
    ggplot2::ggplot(ggplot2::aes(.data$x, .data$y, label = 1:n)) +
    ggplot2::geom_point() +
    ggrepel::geom_label_repel(seed = 1234) +
    ggplot2::theme_linedraw() +
    ggplot2::labs(
      title = cli::pluralize("Individuals projected on the PC{plane[1]}-{plane[2]} plane"),
      subtitle = cli::pluralize("Combined percentage of variance explained: {round(sum(tpca$var_props[plane]) * 100, digits = 1)}%"),
      x = cli::pluralize("PC{plane[1]} ({round(sum(tpca$var_props[plane[1]]) * 100, digits = 1)}%)"),
      y = cli::pluralize("PC{plane[2]} ({round(sum(tpca$var_props[plane[2]]) * 100, digits = 1)}%)")
    )
}
