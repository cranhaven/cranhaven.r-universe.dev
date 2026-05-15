#' Q-Q plot of residual correlation z-statistics
#'
#' Draws a Q-Q plot for residual correlation z-statistics returned by
#' \code{resid_cor()}. For multi-group models, a separate panel is drawn for
#' each group. The \code{n} most extreme pairs (by |z|) are labeled.
#'
#' The z-statistics are expected to follow approximately a standard normal
#' distribution N(0, 1) under correct model specification, so systematic
#' deflections from the diagonal in the Q-Q plot indicate potential model
#' misfit or localized residual dependencies.
#'
#' If \code{z} is not available from \code{lavaan::lavResiduals()}, the
#' function attempts to compute it as \code{cor / se}. If neither \code{z}
#' nor \code{se} are available, the function stops with an informative error.
#'
#' @param fit A fitted \code{lavaan} object.
#' @param n   Number of most extreme |z| points to label (per group). Default 5.
#' @param title Optional plot title.
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{resid_cor}}, \code{\link{resid_corrplot}}, \code{\link{hopper_plot}}
#'
#' @examples
#' # Single-group example
#' HS.model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' fit <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939)
#' resid_qq(fit, n = 5)
#'
#' # Multi-group example (groups by "school")
#' fit_mg <- lavaan::cfa(HS.model,
#'                       data  = lavaan::HolzingerSwineford1939,
#'                       group = "school")
#' resid_qq(fit_mg, n = 7, title = "Residual z Q-Q by group")
#'
#' @export
resid_qq <- function(fit, n = 5, title = NULL) {
  .assert_lavaan_fit(fit)

  r <- resid_cor(fit)

  # ensure z exists; if not, try to compute from cor/se
  if (!("z" %in% names(r)) || all(is.na(r$z))) {
    if ("se" %in% names(r) && any(!is.na(r$se))) {
      r <- dplyr::mutate(r, z = .data$cor / .data$se)
    } else {
      stop("`resid_qq()` needs residual z-statistics or SE to compute them, ",
           "but neither `z` nor `se` are available from `resid_cor()`.", call. = FALSE)
    }
  }

  if (.is_single_group(fit)) {
    r2 <- r |>
      dplyr::arrange(.data$z) |>
      dplyr::mutate(
        p = stats::ppoints(dplyr::n()),
        expected = stats::qnorm(.data$p)
      )

    lbl <- r2 |>
      dplyr::slice_max(abs(.data$z), n = n, with_ties = FALSE)

    p <- ggplot2::ggplot(r2, ggplot2::aes(x = .data$expected, y = .data$z)) +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Theoretical", y = "Observed")

    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(data = lbl, ggplot2::aes(label = .data$pair))
    } else {
      p <- p + ggplot2::geom_text(data = lbl, ggplot2::aes(label = .data$pair), vjust = -0.3)
    }

    if (!is.null(title)) p <- p + ggplot2::labs(title = title)
    return(p)
  }

  # multi-group
  r2 <- r |>
    dplyr::group_by(.data$group) |>
    dplyr::arrange(.data$z, .by_group = TRUE) |>
    dplyr::mutate(
      p = stats::ppoints(dplyr::n()),
      expected = stats::qnorm(.data$p)
    ) |>
    dplyr::ungroup()

  lbl <- r2 |>
    dplyr::group_by(.data$group) |>
    dplyr::slice_max(abs(.data$z), n = n, with_ties = FALSE) |>
    dplyr::ungroup()

  p <- ggplot2::ggplot(r2, ggplot2::aes(x = .data$expected, y = .data$z)) +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Theoretical", y = "Observed") +
    ggplot2::facet_wrap(~group)

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_text_repel(data = lbl, ggplot2::aes(label = .data$pair))
  } else {
    p <- p + ggplot2::geom_text(data = lbl, ggplot2::aes(label = .data$pair), vjust = -0.3)
  }

  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  p
}

