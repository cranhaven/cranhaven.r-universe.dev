#' Hopper plot of the largest residual correlations
#'
#' Draws a "hopper" plot of the top-\code{n_max} absolute residual correlations
#' (Bentler type) computed by \code{resid_cor()} — either for a single group or
#' per-group facets for multi-group models.
#'
#' @param fit   A fitted \code{lavaan} object.
#' @param title Optional plot title.
#' @param n_max Number of variable pairs to show (per group when multi-group).
#' @param sep   Separator used by \code{.reorder_within()} for multi-group axis
#'   labeling. You usually don't need to change this.
#'
#' @return A \code{ggplot2} object.
#' @seealso \link{resid_cor}, \link{resid_corrplot}
#' @family lavDiag-visualization
#' @importFrom rlang .data
#'
#' @examples
#' HS.model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' fit <- lavaan::cfa(HS.model,
#'                    data = lavaan::HolzingerSwineford1939)
#' hopper_plot(fit, n_max = 10)
#'
#' @export
hopper_plot <- function(fit, title = NULL, n_max = 15, sep = "___") {
  # stop early if not a lavaan fit (internal helper)
  .assert_lavaan_fit(fit)

  residuals <- resid_cor(fit)

  if (.is_single_group(fit)) {
    df <- residuals |>
      dplyr::slice_max(.data$abs_cor, n = n_max, with_ties = FALSE) |>
      dplyr::mutate(
        pair = stats::reorder(.data$pair, - .data$abs_cor),
        sign = ifelse(.data$cor < 0, "Negative", "Positive")
      )

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$pair, y = .data$abs_cor)) +
      ggplot2::geom_line(ggplot2::aes(group = 1)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$sign), size = 5) +
      ggplot2::labs(
        y = "Absolute residual correlation",
        x = "Variable pair",
        color = "Sign"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

    if (!is.null(title)) p <- p + ggplot2::labs(title = title)
    return(p)
  }

  # multi-group
  df <- residuals |>
    dplyr::group_by(.data$group) |>
    dplyr::slice_max(.data$abs_cor, n = n_max, with_ties = FALSE) |>
    dplyr::arrange(.data$group, .data$abs_cor) |>
    dplyr::mutate(
      pair = .reorder_within(.data$pair, - .data$abs_cor, .data$group, sep = sep),
      sign = ifelse(.data$cor < 0, "Negative", "Positive")
    )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$pair, y = .data$abs_cor)) +
    ggplot2::geom_line(ggplot2::aes(group = 1)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$sign), size = 5) +
    ggplot2::labs(
      y = "Absolute residual correlation",
      x = "Variable pair",
      color = "Sign"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::facet_wrap(~group, scales = "free_x") +
    ggplot2::scale_x_discrete(labels = function(x) sub(paste0(sep, ".*$"), "", x))

  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  return(p)
}
