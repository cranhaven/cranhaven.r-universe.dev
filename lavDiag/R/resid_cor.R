#' Residual correlations (Bentler or other types) as a tidy tibble
#'
#' Creates a tidy tibble of residual **correlations** from a fitted \code{lavaan}
#' model, including standard errors and z-statistics when available. Supports
#' single- and multi-group models and allows selection of the correlation type
#' (e.g., Bentler, Pearson, or residual covariance-based).
#'
#' @param fit A fitted \code{lavaan} object.
#' @param type Character; which type of residual correlation to extract. One of
#'   \code{c("cor.bentler", "cor", "cor.sample", "cov", "cov.sample")}. Default is
#'   \code{"cor.bentler"}.
#'
#' @details
#' Internally uses \code{lavaan::lavResiduals(type = type, se = TRUE)}.
#' For multi-group models, a \code{group} column is added (using
#' \code{lavaan::lavInspect(fit, "group.label")} when available).
#'
#' \strong{Duplicate removal & stable ordering}
#' \itemize{
#'   \item Residual correlations are first obtained as (group-wise) symmetric matrices.
#'   \item Only the \emph{upper triangle without the diagonal} is kept, using a logic
#'         equivalent to \code{mat[upper.tri(mat, diag = FALSE)]}.
#'   \item Variable pairs are created via \code{v1 <- pmin(i, j)}, \code{v2 <- pmax(i, j)}
#'         so that each pair appears once regardless of original order.
#'   \item A human-readable \code{pair} label is created as \code{paste0(v1, "-", v2)}.
#'   \item The result is sorted stably by \code{group} (if present) and \code{pair} for
#'         reproducible outputs across sessions.
#' }
#'
#' @return
#' A tibble with columns:
#' \itemize{
#'   \item \code{v1}, \code{v2} – variable names in the pair
#'   \item \code{pair} – canonical pair label \code{"v1-v2"} with alphabetical ordering via \code{pmin/pmax}
#'   \item \code{cor} – residual correlation
#'   \item \code{abs_cor} – absolute value of \code{cor}
#'   \item \code{se} – standard error (if available from lavaan)
#'   \item \code{z} – z-statistic (if available)
#'   \item \code{group} – group label (multi-group models only)
#' }
#'
#' @seealso
#' \code{\link{resid_corrplot}}, \code{\link{hopper_plot}}, \code{\link{resid_qq}}
#'
#' @examples
#' HS.model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' fit <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939)
#' resid_cor(fit)
#' resid_cor(fit, type = "cor")  # standard residual correlations
#'
#' # Multi-group example (group by school)
#' fit_mg <- lavaan::cfa(
#'   HS.model,
#'   data  = lavaan::HolzingerSwineford1939,
#'   group = "school"
#' )
#' rc <- resid_cor(fit_mg, type = "cor.bentler")
#' head(rc)
#'
#' @export
resid_cor <- function(
    fit,
    type = c("cor.bentler", "cor", "cor.sample", "cov", "cov.sample")) {

  .assert_lavaan_fit(fit)
  type <- match.arg(type)

  x <- lavaan::lavResiduals(fit, type = type, se = TRUE)

  tidy_one <- function(lst) {
    have_se <- !is.null(lst$cov.se)
    have_z  <- !is.null(lst$cov.z)

    cov_tbl <- tibble::as_tibble(as.matrix(lst$cov), .name_repair = "minimal") |>
      dplyr::mutate(v1 = colnames(lst$cov))

    long <- tidyr::pivot_longer(
      cov_tbl,
      cols = -tidyselect::all_of("v1"),
      names_to = "v2",
      values_to = "cor"
    ) |>
      dplyr::mutate(
        v1c  = pmin(.data$v1, .data$v2),
        v2c  = pmax(.data$v1, .data$v2),
        pair = paste0(.data$v1c, "-", .data$v2c)
      ) |>
      dplyr::filter(.data$v1 != .data$v2) |>
      dplyr::distinct(.data$pair, .keep_all = TRUE) |>
      dplyr::transmute(
        v1   = .data$v1c,
        v2   = .data$v2c,
        pair = .data$pair,
        cor  = .data$cor
      )

    if (have_se) {
      se_tbl <- tibble::as_tibble(as.matrix(lst$cov.se), .name_repair = "minimal") |>
        dplyr::mutate(v1 = colnames(lst$cov.se)) |>
        tidyr::pivot_longer(
          cols = -tidyselect::all_of("v1"),
          names_to = "v2",
          values_to = "se"
        ) |>
        dplyr::mutate(
          v1 = pmin(.data$v1, .data$v2),
          v2 = pmax(.data$v1, .data$v2),
          pair = paste0(.data$v1, "-", .data$v2)
        ) |>
        dplyr::distinct(.data$pair, .keep_all = TRUE) |>
        dplyr::select(.data$pair, .data$se)
      long <- dplyr::left_join(long, se_tbl, by = "pair")
    }

    if (have_z) {
      z_tbl <- tibble::as_tibble(as.matrix(lst$cov.z), .name_repair = "minimal") |>
        dplyr::mutate(v1 = colnames(lst$cov.z)) |>
        tidyr::pivot_longer(
          cols = -tidyselect::all_of("v1"),
          names_to = "v2",
          values_to = "z"
        ) |>
        dplyr::mutate(
          v1 = pmin(.data$v1, .data$v2),
          v2 = pmax(.data$v1, .data$v2),
          pair = paste0(.data$v1, "-", .data$v2)
        ) |>
        dplyr::distinct(.data$pair, .keep_all = TRUE) |>
        dplyr::select(.data$pair, .data$z)
      long <- dplyr::left_join(long, z_tbl, by = "pair")
    }

    long |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c("cor", "se", "z")), ~ as.double(.)),
        abs_cor = as.double(abs(.data$cor))
      ) |>
      dplyr::relocate(.data$abs_cor, .after = .data$cor)
  }

  ng <- lavaan::lavInspect(fit, "ngroups")
  if (identical(ng, 1L)) {
    tidy_one(x) |>
      dplyr::arrange(dplyr::desc(.data$abs_cor))
  } else {
    labs <- tryCatch(lavaan::lavInspect(fit, "group.label"), error = function(e) NULL)
    if (is.null(labs) || length(labs) != ng) labs <- as.character(seq_len(ng))

    out <- lapply(seq_len(ng), function(g) {
      tg <- tidy_one(x[[g]])
      tg$group <- labs[g]
      tg
    }) |>
      dplyr::bind_rows()

    out |>
      dplyr::arrange(.data$group, dplyr::desc(.data$abs_cor))
  }
}
