#' Corrplot of residual correlations (configurable type)
#'
#' Draw corrplot(s) of residual correlations from a fitted \pkg{lavaan} model.
#' The \code{type} argument allows selecting which residual correlation or covariance
#' metric to visualize (e.g., Bentler, Pearson, or sample-based). For multi-group
#' models you can harmonize the color scale across groups via \code{common_scale = TRUE}.
#' Produces base plots.
#'
#' @details
#' Uses corrplot::corrplot.mixed() for rendering. When common_scale = TRUE,
#' the color legend is harmonized across groups by taking a common symmetric range
#' (-L, L) with \eqn{L = max(|values|)} across all groups; otherwise each
#' group panel uses its own range. The is.corr flag is set automatically based
#' on type (TRUE when type starts with "cor").
#'
#' If \code{record = TRUE}, returns a recorded plot object (single-group) or a named
#' list of recorded plots (multi-group) created with \code{grDevices::recordPlot()}.
#' You can later replay them with \code{grDevices::replayPlot()}.
#'
#' @param fit A fitted \code{lavaan} object.
#' @param type Character; which type of residual correlation/covariance to plot.
#'   One of \code{c("cor.bentler", "cor", "cor.sample", "cov", "cov.sample")}.
#'   Default \code{"cor.bentler"}.
#' @param order One of \code{c("original","AOE","FPC","hclust","alphabet")} controlling
#'   the variable ordering in the plot.
#' @param hclust.method One of
#'   \code{c("complete","ward","ward.D","ward.D2","single","average","mcquitty","median","centroid")}.
#' @param common_scale Logical; use a common symmetric color range across groups?
#'   Default \code{TRUE}.
#' @param title_prefix Optional character prefix for multi-group plot titles.
#' @param record Logical; if \code{TRUE}, return recorded plot(s) via \code{recordPlot()}.
#'   Default \code{FALSE}.
#'
#' @return
#' - If \code{record = FALSE} (default): invisibly returns \code{NULL} (plots are drawn as a side-effect).
#' - If \code{record = TRUE}: a \code{recordedplot} (single-group) or a named \code{list} of \code{recordedplot}s (multi-group).
#'
#' @seealso \code{\link{resid_cor}},' \code{\link{resid_qq}}, \code{\link{hopper_plot}},
#' \code{\link[corrplot]{corrplot.mixed}},
#'
#' @examples
#' HS.model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' fit <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939)
#'
#' # Draw Bentler-type residual correlations
#' resid_corrplot(fit, type = "cor.bentler", order = "hclust")
#'
#' # Draw standard residual correlations
#' resid_corrplot(fit, type = "cor")
#'
#' # Capture plot object for later replay
#' rec <- resid_corrplot(fit, type = "cor.bentler", order = "hclust", record = TRUE)
#' if (interactive()) grDevices::replayPlot(rec)
#'
#' # Multi-group demo of common_scale and title_prefix
#' fit_mg <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939, group = "school")
#' # harmonized color scale across groups
#' resid_corrplot(fit_mg, type = "cor.bentler", common_scale = TRUE,  title_prefix = "School: ")
#' # per-group color scales
#' resid_corrplot(fit_mg, type = "cor.bentler", common_scale = FALSE, title_prefix = "School: ")
#'
#' @export
resid_corrplot <- function(fit,
                           type = c("cor.bentler", "cor", "cor.sample", "cov", "cov.sample"),
                           order = c("original", "AOE", "FPC", "hclust", "alphabet"),
                           hclust.method = c("complete", "ward", "ward.D", "ward.D2",
                                             "single", "average", "mcquitty", "median", "centroid"),
                           common_scale = TRUE,
                           title_prefix = NULL,
                           record = FALSE) {
  .assert_lavaan_fit(fit)
  type <- match.arg(type)

  if (utils::packageVersion("corrplot") < "0.90") {
    stop("`resid_corrplot()` requires corrplot >= 0.90 (supports `col.lim`). ",
         "Installed: ", as.character(utils::packageVersion("corrplot")), call. = FALSE)
  }

  order         <- match.arg(order)
  hclust.method <- match.arg(hclust.method)

  rs <- lavaan::lavResiduals(fit, type = type)
  ng <- lavaan::lavInspect(fit, "ngroups")

  draw_one <- function(mat, ttl = NULL, lim = NULL, record = FALSE) {
    mat <- as.matrix(mat)
    corrplot::corrplot.mixed(
      mat,
      order = order,
      hclust.method = hclust.method,
      is.corr = grepl("^cor", type),
      cl.ratio = 0.1, tl.srt = 90, mar = c(0, 0, 2, 0),
      lower.col = "black",
      col.lim = if (!is.null(lim)) c(-lim, lim) else NULL,
      title = ttl
    )
    if (record) grDevices::recordPlot() else invisible(NULL)
  }

  if (identical(ng, 1L)) {
    mat <- rs$cov
    lim <- if (isTRUE(common_scale)) max(abs(mat), na.rm = TRUE) else NULL
    res <- draw_one(mat, ttl = NULL, lim = lim, record = record)
    if (record) return(res)
    return(invisible(NULL))
  }

  mats <- lapply(rs, function(lst) lst$cov)
  labs <- tryCatch(lavaan::lavInspect(fit, "group.label"), error = function(e) NULL)
  if (is.null(labs) || length(labs) != length(mats)) {
    labs <- names(mats)
    if (is.null(labs) || any(!nzchar(labs))) labs <- as.character(seq_along(mats))
  }

  lim <- if (isTRUE(common_scale)) max(abs(unlist(mats, use.names = FALSE)), na.rm = TRUE) else NULL

  if (!record) {
    for (i in seq_along(mats)) {
      ttl <- if (!is.null(title_prefix)) paste0(title_prefix, labs[i]) else labs[i]
      draw_one(mats[[i]], ttl = ttl, lim = lim, record = FALSE)
    }
    return(invisible(NULL))
  } else {
    recs <- vector("list", length(mats))
    names(recs) <- labs
    for (i in seq_along(mats)) {
      ttl <- if (!is.null(title_prefix)) paste0(title_prefix, labs[i]) else labs[i]
      recs[[i]] <- draw_one(mats[[i]], ttl = ttl, lim = lim, record = TRUE)
    }
    return(recs)
  }
}
