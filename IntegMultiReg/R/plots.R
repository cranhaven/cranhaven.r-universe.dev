## Stand-alone plotting helpers for objects of class "imr".

#' Plot the Top Selected Features of an IMR Fit
#'
#' @description
#' Draws a horizontal bar chart of the features with the highest marginal
#' posterior inclusion probability (mPIP).  For each platform-feature pair, the
#' plotted score is the highest mPIP attained among the availability subgroups
#' containing that platform; the displayed bars are the largest such scores
#' across all platforms.  This complements the per-platform heatmap of
#' `plot(fit, type = "selection")`.
#'
#' @param object A fitted object of class `"imr"` returned by [imr()].
#' @param top Integer; the number of highest-mPIP features to display
#'   (default `10`).
#' @param base_cex Overall text-size multiplier. The `cex_*` arguments default
#'   to values derived from this multiplier (default `1`).
#' @param cex_names Feature-label size multiplier. If `NULL`, a plot-specific
#'   default derived from `base_cex` is used.
#' @param cex_axis Axis-label size multiplier. If `NULL`, a plot-specific
#'   default derived from `base_cex` is used.
#' @param cex_lab Axis-title size multiplier. If `NULL`, a plot-specific
#'   default derived from `base_cex` is used.
#' @param cex_main Main-title size multiplier. If `NULL`, a plot-specific
#'   default derived from `base_cex` is used.
#' @param cex_legend Legend text size multiplier. If `NULL`, a plot-specific
#'   default derived from `base_cex` is used.
#' @param col Optional bar colours. If `NULL`, colours are generated from
#'   `palette`.
#' @param palette Palette name used when `col = NULL`; `"platform"` gives the
#'   package's standard muted platform colours (default).
#' @param reference Optional vertical reference line; use `NULL` to suppress it
#'   (default `0.5`).
#' @param show_source Logical; should each bar be annotated with the
#'   availability subgroup (bitstring) in which the feature attained its maximum
#'   mPIP (default `TRUE`)?
#' @param legend Logical; should the platform legend be drawn (default `TRUE`)?
#' @param xlim Numeric vector of length two giving the horizontal axis limits
#'   (default `c(0, 1)`).
#' @param mar,mgp Optional graphical margin and axis-title placement vectors
#'   passed to [graphics::par()] for finer layout control.
#' @param ... Further graphical parameters passed to [graphics::barplot()].
#'
#' @return Invisibly, a data frame of the displayed features with columns
#'   `platform`, `feature`, `mpip` and `subgroup`, where `mpip` is the maximum
#'   subgroup mPIP for that platform-feature pair and `subgroup` is the
#'   subgroup where the maximum is attained. Rows are ordered by decreasing
#'   mPIP.
#' @seealso [imr()], [plot.imr()], [plot_subgroup_sizes()]
#' @examples
#' \donttest{
#' data("simIMR", package = "IntegMultiReg")
#' fit <- imr(
#'   platform_data_list = simIMR$platforms, outcome = simIMR$outcome,
#'   cov = simIMR$covariates, type_outcome = "binary",
#'   nu = c(-4, -3, -4), sample_mcmc = c(200, 100), ssize = 5, seed = 1
#' )
#' plot_top_features(fit, top = 8)
#' }
#' @export
plot_top_features <- function(object, top = 10, base_cex = 1,
                              cex_names = NULL, cex_axis = NULL,
                              cex_lab = NULL, cex_main = NULL,
                              cex_legend = NULL, col = NULL,
                              palette = "platform", reference = 0.5,
                              show_source = TRUE, legend = TRUE, xlim = c(0, 1),
                              mar = NULL, mgp = NULL, ...) {
  if (!inherits(object, "imr")) {
    .imr_abort("`object` must be an `imr` object returned by `imr()`.")
  }
  dots <- list(...)
  top <- .imr_check_integer_scalar(top, "top", min = 1)
  sz <- .imr_plot_cex(
    dots, base_cex = base_cex, cex_names = cex_names,
    cex_axis = cex_axis, cex_lab = cex_lab, cex_main = cex_main,
    cex_legend = cex_legend,
    defaults = list(axis = 0.95, lab = 1.05, main = 1.1,
                    names = 0.95, legend = 0.9, values = 0.9)
  )
  dots <- sz$dots
  .imr_check_flag(show_source, "show_source")
  .imr_check_flag(legend, "legend")
  xlim <- .imr_check_numeric_vector(xlim, "xlim", length = 2)
  if (xlim[1] >= xlim[2]) {
    .imr_abort("`xlim` must be increasing.")
  }
  if (!is.null(reference)) {
    reference <- .imr_check_numeric_vector(
      reference, "reference", length = 1, nonnegative = TRUE
    )
    if (reference > 1) {
      .imr_abort("`reference` must be between 0 and 1, or NULL.")
    }
  }
  ## Collect every feature's best mPIP (and the subgroup achieving it).
  rows <- list()
  for (l in seq_len(object$n_platform)) {
    m <- .imr_mpip(object, l)
    if (nrow(m) == 0 || ncol(m) == 0) next
    maxp <- apply(m, 2, max)
    sg <- rownames(m)[apply(m, 2, which.max)]
    rows[[l]] <- data.frame(
      platform = object$platform_names[l],
      feature = colnames(m),
      mpip = as.numeric(maxp),
      subgroup = sg,
      stringsAsFactors = FALSE
    )
  }
  tab <- do.call(rbind, rows)
  if (is.null(tab) || nrow(tab) == 0) {
    .imr_abort("The fit contains no selectable features to plot.")
  }
  tab <- tab[order(tab$mpip, decreasing = TRUE), , drop = FALSE]
  top <- min(top, nrow(tab))
  tab <- tab[seq_len(top), , drop = FALSE]

  ## One colour per platform, drawn in increasing order so the largest bar is
  ## at the top of the horizontal chart.
  platforms <- object$platform_names
  pal <- .imr_plot_palette(length(platforms), col = col, palette = palette)
  ord <- rev(seq_len(top))
  labels <- paste0(tab$feature[ord], " (", tab$platform[ord], ")")

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)
  pp <- .imr_plot_par(mar, mgp, default_mar = c(4.8, 8.8, 3.2, 1))
  graphics::par(mar = pp$mar, mgp = pp$mgp)
  if (is.null(dots$border)) dots$border <- NA
  bp <- do.call(graphics::barplot, c(list(
    height = tab$mpip[ord], names.arg = labels, horiz = TRUE, las = 1,
    xlim = xlim, xlab = "Maximum subgroup mPIP",
    main = sprintf("Top %d features by max subgroup mPIP", top),
    col = pal[match(tab$platform[ord], platforms)],
    cex.names = sz$names, cex.axis = sz$axis,
    cex.lab = sz$lab, cex.main = sz$main
  ), dots))
  if (!is.null(reference)) {
    graphics::abline(v = reference, lty = 2,
                     col = .imr_plot_reference_colour())
  }
  ## Annotate each bar with the availability subgroup where its maximum mPIP was
  ## attained.  Long bars carry the label inside the coloured region (light
  ## text); short bars carry it just past the tip (dark text) so it never
  ## overplots the reference line or spills past the axis.
  if (show_source) {
    xend <- tab$mpip[ord]
    src <- tab$subgroup[ord]
    inside <- xend >= xlim[1] + 0.5 * (xlim[2] - xlim[1])
    if (any(inside)) {
      graphics::text(xend[inside], bp[inside], labels = src[inside],
                     pos = 2, offset = 0.35, col = "white",
                     font = 2, cex = sz$values)
    }
    if (any(!inside)) {
      graphics::text(xend[!inside], bp[!inside], labels = src[!inside],
                     pos = 4, offset = 0.35,
                     col = .imr_plot_reference_colour(),
                     font = 2, cex = sz$values)
    }
  }
  if (legend) {
    graphics::legend("bottomright", legend = platforms, fill = pal,
                     bty = "n", cex = sz$legend)
  }
  invisible(tab[, c("platform", "feature", "mpip", "subgroup")])
}


#' Plot the Availability Subgroup Sizes of an IMR Fit
#'
#' @description
#' Draws a bar chart of the number of subjects in each modelled
#' availability subgroup (the non-empty regions of the Venn diagram), giving a
#' quick picture of how the sample is distributed across subgroups.
#'
#' @param object A fitted object of class `"imr"` returned by [imr()].
#' @param base_cex Overall text-size multiplier. The `cex_*` arguments default
#'   to values derived from this multiplier (default `1`).
#' @param cex_axis Axis-label size multiplier. If `NULL`, a plot-specific
#'   default derived from `base_cex` is used.
#' @param cex_lab Axis-title size multiplier. If `NULL`, a plot-specific
#'   default derived from `base_cex` is used.
#' @param cex_main Main-title size multiplier. If `NULL`, a plot-specific
#'   default derived from `base_cex` is used.
#' @param cex_values Bar-value label size multiplier. If `NULL`, a plot-specific
#'   default derived from `base_cex` is used.
#' @param col Optional bar colours. If `NULL`, colours are generated from
#'   `palette`.
#' @param palette Palette name used when `col = NULL`; `"subgroup"` gives the
#'   package's standard neutral grey-blue subgroup colours (default).
#' @param show_values Logical; should sample sizes be printed above the bars
#'   (default `TRUE`)?
#' @param ylim Optional numeric vector of length two giving the vertical axis
#'   limits. If `NULL`, limits are chosen from the subgroup sizes.
#' @param mar,mgp Optional graphical margin and axis-title placement vectors
#'   passed to [graphics::par()] for finer layout control.
#' @param ... Further graphical parameters passed to [graphics::barplot()].
#'
#' @return Invisibly, the named integer vector of subgroup sizes.
#' @seealso [imr()], [plot.imr()], [plot_top_features()]
#' @examples
#' \donttest{
#' data("simIMR", package = "IntegMultiReg")
#' fit <- imr(
#'   platform_data_list = simIMR$platforms, outcome = simIMR$outcome,
#'   cov = simIMR$covariates, type_outcome = "binary",
#'   nu = c(-4, -3, -4), sample_mcmc = c(200, 100), ssize = 5, seed = 1
#' )
#' plot_subgroup_sizes(fit)
#' }
#' @export
plot_subgroup_sizes <- function(object, base_cex = 1, cex_axis = NULL,
                                cex_lab = NULL, cex_main = NULL,
                                cex_values = NULL, col = NULL,
                                palette = "subgroup", show_values = TRUE,
                                ylim = NULL, mar = NULL, mgp = NULL, ...) {
  if (!inherits(object, "imr")) {
    .imr_abort("`object` must be an `imr` object returned by `imr()`.")
  }
  dots <- list(...)
  sz <- .imr_plot_cex(
    dots, base_cex = base_cex, cex_axis = cex_axis,
    cex_lab = cex_lab, cex_main = cex_main, cex_values = cex_values,
    defaults = list(axis = 0.95, lab = 1.05, main = 1.1,
                    names = 1, legend = 0.9, values = 0.95)
  )
  dots <- sz$dots
  .imr_check_flag(show_values, "show_values")
  sizes <- as.integer(object$sample_size)
  names(sizes) <- object$model_bitstrings
  if (is.null(ylim)) {
    ylim <- c(0, max(sizes) * 1.15)
  } else {
    ylim <- .imr_check_numeric_vector(ylim, "ylim", length = 2)
    if (ylim[1] >= ylim[2]) {
      .imr_abort("`ylim` must be increasing.")
    }
  }

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)
  pp <- .imr_plot_par(mar, mgp, default_mar = c(4.8, 4.8, 3.2, 1))
  graphics::par(mar = pp$mar, mgp = pp$mgp)
  if (is.null(dots$border)) dots$border <- NA
  bp <- do.call(graphics::barplot, c(list(
    height = sizes, xlab = "Availability subgroup (bitstring)",
    ylab = "Number of subjects", main = "Subjects per availability subgroup",
    col = .imr_plot_palette(length(sizes), col = col, palette = palette),
    ylim = ylim, cex.axis = sz$axis,
    cex.lab = sz$lab, cex.main = sz$main
  ), dots))
  if (show_values) {
    graphics::text(bp, sizes, labels = sizes, pos = 3, cex = sz$values)
  }
  invisible(sizes)
}
