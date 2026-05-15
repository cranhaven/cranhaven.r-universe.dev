#' Plot model-implied vs empirical item curves by latent factor (single- or multi-group)
#'
#' @description
#' Given the output list of `item_data()`, draw smooth curves of the
#' model-implied relation and the empirical GAM-based relation between a chosen
#' latent factor (x-axis) and all items that load on it (y-axis). Works for
#' continuous, ordinal, and mixed models; supports single- and multi-group fits.
#'
#' @param x A list returned by `item_data()`.
#' @param latent Character scalar: latent factor ID shown on x-axis.
#' @param items Optional character vector of item names to include; inferred if `NULL`.
#' @param show_model,show_empirical,show_points Logical toggles to draw the model curve,
#'   the empirical curve, and raw datapoints (all `TRUE` by default).
#' @param show_metrics Logical: print per-item fit metrics inside each facet (default `TRUE`).
#' @param metrics_pad Additional top padding for facets when `show_metrics = TRUE`.
#'   Controls the gap between metric labels and the plotting area (default `0.05`).
#' @param point_size Numeric size of points (defaults chosen for legibility).
#' @param line_width Numeric linewidth for model/empirical curves (defaults chosen for legibility).
#' @param color_points,color_model,color_empirical Colors for points, model curve/ribbon, empirical curve/ribbon
#'   (defaults chosen for legibility).
#' @param alpha_points,alpha_ribbon Alphas for points and ribbons (defaults chosen for legibility).
#' @param jitter_sd Vertical jitter SD (on the data scale); normally distributed.
#'   The applied jitter is truncated to ±3·SD to avoid extreme outliers.
#' @param jitter_seed Optional integer seed for deterministic vertical jitter.
#' @param sample_frac Optional fraction in (0,1] to thin raw datapoints before plotting (default 1 = no thinning).
#' @param point_shape Integer/character ggplot2 shape for points (choose a fast filled shape, default 16).
#' @param ribbons Logical: draw CI ribbons when available (default `TRUE`).
#' @param penalized Logical: use penalized metrics in facet captions (default `FALSE`).
#' @param facet One of `"wrap"`, `"grid"`. Multi-group inputs always use grid.
#' @param ncol,nrow Optional layout hints used when `facet = "wrap"`.
#' @param scales Facet scales (passed to ggplot2 facets). Default `"fixed"`.
#' @param sort Item ordering by a metric; one of `"none"`, `"r2"`, `"r2_pen"`,
#'   `"rmse"`, `"rmse_pen"`, `"mae"`, `"mae_pen"` (default `"none"`). For multi-group,
#'   the metric is averaged across groups.
#' @param sort_dir Direction of sort if `sort != "none"`; one of `"auto"`,
#'   `"asc"`, `"desc"`. `auto` sorts R²-like metrics descending and error metrics ascending.
#'
#' @return A `ggplot2` object (single-group) or a faceted `ggplot2` object (multi-group).
#'   The plot is returned (and printed if not assigned) and can be further modified with `+`.
#'
#' @examples
#' \donttest{
#' HS.model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' fit <- lavaan::cfa(HS.model,
#'                    data = lavaan::HolzingerSwineford1939,
#'                    meanstructure = TRUE)
#' idata <- item_data(fit)
#' item_plot(idata, latent = 'visual')
#'
#' # Multi-group example (facet grid, sort by R2)
#' fit_mg <- lavaan::cfa(HS.model,
#'                       data = lavaan::HolzingerSwineford1939,
#'                       group = 'school', meanstructure = TRUE)
#' idata_mg <- item_data(fit_mg)
#' item_plot(idata_mg, latent = 'visual', facet = 'grid', sort = 'r2')
#' }
#'
#' @seealso \link{item_data}, \link{augment}, \link{prepare}
#' @family lavDiag-visualization
#' @export
item_plot <- function(
    x,
    latent,
    items           = NULL,
    show_model      = TRUE,
    show_empirical  = TRUE,
    show_points     = TRUE,
    show_metrics    = TRUE,
    metrics_pad     = 0.05,
    point_size      = 1.2,
    line_width      = 0.9,
    color_points    = "black",
    color_model     = "blue",
    color_empirical = "red",
    alpha_points    = 0.05,
    alpha_ribbon    = 0.20,
    jitter_sd       = 0.05,
    jitter_seed     = NULL,
    sample_frac     = 1,
    point_shape     = 16,
    ribbons         = TRUE,
    penalized       = FALSE,
    facet           = c("wrap","grid"),
    ncol            = NULL,
    nrow            = NULL,
    scales          = c("fixed","free_y","free","free_x"),
    sort            = c("none","r2","r2_pen","rmse","rmse_pen","mae","mae_pen"),
    sort_dir        = c("auto","asc","desc")
) {
  facet  <- match.arg(facet)
  scales <- match.arg(scales)
  sort_dir <- match.arg(sort_dir)
  sort <- match.arg(sort)
  stopifnot(is.null(jitter_seed) || (is.numeric(jitter_seed) && length(jitter_seed) == 1L && is.finite(jitter_seed)))
  if (!is.numeric(sample_frac) || length(sample_frac) != 1L || !is.finite(sample_frac) || sample_frac <= 0) sample_frac <- 1
  sample_frac <- min(sample_frac, 1)
  if (!is.numeric(metrics_pad) || length(metrics_pad) != 1L || !is.finite(metrics_pad) || metrics_pad < 0) metrics_pad <- 0.05

  stopifnot(is.list(x), "original_data" %in% names(x))
  orig <- x$original_data
  new  <- x$new_data
  mets <- x$metrics
  if (is.null(new)) stop("x$new_data is NULL.")
  if (!latent %in% names(new)) stop(sprintf("Latent '%s' not found in new_data.", latent))

  # Infer item list from prediction columns if not supplied
  if (is.null(items)) {
    rx <- paste0(
      "^(?:m|e)_(?:est|lwr|upr)_([^_]+)_",
      stringr::str_replace_all(latent, "([\\.^$|()\\[\\]{}*+?])", "\\\\\\1"),
      "$")
    cand  <- names(new)[stringr::str_detect(names(new), rx)]
    items <- sort(unique(stats::na.omit(stringr::str_match(cand, rx)[, 2])))
    if (!length(items)) stop("No <item>_<latent> prediction columns found in new_data for the requested latent.")
  }

  # Optional: sort items by a chosen metric (averaged across groups when present)
  if (!identical(sort, "none") && !is.null(mets) && nrow(mets)) {
    metric <- sort
    agg <- mets |>
      dplyr::filter(.data$item %in% items) |>
      dplyr::group_by(.data$item) |>
      dplyr::summarise(score = mean(.data[[metric]], na.rm = TRUE), .groups = "drop")
    auto_desc <- grepl("^r2", metric)
    dir <- switch(sort_dir, auto = if (auto_desc) "desc" else "asc", asc = "asc", desc = "desc")
    agg <- if (identical(dir, "desc")) dplyr::arrange(agg, dplyr::desc(.data$score)) else dplyr::arrange(agg, .data$score)
    items <- intersect(agg$item, items)
  }

  new_lat <- dplyr::filter(new, .data$.latent_var %in% latent)
  if (!nrow(new_lat)) stop("No rows for the requested latent.")

  escaped_lat <- stringr::str_replace_all(latent, "([\\.^$|()\\[\\]{}*+?])", "\\\\\\1")
  cols_rx  <- paste0("^(?:m|e)_(?:est|lwr|upr)_[^_]+_", escaped_lat, "$")
  name_rx  <- paste0("^(m|e)_(est|lwr|upr)_([^_]+)_", escaped_lat, "$")

  core_cols <- c(".rid", ".gid", ".group", ".latent_var")
  pred_cols <- grep(cols_rx, names(new_lat), value = TRUE, perl = TRUE)
  if (!length(pred_cols)) stop("No prediction columns found for the chosen latent in new_data.")

  curves <- new_lat |>
    dplyr::mutate(x = .data[[latent]]) |>
    dplyr::select(dplyr::any_of(c(core_cols, "x")), tidyselect::all_of(pred_cols)) |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(pred_cols),
      names_to = c("src", "stat", "item"),
      names_pattern = name_rx,
      values_to = "val"
    ) |>
    tidyr::pivot_wider(
      names_from  = c("src", "stat"),
      values_from = "val",
      names_glue  = "{src}_{stat}"
    ) |>
    dplyr::mutate(
      .group = dplyr::if_else(is.na(.data$.group) & is.finite(.data$.gid), as.character(.data$.gid), .data$.group)
    )

  if (".group" %in% names(curves)) {
    ug <- unique(stats::na.omit(curves$.group))
    if (length(ug) <= 1L) curves$.group <- NULL
  }

  # Apply item ordering to plotting data if requested
  if (length(items) && "item" %in% names(curves)) {
    curves$item <- factor(curves$item, levels = items)
  }

  keep_items <- intersect(items, names(orig))
  if (length(keep_items) == 0L) show_points <- FALSE

  # Prepare raw points (optionally stratified thinning and safe jitter for ordinal)
  if (isTRUE(show_points)) {
    pts_long <- orig |>
      dplyr::select(dplyr::any_of(c(".gid", ".group", latent, keep_items))) |>
      tidyr::pivot_longer(cols = tidyselect::all_of(keep_items), names_to = "item", values_to = "y") |>
      dplyr::mutate(item = if (length(items)) factor(.data$item, levels = items) else .data$item)

    if (sample_frac < 1 && nrow(pts_long) > 0) {
      # Stratified thinning to preserve balance across .group × item (and .gid if available)
      grp_vars <- intersect(c(".gid", ".group", "item"), names(pts_long))
      pts_long <- pts_long |>
        dplyr::group_by(dplyr::across(tidyselect::all_of(grp_vars))) |>
        dplyr::slice_sample(prop = sample_frac) |>
        dplyr::ungroup()
    }

    if (".group" %in% names(curves)) {
      map_tbl <- dplyr::distinct(curves, dplyr::across(dplyr::all_of(c(".gid", ".group"))))
      if (".group" %in% names(pts_long)) pts_long <- dplyr::select(pts_long, - .group)
      pts_long <- dplyr::left_join(pts_long, map_tbl, by = ".gid")
    }

    # Safe numeric jitter (handles ordered factors) with ±3·SD truncation, RNG-local
    jitter_draw <- function(n, sd) {
      eps <- stats::rnorm(n, sd = sd)
      lim <- 3 * sd
      if (is.finite(lim) && lim > 0) eps <- pmax(pmin(eps, lim), -lim)
      eps
    }

    if (nrow(pts_long) > 0) {
      y_num <- if (is.numeric(pts_long$y)) pts_long$y else as.numeric(pts_long$y)
      eps <- if (!is.null(jitter_seed)) withr::with_seed(jitter_seed, jitter_draw(nrow(pts_long), jitter_sd)) else jitter_draw(nrow(pts_long), jitter_sd)
      pts_long$y_jit <- y_num + eps
    } else {
      pts_long$y_jit <- numeric(0)
    }
  } else {
    pts_long <- NULL
  }

  p <- ggplot2::ggplot(curves, ggplot2::aes(x = x))

  if (isTRUE(show_points) && !is.null(pts_long) && nrow(pts_long) > 0) {
    p <- p + ggplot2::geom_point(
      data = pts_long,
      ggplot2::aes(x = !!rlang::sym(latent), y = .data$y_jit),
      inherit.aes = FALSE,
      color = color_points,
      alpha = alpha_points,
      size = point_size,
      shape = point_shape,
      show.legend = FALSE,
      position = ggplot2::position_identity()
    )
  }

  if (isTRUE(show_model)) {
    if (isTRUE(ribbons) && any(!is.na(curves$m_lwr)))
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$m_lwr, ymax = .data$m_upr, fill = "Model"), alpha = alpha_ribbon)
    p <- p + ggplot2::geom_line(ggplot2::aes(y = .data$m_est, color = "Model"), linewidth = line_width)
  }

  if (isTRUE(show_empirical)) {
    if (isTRUE(ribbons) && any(!is.na(curves$e_lwr)))
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$e_lwr, ymax = .data$e_upr, fill = "Empirical"), alpha = alpha_ribbon)
    p <- p + ggplot2::geom_line(ggplot2::aes(y = .data$e_est, color = "Empirical"), linewidth = line_width)
  }

  if (isTRUE(show_metrics) && !is.null(mets) && nrow(mets)) {
    met_cols <- if (isTRUE(penalized)) c("r2_pen", "rmse_pen", "mae_pen") else c("r2", "rmse", "mae")
    lab_tbl <- mets |>
      dplyr::filter(.data$item %in% items) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(met_cols),
                                  ~ ifelse(is.na(.x), NA_character_, sprintf("%.3f", .x))) ) |>
      dplyr::transmute(
        .gid = .data$.gid,
        .group = .data$.group,
        item = .data$item,
        cap = paste0(
          "italic(R)^2*\" = ", .data$r2, "\"*\", \"~",
          "plain(\"RMSE\")*\" = ", .data$rmse, "\"*\", \"~",
          "plain(\"MAE\")*\" = ", .data$mae,  "\"*\".\""
        )
      )

    if (".group" %in% names(curves)) {
      xr  <- curves |>
        dplyr::group_by(.data$.group, .data$item) |>
        dplyr::summarise(x_mid = stats::median(.data$x, na.rm = TRUE), .groups = "drop")
      ann <- dplyr::left_join(lab_tbl, xr, by = c(".group" = ".group", "item" = "item"))
    } else {
      xr  <- curves |>
        dplyr::group_by(.data$item) |>
        dplyr::summarise(x_mid = stats::median(.data$x, na.rm = TRUE), .groups = "drop")
      ann <- dplyr::left_join(lab_tbl, xr, by = dplyr::join_by(item))
    }

    p <- p + ggplot2::geom_text(
      data = ann,
      ggplot2::aes(x = .data$x_mid, y = Inf, label = .data$cap),
      parse = TRUE,
      inherit.aes = FALSE,
      hjust = 0.5,
      vjust = 1.3
    ) + ggplot2::coord_cartesian(clip = "off")
  }

  # Compute dynamic top padding: add `metrics_pad` only when metrics are drawn
  top_pad <- if (isTRUE(show_metrics) && !is.null(mets) && nrow(mets)) 0.15 + metrics_pad else 0.05

  p <- p +
    ggplot2::scale_color_manual(name = "Predictions",
                                values = c(Model = color_model, Empirical = color_empirical),
                                breaks = c("Model","Empirical"),
                                limits = c("Model","Empirical")) +
    ggplot2::scale_fill_manual(name = "Predictions",
                               values = c(Model = color_model, Empirical = color_empirical),
                               breaks = c("Model","Empirical"),
                               limits = c("Model","Empirical")) +
    ggplot2::labs(x = latent, y = NULL) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, top_pad))) +
    ggplot2::theme(legend.position = "top",
                   plot.margin = ggplot2::margin(10, 10, 20, 10))

  if (".group" %in% names(curves)) {
    p <- p + ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym("item")), cols = ggplot2::vars(!!rlang::sym(".group")), scales = scales)
  } else {
    if (identical(facet, "grid")) {
      p <- p + ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym("item")), scales = scales)
    } else {
      if (is.null(ncol) && is.null(nrow)) {
        n_items <- length(unique(curves$item))
        ncol <- ceiling(sqrt(n_items))
      }
      p <- p + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym("item")), scales = scales, ncol = ncol, nrow = nrow)
    }
  }

  return(p)
}

utils::globalVariables(c(".group", "item"))



