## Shared base-graphics helpers used by the IMR plotting functions.

#' @keywords internal
#' @noRd
.imr_plot_take_alias <- function(dots, alias, value) {
  if (!is.null(dots[[alias]])) {
    value <- dots[[alias]]
    dots[[alias]] <- NULL
  }
  list(value = value, dots = dots)
}

#' @keywords internal
#' @noRd
.imr_plot_cex <- function(dots, base_cex = 1, cex_axis = NULL,
                          cex_lab = NULL, cex_main = NULL,
                          cex_names = NULL, cex_legend = NULL,
                          cex_values = NULL,
                          defaults = list(axis = 1, lab = 1.15,
                                          main = 1.2, names = 1,
                                          legend = 0.95, values = 0.95)) {
  base_cex <- .imr_check_numeric_vector(
    base_cex, "base_cex", length = 1, positive = TRUE
  )

  r <- .imr_plot_take_alias(dots, "cex.axis", cex_axis)
  cex_axis <- r$value; dots <- r$dots
  r <- .imr_plot_take_alias(dots, "cex.lab", cex_lab)
  cex_lab <- r$value; dots <- r$dots
  r <- .imr_plot_take_alias(dots, "cex.main", cex_main)
  cex_main <- r$value; dots <- r$dots
  r <- .imr_plot_take_alias(dots, "cex.names", cex_names)
  cex_names <- r$value; dots <- r$dots
  r <- .imr_plot_take_alias(dots, "cex.legend", cex_legend)
  cex_legend <- r$value; dots <- r$dots
  r <- .imr_plot_take_alias(dots, "cex.values", cex_values)
  cex_values <- r$value; dots <- r$dots

  out <- list(
    axis = if (is.null(cex_axis)) defaults$axis * base_cex else cex_axis,
    lab = if (is.null(cex_lab)) defaults$lab * base_cex else cex_lab,
    main = if (is.null(cex_main)) defaults$main * base_cex else cex_main,
    names = if (is.null(cex_names)) defaults$names * base_cex else cex_names,
    legend = if (is.null(cex_legend)) {
      defaults$legend * base_cex
    } else {
      cex_legend
    },
    values = if (is.null(cex_values)) {
      defaults$values * base_cex
    } else {
      cex_values
    },
    dots = dots
  )

  for (nm in setdiff(names(out), "dots")) {
    out[[nm]] <- .imr_check_numeric_vector(
      out[[nm]], paste0("cex_", nm), length = 1, positive = TRUE
    )
  }
  out
}

#' @keywords internal
#' @noRd
.imr_plot_par <- function(mar, mgp, default_mar, default_mgp = c(2.7, 0.8, 0)) {
  if (is.null(mar)) mar <- default_mar
  if (is.null(mgp)) mgp <- default_mgp
  mar <- .imr_check_numeric_vector(
    mar, "mar", length = 4, nonnegative = TRUE
  )
  mgp <- .imr_check_numeric_vector(
    mgp, "mgp", length = 3, nonnegative = TRUE
  )
  list(mar = mar, mgp = mgp)
}

#' @keywords internal
#' @noRd
.imr_plot_palette <- function(n, col = NULL, palette = "Dark 3") {
  if (n < 1L) return(character(0))
  if (!is.null(col)) {
    if (!is.atomic(col) || length(col) == 0L) {
      .imr_abort("`col` must contain at least one colour.")
    }
    return(rep_len(col, n))
  }
  if (!is.character(palette) || length(palette) != 1L || is.na(palette)) {
    .imr_abort("`palette` must be a single character string.")
  }
  switch(tolower(palette),
    platform = rep_len(.imr_plot_platform_colours(), n),
    platforms = rep_len(.imr_plot_platform_colours(), n),
    subgroup = grDevices::colorRampPalette(.imr_plot_subgroup_colours())(n),
    subgroups = grDevices::colorRampPalette(.imr_plot_subgroup_colours())(n),
    heatmap = grDevices::colorRampPalette(.imr_plot_heatmap_colours())(n),
    grey = .imr_plot_greys(n),
    gray = .imr_plot_greys(n),
    grDevices::hcl.colors(n, palette = palette)
  )
}

#' @keywords internal
#' @noRd
.imr_plot_greys <- function(n = 64) {
  grDevices::colorRampPalette(c("#FAFAFA", "#1F1F1F"))(n)
}

#' @keywords internal
#' @noRd
.imr_plot_platform_colours <- function() {
  ## Muted academic palette: distinct, print-friendly, and colourblind aware.
  c("#2F5D7C", "#8A5A44", "#557A60", "#6D5A7D",
    "#8A7D4E", "#5B7E91", "#6F6F6F", "#3F4A52")
}

#' @keywords internal
#' @noRd
.imr_plot_subgroup_colours <- function() {
  c("#D9E2E7", "#A7B9C6", "#5F7F99")
}

#' @keywords internal
#' @noRd
.imr_plot_heatmap_colours <- function() {
  c("#F9FAFA", "#E2E9ED", "#A8B9C6", "#4E6475")
}

#' @keywords internal
#' @noRd
.imr_plot_reference_colour <- function() {
  "#707070"
}

#' @keywords internal
#' @noRd
.imr_plot_trace_colour <- function() {
  "#2B2B2B"
}
