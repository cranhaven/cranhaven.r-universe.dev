# plotting functions
# ---

#' @title Plot distance matrix
#'
#' @description
#' Plot a heatmap of the distance matrix using \link[ggplot2]{geom_tile}.
#'
#' @param DM a distance matrix
#' @param col_palette a colour palette, previously it was set to the `spectral`
#'   palette of \link[RColorBrewer]{brewer.pal} but now it is, by default, to the
#'   color-blind firendly \link[viridis]{viridis}.
#'
#' @param log_scale logical. Default FALSE
#' @param cex numerical value by which text should be magnified (default font
#'   size in \link[ggplot2]{theme} is 11)
#' @param show_dendro If the dendrogram resulting from \link[stats]{hclust} should
#'   be shown. Default TRUE
#' @param title Title of the plot passed to \link[ggplot2]{labs}. No title by default.
#' @importFrom ggplot2 ggplot aes element_text element_blank element_line unit theme labs guide_colourbar
#' @importFrom rlang .data
#' @importFrom viridis viridis
#' @return plot \link[ggplot2]{ggplot}
#' @keywords plot; distance matrix; heatmap
#' @export
plot_distance_matrix <- function(DM, col_palette = viridis(n = 11),
                        log_scale = FALSE, cex = 1, show_dendro = TRUE,
                        title = "") {

  # This assumes that the input is a matrix
  if (is.matrix(DM)) {
    if (is.null(rownames(DM))) {
      # assign numerical labels
      rownames(DM) <- 1:dim(DM)[1]
    }
    if (is.null(colnames(DM))) {
      # assign numerical labels
      colnames(DM) <- 1:dim(DM)[2]
    }
  }
  # convert object of class [stats::hclust] to class [stats::dendrogram]
  dd_col <- stats::as.dendrogram(stats::hclust(stats::as.dist(DM)))
  # order the labels of the leaves in the dendrogram
  col_ord <- stats::order.dendrogram(dd_col)
  # same for rows
  dd_row <- stats::as.dendrogram(stats::hclust(stats::as.dist((DM))))
  row_ord <- stats::order.dendrogram(dd_row)

  # re-order the distance matrix
  DM_ord <- DM[row_ord, col_ord]
  DM_ord_names <- attr(DM_ord, "dimnames")
  if (requireNamespace("tidyr", quietly = TRUE)) {
    df <- tidyr::as_tibble(DM_ord, rownames = "from")
    df$from <- factor(df$from, levels = df$from, ordered = TRUE)
    mdf <- tidyr::pivot_longer(df, names_to = "to",
      names_ptypes = list("to" = factor()),
      values_to = "value", -.data$from)

  } else {
    df <- as.data.frame(DM_ord)
    colnames(df) <- DM_ord_names[[2]]
    df$from <- factor(DM_ord_names[[1]], levels = DM_ord_names[[1]], ordered = TRUE)
    mdf <- reshape2::melt(df, id.vars = "from")
    colnames(mdf)[2] <- "to"
  }
  # ---
  # Extract line segment and label data from stats::dendrogram() or stats::hclust() object,
  # resulting in a list of data frames containing line segment data and label data.
  ddata_x <- ggdendro::dendro_data(dd_row)
  ddata_y <- ggdendro::dendro_data(dd_col)
  # ---
  # Building plot(s)
  default_theme <- ggplot2::theme_get()
  # ggplot2::theme_update(
  #   axis.text = ggplot2::element_text(size = default_theme$axis.text$size * cex),
  #   legend.text = ggplot2::element_text(size = default_theme$legend.text$size * cex)
  #   )
  ## Create plot components
  ### Heatmap
  if (!log_scale) {
    p1 <- ggplot(mdf, aes(x = .data$to, y = .data$from)) +
      # ggplot2::geom_tile(aes(fill = .data$value)) +
      ggplot2::geom_raster(aes(fill = .data$value)) +
      ggplot2::coord_equal() +
      ggplot2::scale_fill_gradientn(colours = col_palette, guide = "none") +
      ggplot2::guides(
        fill = guide_colourbar(barheight = default_theme$legend.key.size * 10, nbin = 100)
      ) +
      labs(x = "", y = "", fill = "distance", title = title) +
      theme(
        panel.background = element_blank(),
        legend.position = "right",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt"),
        axis.text = element_text(size = default_theme$axis.text$size * cex),
        legend.text = element_text(size = default_theme$legend.text$size * cex),
        legend.title = element_text(size = default_theme$legend.text$size * (cex + .4)),
        plot.title = element_text(size = default_theme$plot.title$size * cex),
        )
  } else {
    p1 <- ggplot(mdf, aes(x = .data$to, y = .data$from)) +
      ggplot2::geom_tile(aes(fill = log10(.data$value))) +
      ggplot2::coord_equal() +
      ggplot2::scale_fill_gradientn(colours = col_palette, guide = "none") +
      ggplot2::guides(
        fill = guide_colourbar(barheight = 20, nbin = 100)
      ) +
      labs(x = "", y = "", fill = "distance", title = title) +
      theme(
        panel.background = element_blank(),
        legend.position = "right",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt"),
        axis.text = element_text(size = default_theme$axis.text$size * cex),
        legend.text = element_text(size = default_theme$legend.text$size * cex),
        legend.title = element_text(size = default_theme$legend.text$size * (cex + 0.3)),
        plot.title = element_text(size = default_theme$plot.title$size * cex),
      )
  }

  # Dendrogram 1
  p2 <- ggplot(ggdendro::segment(ddata_x)) +
    ggplot2::geom_segment(aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)) +
    ggplot2::theme_void()

  # Dendrogram 2
  p3 <- ggplot(ggdendro::segment(ddata_y)) +
    ggplot2::geom_segment(aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)) +
    ggplot2::scale_x_discrete(expand = c(0.02, 0.02)) +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_flip() +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 0, 2, 2, "pt"))

  if (show_dendro) {
    # grDevices::dev.new(width = 16, height = 9)
    if (requireNamespace("cowplot", quietly = TRUE)) {
      cowplot::plot_grid(p3, p1, align = "h", axis = "bt",
        rel_widths = c(.3, 1))
    } else {
      warning("Consider installing the package cowplot which allows an easier and better alignement.")
      grid::grid.newpage()
      print(p3, vp = grid::viewport(
        width = 0.2, height = .925, x = 0.2, y = 0.505)
      )
      #print(p2, vp=viewport(0.74, 0.2, x=0.46, y=0.9))
      print(p1, vp = grid::viewport(
        width = 0.8, height = 1, x = 0.62, y = 0.5)
      )
    }
  } else {
    return(p1)
  }
}

#' @title Plot distance matrix as heatmap
#' @description
#' `r lifecycle::badge("deprecated")`
#' @param DM a distance matrix
#' @param colPalette `r lifecycle::badge("deprecated")` in the new function \link{plot_distance_matrix}
#' it is called \code{col_palette}
#' @param log.scale `r lifecycle::badge("deprecated")` in the new function \link{plot_distance_matrix}
#' it is called \code{log_scale}
#' @param cex numerical value by which text should be magnified (default font
#'   size in [ggplot2:theme] is 11)
#' @param showDendrogram `r lifecycle::badge("deprecated")` in the new function \link{plot_distance_matrix}
#' it is called \code{show_dendro}
#' @param title Title of the plot passed to \link[ggplot2]{labs}. No title by
#'   default
#' @return a \link[ggplot2]{ggplot}
#' @export
plotHeatmap <- function(DM, colPalette = NULL,
                        log.scale = FALSE, cex = 1, showDendrogram = TRUE, title = "") {
  .Deprecated("plot_distance_matrix")
  if (is.null(colPalette)) {
    colPalette <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
  } else {
    colPalette <- viridis(n = 11)
  }
  return(
    plot_distance_matrix(DM, col_palette = colPalette, log_scale = log.scale, cex = cex,
                         show_dendro = showDendrogram, title = title)
    )
}
