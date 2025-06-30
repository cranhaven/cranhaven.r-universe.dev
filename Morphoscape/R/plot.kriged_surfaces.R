plot.kriged_surfaces <- function(x, alpha = .5, pt.col = "black", interpolate = TRUE, contour = TRUE, ...) {
  fn_grid <- x$dataframes$grid

  fn_grid_long <- reshape(fn_grid, direction = "long", varying = -(1:2), v.names = "stat",
                          timevar = "func", times = names(fn_grid)[-(1:2)])
  
  p <- ggplot() +
    geom_raster(data = fn_grid_long, mapping = aes(x = .data$x, y = .data$y, fill = .data$stat),
                interpolate = interpolate) +
    facet_wrap(~func) +
    scale_fill_continuous(name = NULL, type = "viridis") +
    # coord_cartesian(expand = FALSE) +
    coord_fixed(expand = FALSE) + #uncomment to make the x and y axis the same scale (square pixels)
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = "black"),
          panel.grid = element_blank())
  if (contour) {
    p <- p  +
      geom_contour(data = fn_grid_long, mapping = aes(x = .data$x, y = .data$y, z = .data$stat),
                   color = "white", bins = 5, alpha = .7)
  }
  if (!is.null(fn_new_data <- x$dataframes$new_data)) {
    fn_new_data <- fn_new_data[names(fn_grid)]
    fn_new_data_long <- reshape(fn_new_data, direction = "long", varying = -(1:2), v.names = "stat",
                                timevar = "func", times = names(fn_new_data)[-(1:2)])
    
    p <- p + geom_point(data = fn_new_data_long, mapping = aes(x = .data$x, y = .data$y),
                        fill = pt.col, alpha = alpha, size = 1)
  }
  p
}
