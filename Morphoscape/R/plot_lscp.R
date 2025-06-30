plot.grp_Wprime <- function(x, alpha = 1, pt.col = "black", interpolate = TRUE, contour = TRUE, ...) {
  plot.wtd_lscp(x$Wprime, alpha = alpha, pt.col = pt.col,
                interpolate = interpolate, contour = contour,
                ...)
}

plot.by_Wprime <- function(x, level, ncol = 1, alpha = 1, pt.col = "black", interpolate = TRUE, contour = TRUE, ...) {
  if (missing(level)) by_levels <- names(x$grp_Wprimes)
  else if (!is.atomic(level) || !all(as.character(level) %in% names(x$grp_Wprimes))) {
    stop(sprintf("'level' must be a vector of levels of %s for which to display the optimal landscapes.",
                 attr(x$by, "by_name")), call. = FALSE)
  }
  else by_levels <- level
  
  # grid <- x$grp_Wprime[[1]]$Wprime$Wprime$grid[c("x", "y")]
  
  data <- do.call("rbind", lapply(by_levels, function(i) {
    cbind(x$grp_Wprimes[[i]]$Wprime$Wprime$grid[c("x", "y", "Z")],
          .level = i)
  }))
  
  new_data <- do.call("rbind", lapply(by_levels, function(i) {
    cbind(x$grp_Wprime[[i]]$Wprime$Wprime$new_data[c("x", "y", "Z")],
          .level = i, .class = x$by)
  }))
  
  p <- ggplot() +
    geom_raster(data = data,
                mapping = aes(x = .data$x, y = .data$y, fill = .data$Z),
                interpolate = interpolate) +
    scale_fill_continuous(name = NULL, type = "viridis") +
    coord_fixed(expand = FALSE) #uncomment to make the x and y axis the same scale (square pixels)
  
  if (contour) {
    p <- p  +
      geom_contour(data = data,
                   mapping = aes(x = .data$x, y = .data$y, z = .data$Z),
                   color = "white", bins = 5, alpha = .7)
  }
  
  p <- p + geom_point(data = new_data,
                      mapping = aes(x = .data$x, y = .data$y, color = .data$.class),
                      alpha = alpha, size = 1) +
    facet_wrap(".level", ncol = ncol) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = "black"),
          panel.grid = element_blank()) +
    labs(color = attr(x$by, "by_name"))
  
  p
}

plot.wtd_lscp <- function(x, alpha = 1, pt.col = "black", interpolate = TRUE, contour = TRUE, ...) {
  plot_lscp(grid = x$Wprime$grid[c("x", "y", "Z")],
            interpolate = interpolate, contour = contour,
            new_data = x$Wprime$new_data, alpha = alpha,
            pt.col = pt.col, ...)
}

plot_lscp <- function(grid, interpolate = TRUE, contour = TRUE, new_data = NULL, alpha = 1, pt.col = "black", ...) {
  
  p <- ggplot() +
    geom_raster(data = grid,
                mapping = aes(x = .data$x, y = .data$y, fill = .data$Z),
                interpolate = interpolate) +
    scale_fill_continuous(name = NULL, type = "viridis") +
    coord_fixed(expand = FALSE) #uncomment to make the x and y axis the same scale (square pixels)
  
  if (contour) {
    p <- p  +
      geom_contour(data = grid,
                   mapping = aes(x = .data$x, y = .data$y, z = .data$Z),
                   color = "white", bins = 5, alpha = .7)
  }
  if (!is.null(new_data)) {
    p <- p + geom_point(data = new_data[c("x", "y")],
                        mapping = aes(x = .data$x, y = .data$y),
                        fill = pt.col, alpha = alpha, size = 1) 
  }
  
  p <- p +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = "black"),
          panel.grid = element_blank())
  
  p
}
