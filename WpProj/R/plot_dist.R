
plot.distcompare <- function(x = NULL, models = NULL, ylim = NULL, ylabs = c(NULL,NULL),
                             xlab = NULL, xlim = NULL,
                             linesize = 0.5, pointsize = 1.5, facet.group = NULL, ...) {
  distance <- x
  mc <- match.call(expand.dots = TRUE)
  if(is.null(distance)) {
    distance <- distCompare(models, ...)
  }
  if (!inherits(distance, "distcompare")) stop("distance must be output of distCompare function")
  
  ppost <- pmean <- NULL
  
  if(is.null(xlab)) xlab <- "Number of active coefficients"
  
  if ( !is.null(distance$parameters) ) {
    ylim_post <- set_y_limits(distance, ylim, "parameters")
    xlim_post <- set_x_limits(distance, xlim, "parameters")
    
    
    nactive <- dist <- groups <- NULL
    ppost <- ggplot2::ggplot( distance$parameters, 
                              ggplot2::aes(x=nactive, y=dist, color = groups, group=groups )) +
      ggplot2::geom_line(size = linesize, position = ggplot2::position_dodge(width = 0.25)) + 
      ggplot2::geom_point(size = pointsize, position = ggplot2::position_dodge(width = 0.25)) + 
      ggsci::scale_color_jama() + 
      ggplot2::labs(color ="Method") +
      ggplot2::xlab(xlab) + 
      ggplot2::ylab(ylabs[1]) + ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = xlim_post) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = ylim_post )
    if(!is.null(facet.group)) {
      ppost <- ppost + ggplot2::facet_grid(facet.group)
    }
  }
  
  if (!is.null(distance$predictions)){
    ylim_mean <- set_y_limits(distance, ylim, "predictions")
    xlim_mean <- set_x_limits(distance, xlim, "predictions")
    pmean <- ggplot2::ggplot( distance$predictions, 
                              ggplot2::aes(x=nactive, y=dist, color = groups, group=groups )) +
      ggplot2::geom_line(size = linesize, position = ggplot2::position_dodge(width = 0.25)) + 
      ggplot2::geom_point(size = pointsize, position = ggplot2::position_dodge(width = 0.25)) +
      ggsci::scale_color_jama() + 
      ggplot2::labs(color ="Method") +
      ggplot2::xlab(xlab) + 
      ggplot2::ylab(ylabs[length(ylabs)]) + ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = xlim_mean) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = ylim_mean )
    if(!is.null(facet.group)) {
      pmean <- pmean + ggplot2::facet_grid(facet.group)
    }
  }
  
  plots <- list(parameters = ppost, predictions = pmean)
  class(plots) <- c("plotcompare","WpProj")
  return(plots)
}

methods::setClass("plotcompare")

print.plotcompare <- function(x) {
  for(i in 1:length(x)) {
    if(is.null(x[[i]])) next
    print(x[[i]])
  }
}

set_y_limits <- function(distance_data, ylim, quantity){
  idx <- if (quantity == "parameters"){
    1L
  } else if (quantity == "predictions") {
    2L
  }
  
  if (!is.null(ylim)) {
    if (is.numeric(ylim)){
      if (length(ylim) == 4){
        return(ylim[(idx-1)*2 + 1:2])
      } else {
        return(ylim)
      }
    } 
    if (is.list(ylim) & !is.null(ylim[[idx]])) return(ylim[[idx]])
  } 
  # else {
  #   return(NULL)
  # }
  df <- distance_data
  if (is.null(df)) return(NULL)
  if (is.null(df$dist)) return(NULL)
  range.size <- max(df$hi - df$low)
  add.factor <- range.size * 1.2 - range.size
  min_y <- max(0, min(df$low) - add.factor)
  max_y <- max(df$hi) + add.factor
  max_y <- max(df$hi) * 1.1
  ylim <- c(min_y, max_y)
  return(ylim)
}


set_x_limits <- function(distance_data, xlim, quantity){
  idx <- if (quantity == "parameters"){
    1L
  } else if (quantity == "predictions") {
    2L
  }
  
  if (!is.null(xlim)) {
    if (is.numeric(xlim)){
      if (length(xlim) == 4){
        return(xlim[(idx-1)*2 + 1:2])
      } else {
        return(xlim)
      }
    } 
    if (is.list(xlim) & !is.null(xlim[[idx]])) return(xlim[[idx]])
  } 
  # else {
  #   return(NULL)
  # }
  
  df <- distance_data
  if (is.null(df)) return(NULL)
  if (is.null(df$nzero)) return(NULL)
  min_x <- min(df$nactive)
  max_x <- max(df$nactive)
  xlim <- c(min_x, max_x)
  return(xlim)
}

set_equal_y_limits.plotcompare <- function(x){
  # dist.list <- list(dist = unlist(sapply(distance_data, function(x) x[[quantity]]$data$dist )))
  x <- distance_data
  dist <- ylim <- list(parameters = NULL, predictions = NULL)
  for(i in c("parameters", "predictions")){
    dist[[i]] <- list(dist = unlist(sapply(distance_data, function(x) x[[i]]$data$dist )))
    ylim[[i]] <- set_y_limits(dist, ylim[[i]], i)
  }
  for(j in seq_along(distance_data)) {
    for(i in c("parameters", "predictions")) {
      distance_data[[j]][[i]] <- distance_data[[j]][[i]] + ggplot2::scale_y_continuous(expand = c(0, 0), limits = ylim[[i]] )
    }
  }
  return(distance_data)
}

#' Plot `distcompare` Objects
#'
#' @param x object of class `distcompare`
#' @param models Can give list of `WpProj` outputs and have them turned into `distcompare` object for immediate plotting
#' @param ylim Limits on y-axis
#' @param ylabs Y-axis labels
#' @param xlab  X-axis labels
#' @param xlim  Limits of the x-axis
#' @param linesize How big to make the lines?
#' @param pointsize How big to make the points?
#' @param facet.group Should the plots be turned into a \link[ggplot2]{facet_grid}?
#' @param ... Additional options for the wasserstein distance if just inputing raw `WpProj` models
#'
#' @return A `ggplot2` object
#' @keywords internal
methods::setMethod("plot", c("x" = "distcompare"), plot.distcompare)

# methods::setMethod("print", c("x" = "plotcompare"), print.plotcompare)
