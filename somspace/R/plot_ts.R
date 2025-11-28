#' Plot time series
#' 
#' @description Plots the time series of SOM nodes or regions mean 
#' 
#' @param x is either a `somsp` or a `regs` object  
#' @param n is either the set of nodes for `somsp` or the number of regions for `regs`
#' @return plot object
#' 
#' @details In case of `regs`, all the regions are ploted. 
#' 
#' @seealso \code{\link{somspa}}
#' 
#' @import ggplot2
#' @rawNamespace import(data.table, except = melt)
#' @importFrom maps map
#' @export 

plot_ts <- function(x, n) UseMethod("plot_ts")

#' @export 

plot_ts.somsp <- function(x, n){
  to_plot <- x$input_dt[, .(node, time, variable)]
  to_plot <- unique(to_plot[, .(variable = mean(variable)), .(time, node)])
  ggplot(to_plot[node %in% n], aes(x = time, y = variable)) +
    geom_line(alpha = 0.3) +
    geom_smooth(method = 'loess', span = 0.1, col =  "black", fill = 'dark red') +
    facet_wrap(~node) +
    labs(x = "Time", y = "Variable") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "grey20")) +
    theme(strip.text.x = element_text(colour = "grey90", size = 10))
}

#' @export 

plot_ts.regs <- function(x, n){
  to_plot <- get_ts(x, n)
  ggplot(to_plot, aes(x = time, y = variable)) +
    geom_line(alpha = 0.3) +
    geom_smooth(method = 'loess', span = 0.1, col =  "black", fill = 'dark red') +
    facet_wrap(~region) +
    labs(x = "Time", y = "Variable") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "grey20")) +
    theme(strip.text.x = element_text(colour = "grey90", size = 10))
}
