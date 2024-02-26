#' @title Plot a quality control chart
#'
#' @description Plot a 'cgrcusum', 'bkcusum', 'bercusum' or 'funnelplot' chart.
#'
#'
#' @param x chart to plot
#' @param h control limit to display for 'cgrcusum', 'bkcusum' or 'bercusum'
#' @param percentage Should output be shown in percentages?
#' @param ... further plotting parameters
#'
#'
#' @author Daniel Gomon
#'
#' @seealso \code{\link[cgrcusum]{cgrcusum}}, \code{\link[cgrcusum]{bkcusum}}, \code{\link[cgrcusum]{bercusum}}, \code{\link[cgrcusum]{funnelplot}}
#'
#' @returns A plot of the associated chart is displayed in the current graphics device.
#'
#' @describeIn plot Plot a CGR-CUSUM
#' @import ggplot2
#' @export
plot.cgrcusum <- function(x, h, ...){
  time <- value <- NULL
  requireNamespace('ggplot2')
  g <- ggplot(as.data.frame(x$CGR),
                       mapping = aes(x = time, y = value)) + geom_line()
  if(!missing(h)){
    g <- g + geom_hline(yintercept = h, color = "red")
  } else if("h" %in% names(x)){
    g <- g + geom_hline(yintercept = x$h, color = "red")
  }
  return(g)
}

#' @describeIn plot Plot a BK-CUSUM
#' @import ggplot2
#' @export
plot.bkcusum <- function(x, h, ...){
  time <- value <- NULL
  requireNamespace('ggplot2')
  g <- ggplot(as.data.frame(x$BK),
              mapping = aes(x = time, y = value)) + geom_line()
  if(!missing(h)){
    g <- g + geom_hline(yintercept = h, color = "red")
  } else if("h" %in% names(x)){
    g <- g + geom_hline(yintercept = x$h, color = "red")
  }
  return(g)
}



#' @describeIn plot Plot a funnelplot
#' @import ggplot2
#' @export
plot.funnelplot <- function(x, percentage = TRUE, ...){
  numtotal <- lower <- conflev <- upper <- p <- instance <- NULL
  #Supply plot.FUNNEL with output from FUNNELsim or a data frame with $instancedata and $p0 and $conflev
  if(percentage == TRUE){
    x$plotdata[, c("lower", "upper")] <- x$plotdata[, c("lower", "upper")] * 100
    x$data$p <- x$data$p * 100
    x$p0 <- x$p0*100
  }
  maxy <- max(x$data$p) + 0.1*max(x$data$p)
  miny <- min(x$data$p) - 0.1*max(x$data$p)
  t <- ggplot() + geom_line(data = x$plotdata, mapping= aes(x = numtotal, y = lower, group = as.factor(conflev)), colour = "blue", linetype = "dashed")  + geom_line(data = x$plotdata,aes(x = numtotal, y = upper, group = as.factor(conflev)),colour = "blue", linetype = "dashed") + geom_point(data = x$data, mapping= aes(x = numtotal, y = p, colour = as.factor(instance))) + theme(legend.position = "none") + geom_hline(yintercept = x$p0, colour = "grey") + ylim(miny, maxy)
  t <- t + labs(x = "Number of outcomes", y = paste0("(Risk-adjusted) Proportion of failure (%)"))
  return(t)
}


#' @describeIn plot Plot a Bernoulli CUSUM
#' @import ggplot2
#' @export
plot.bercusum <- function(x, h = x$h, ...){
  time <- value <- NULL
  g <- ggplot(as.data.frame(x$CUSUM), mapping = aes(x = time, y = value)) + geom_line() + geom_hline(yintercept = h, color = "red")
  return(g)
}

