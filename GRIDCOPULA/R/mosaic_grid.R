#' @import ggplot2
#' @title Draws the density of a grid copula with mosaics
#' @return Returns a graph.
#' @param gc a grid type copula object.
#' @param number.size indicates the size of numbers.
#' @examples
#' n <- 500
#' x <- rgamma(n,4,1/2)
#' e <- rnorm(n,0,.3)
#' y <- sin(x+e)
#' Fx <- ecdf(x)
#' Fy <- ecdf(y)
#' u <- Fx(x)
#' v <- Fy(y)
#' df <- cbind(u,v)
#' k <- 10
#' m <- 10
#' copula.grid <- estimate.gridCopula(U = df, k = k, m = m , method = "ml")
#' mosaic.grid(gc = copula.grid, number.size = 5)
#' @export


mosaic.grid <- function(gc, number.size=5) {
	mg<- gc
  density.values <- data.frame(melt(round(mg$Density, 2)))
  
  d1 <- mg$m
  d2 <- mg$k
  u1 <- seq(1/(2*d1), 1-1/(2*d1), length.out=d1)
  u2 <- seq(1/(2*d2), 1-1/(2*d2), length.out=d2)
  
  density.values$u <- rep(u1, each=d2)
  density.values$v <- rep(rev(u2), times=d1)
  u <- density.values$u 
  v <- density.values$v
  value <- density.values$value
	
  p <- ggplot2::ggplot(data=density.values, ggplot2::aes(x= u, y= v, fill=value)) + 
    ggplot2::geom_tile() + ggplot2::geom_text(ggplot2::aes(label=value), color='white', 
                            size=number.size) + ggplot2::theme_bw() + 
    ggplot2::theme(axis.title.y = element_text(angle = 0)) + 
    ggplot2::theme(legend.title = element_blank())
  plot(p)
  return(0)
}
