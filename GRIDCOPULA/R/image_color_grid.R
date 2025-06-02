#' @title Draws the density of a grid copula with colors
#' @return Returns a graph of the density.
#' @param gc a grid type copula object.
#' @param color.name indicates the palette of colors.
#' @param color.size indicates the number of colors.
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
#' image_color_grid(gc = copula.grid, color.name = "rainbow", color.size = 10)
#' @export


image_color_grid <- function(gc, color.name="heat.colors", color.size=7) {
	mg<- gc
  u1 <- seq(0, 1, length.out=(mg$m+1))
  u2 <- seq(0, 1, length.out=(mg$k+1))
  if(color.name=="heat.colors") {
    paleta.color <- heat.colors(n=color.size)
  } else if(color.name=="rainbow") {
    paleta.color <- rainbow(n=color.size)
  } else if(color.name=="terrain.colors") {
    paleta.color <- terrain.colors(n=color.size)
  } else if(color.name=="topo.colors") {
    paleta.color <- topo.colors(n=color.size)
  } else if(color.name=="cm.colors") {
    paleta.color <- cm.colors(n=color.size)
  } else if(color.name=="tim.colors") {
    paleta.color <- tim.colors(n=color.size)
  } else if(color.name=="gray") {
    paleta.color <- gray(seq(0,1,length.out=color.size))
  }
  image.plot(u1, u2, t(mg$Density[mg$k:1,]), col=paleta.color, 
             xlab="u", ylab="", xaxp=c(0, 1, 4), yaxp=c(0, 1, 4), 
             xlim=c(0,1), ylim=c(0,1))
  abline(v=c(0,1))
  abline(h=c(0,1))
  mtext(text="v", side=2, line=3, las=1)
  return(0)
}
