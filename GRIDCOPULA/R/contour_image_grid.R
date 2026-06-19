#' @title Draws the density / distribution function of a grid copula with contours and colors
#' @return Returns a graph of the density / distribution.
#' @param gc a grid type copula object.
#' @param FUN the name of the function to be applied (d.grid, p.grid), default is 'p.grid'.
#' @param u1 indicates the place for lines on axis u1.
#' @param u2 indicates the place for lines on axis u2.
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
#' copula.grid <- estimate.gridCopula(U = df, k = k, m = m , method = "ls")
#' contour_image_grid(gc = copula.grid, FUN = 'd.grid', color.name= "rainbow", color.size = 10)
#' contour_image_grid(gc = copula.grid, FUN = 'p.grid', color.name = "rainbow", color.size = 10)
#' @export



contour_image_grid <- function(gc, FUN='p.grid', u1=seq(0, 1, length.out=100), 
                               u2=seq(0, 1, length.out=100), color.name="heat.colors", 
                               color.size=40) {
	mg<-gc
  f.u <- outer(u1, u2, FUN, mg)
  f.levels <- seq(0, 1.02*max(f.u), length.out=(color.size+1))
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
  } else if(color.name=="gray") {
    paleta.color <- gray(seq(0,1,length.out=(color.size+1)))
  }
  image(u1, u2, f.u, xlab="u", ylab="", col=paleta.color)
  contour(u1, u2, f.u, xlab="u", ylab="", xaxp=c(0, 1, 4), yaxp=c(0, 1, 4), 
          xlim=c(0,1), ylim=c(0,1), nlevels=color.size, add=TRUE)
  mtext(text="v", side=2, line=3, las=1)
  return(0)
}
