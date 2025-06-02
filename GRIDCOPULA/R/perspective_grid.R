#' @title Draws the density / distribution function of a grid copula with perspective
#' @return Returns a graph of the density / distribution.
#' @param gc a grid type copula object.
#' @param FUN the name of the function to be applied (d.grid, p.grid), default is 'd.grid'.
#' @param u1 indicates the place for lines on axis u1.
#' @param u2 indicates the place for lines on axis u2.
#' @param ang.theta angle for the azimuthal direction.
#' @param ang.phi angle for the colatitude.
#' @param distancia the distance of the eyepoint from the centre of the box.
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
#' perspective.grid(gc = copula.grid, ang.theta = 90 , ang.phi = 80, distancia = 3)
#' perspective.grid(gc = copula.grid, FUN = "p.grid")
#' @export


perspective.grid <- function(gc, FUN='d.grid', u1=seq(0, 1, length.out=21), 
                             u2=seq(0, 1, length.out=21), ang.theta=-30, 
                             ang.phi=25, distancia=10) {
	mg<- gc
  f.u <- outer(u1, u2, FUN, mg)
  
  p <- persp(u1, u2, f.u, theta=ang.theta, phi=ang.phi, r=distancia, 
             zlim=c(0, 1.02*max(f.u)), nticks=3, ticktype="detailed", xlab="u",
             ylab="v", zlab="") 
  return(0)
}

