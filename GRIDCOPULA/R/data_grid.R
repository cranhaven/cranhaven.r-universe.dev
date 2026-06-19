#' @title Draws the scatter plot of bivariate data in the unit square
#' @return Returns a scatter plot of bivariate data in the unit square.
#' @param U matrix of size kx2 with the values of both variables.
#' @param draw.lines draws lines inside the unit square or not.
#' @param k positive integer indicating the number of subintervals for the U2 variable.
#' @param m positive integer indicating the number of subintervals for the U1 variable.
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
#' data.grid(U=df, draw.lines = FALSE, k = k, m = m)
#' data.grid(U=df, draw.lines = TRUE, k = k, m = m)
#' @export


data.grid <- function(U, draw.lines=TRUE, k=4, m=4) {
  plot(U, xlab="u", ylab="", xaxp=c(0, 1, 4), yaxp=c(0, 1, 4), xlim=c(0,1), 
       ylim=c(0,1))
  mtext(text="v", side=2, line=3, las=1)
  if(draw.lines) {
    u.breaks <- seq(0, 1, length.out=m+1)
    v.breaks <- seq(0, 1, length.out=k+1)
    abline(v=u.breaks, lty=2, col="gray")
    abline(h=v.breaks, lty=2, col="gray")
  }
  return(0)
}
