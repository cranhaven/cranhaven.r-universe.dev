#' @title Evaluates the density of a grid copula for a single data point
#' @description Returns the corresponding distribution function values.
#' @param u vector of size 1x2 with the values of the variables.
#' @param mg a grid type copula object.
#' @examples


pdf.grid <- function(u, mg) {

  k <- mg$k
  m <- mg$m
  value <- 0
  u1 <- round(x=u[1], digits=10)
  u2 <- round(x=u[2], digits=10)
  if(0<=u1 & u1<=1 & 0<=u2 & u2<=1) {
    u.index <- 1
    if(0<u1) {
      u.left <- round(x=((u.index-1) / m), digits=10)
      u.right <- round(x=(u.index / m), digits=10)
      while(u1<=u.left | u.right<u1) {
        u.index <- u.index + 1
        u.left <- u.right
        u.right <- round(x=(u.index / m), digits=10)
      }
    }
    v.index <- 1
    if(0<u2) {
      v.left <- round(x=((v.index-1) / k), digits=10)
      v.right <- round(x=(v.index / k), digits=10)
      while(u2<=v.left | v.right<u2) {
        v.index <- v.index + 1
        v.left <- v.right
        v.right <- round(x=(v.index / k), digits=10)
      }
    }
    index.column <- u.index
    index.row <- 1 - (v.index-k)
    value <- mg$Density[index.row, index.column]
  }
  return(value)
}
