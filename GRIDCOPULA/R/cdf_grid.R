#' @import fields
#' @import ggplot2
#' @import reshape2
#' @import Rsolnp
#' @import pracma
#' @title Calculates the distribution function of a grid copula for a single data point
#' @description Returns the corresponding distribution function value.
#' @param u vector of size 1x2 with the values of the variables.
#' @param mg a grid type copula object.
#' @param Vm Volume matrix.
#' @examples



cdf.grid <- function(u, mg, Vm) {
  k <- mg$k
  m <- mg$m
  u1 <- round(x=u[1], digits=10)
  u2 <- round(x=u[2], digits=10)

  if(u1 <= 0 | u2 <= 0) {
    value <- 0
  } else {
    if(1 < u1) {
      u1 <- 1
    }
    if(1 < u2) {
      u2 <- 1
    }
    u.index <- 1
    u.left <- round(x=((u.index-1) / m), digits=10)
    u.right <- round(x=(u.index / m), digits=10)
    while(u1<=u.left | u.right<u1) {
      u.index <- u.index + 1
      u.left <- u.right
      u.right <- round(x=(u.index / m), digits=10)
    }
    v.index <- 1
    v.left <- round(x=((v.index-1) / k), digits=10)
    v.right <- round(x=(v.index / k), digits=10)
    while(u2<=v.left | v.right<u2) {
      v.index <- v.index + 1
      v.left <- v.right
      v.right <- round(x=(v.index / k), digits=10)
    }
    index.column <- u.index
    index.row <- 1 - (v.index-k)
    if(1 < u.index & 1 < v.index) {
      value <- Vm[(index.row+1),(index.column-1)]
      u.excess <- (u1 - (u.index-1)/m)
      v.excess <- (u2 - (v.index-1)/k)
      value <- value + ( u.excess * sum(mg$Density[((index.row+1):k),index.column]) / k )
      value <- value + ( v.excess * sum(mg$Density[index.row,(1:(index.column-1))]) / m )
      value <- value + ( u.excess * v.excess * mg$Density[index.row,index.column] )
    }
    if(u.index==1 & 1 < v.index) {
      u.excess <- u1
      v.excess <- (u2 - (v.index-1)/k)
      value <- u.excess * sum(mg$Density[((index.row+1):k),index.column]) / k
      value <- value + ( u.excess * v.excess * mg$Density[index.row,index.column] )
    }
    if(1 < u.index & v.index==1) {
      u.excess <- (u1 - (u.index-1)/m)
      v.excess <- u2
      value <- v.excess * sum(mg$Density[index.row,(1:(index.column-1))]) / m
      value <- value + ( u.excess * v.excess * mg$Density[index.row,index.column] )
    }
    if(u.index==1 & v.index==1) {
      u.excess <- u1
      v.excess <- u2
      value <- u.excess * v.excess * mg$Density[index.row,index.column]
    }
  }
  return(value)
}
