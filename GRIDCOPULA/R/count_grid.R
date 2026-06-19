#' @title Counts the data according to a specified grid
#' @description Returns a matrix of size kxm indicating the quantity of data.
#' @param U a matrix of size nx2 with the observed values.
#' @param k a positive integer indicating the number of subintervals for the U2 variable.
#' @param m a positive integer indicating the number of subintervals for the U1 variable.
#' @examples



count.grid <- function(U, k, m) {
  B <- matrix(0, nrow=k, ncol=m)
  n <- nrow(U)
  for(i in 1:n) {
    u1 <- round(x=U[i,1], digits=10)
    u2 <- round(x=U[i,2], digits=10)
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
    }
    index.column <- u.index
    index.row <- 1 - (v.index-k)
    B[index.row, index.column] <- B[index.row, index.column] + 1
  }
  return(B)
}
