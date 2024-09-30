####################################################################################################################################
### Filename:    utility.R
### Description: Function 'sim_power' for performing a power simulation and 'insert_row' to add a row in a data.frame
###              'asymptotic_wilcox_test' calculates two.sided p-values for Wilcox Test
###
###
###
####################################################################################################################################

#' @keywords internal
asymptotic_wilcox_test <- function(x, y) {

  n1 <- length(x)
  n2 <- length(y)
  N <- n1+n2
  data <- c(x,y)
  R <- rank(data, ties.method = "average")
  R1 <- R[1:n1]
  R2 <- R[(n1+1):(n1+n2)]

  sigma_R_squared = sum( (R-(N+1)/2 )^2 ) * 1/(N-1)
  W_N = sqrt(n1*n2*1/N)*(mean(R2) - mean(R1))*1/sqrt(sigma_R_squared)

  p_value <- 2*min( pnorm(W_N), 1-pnorm(W_N) )
  return(p_value)
}

#' @keywords internal
sim_power <- function(x1,x2,nsim,n1,n2) {

  simpower <- 0
  cat("Simulation:\n")

  for(i in 1:nsim){
    if(i%%1000==0){
      cat(paste(i, "/", nsim, "\n"))
    }
    z1 <- sample(x1, size = ceiling(n1), prob = NULL, replace = TRUE)
    z2 <- sample(x2, size = ceiling(n2), prob = NULL, replace = TRUE)
    if(asymptotic_wilcox_test(z1,z2) <= 0.05) {
      simpower <- simpower + 1
    }
  }
  cat("\n")
  return (simpower)
}


#' @keywords internal
insert_row <- function(df, newrow, r) {
  df[seq(r+1,nrow(df)+1),] <- df[seq(r,nrow(df)),]
  df[r,] <- newrow
  return(df)
}
