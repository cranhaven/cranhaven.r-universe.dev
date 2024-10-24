runs_test <- function(xx=xx,p=p,var1=var1,var2=var2,q=q){
  # Version antigua
  ff <- sort(unique(xx))
  # runs_test
  nx <- length(xx)-1
  ni <- numeric()
  for (i in 1:q){
    ni[i] <- sum(xx==ff[i])
  }
  # x <- abs(diff(xx))
  R <- 1 + sum(abs(diff(xx))>0)
  meanR <- 1 + nx*p
  stdR <- sqrt(nx*p*(1-p)+2*(nx-1)*(var2-p^2)+(nx-1)*(nx-2)*(var1-p^2))
  Z <- (R-meanR)/stdR
  return <- list(R=R,meanR=meanR,stdR=stdR, Z=Z)

}
