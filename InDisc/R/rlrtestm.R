rlrtestm <- function(X,mu,lam,cvar,vare,th0,th1,vari,nfac){

  n <- size(X)[1]
  LR <- matrix(0,n,2)
  sum <- 0

  for (i in 1:n){
    OUT <- lrtestm(X[i,], mu, lam, cvar, vare, th0[i,], th1[i,], vari[i],nfac)
    LR[i,1] <- as.numeric(OUT$LR)
    LR[i,2] <- as.numeric(OUT$chi)
    sum <- sum + LR[i,2]
  }

  return(LR)

}
