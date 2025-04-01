priorgral <- function(alpha, nquad,df,ls){

  n<-size(alpha)[1]
  alphamax <- max(alpha)
  amax2 <- alphamax * alphamax
  mvari <- (1-amax2) / amax2
  resi <- matrix(0,n,1)

  for (i in 1:n){
    resi[i] <- ((1-(alpha[i] * alpha[i])) / (alpha[i] * alpha[i])) - mvari
  }

  if (missing(df)==TRUE || missing(ls)==TRUE){
    df <- 5
    ls <- round(mvari*3)
    if (ls == 0){
      ls <- 1
    }
  }

  nodth <- creanodos(-4,4,nquad)
  nodichi<- creanchi(0.02,4,nquad,df,ls)
  if (df == 5){
    var_nodos <- 0.4159 # the variance of the nodes
  }
  if (df == 6){
    var_nodos <- 0.3832
  }
  if (df == 10){
    var_nodos <- 0.2631
  }


  OUT<-list("mvari" = mvari, "resi" = resi, "nodth" = nodth, "nodichi" = nodichi, "var_nodos" = var_nodos)
  return(OUT)
}
