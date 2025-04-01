eap_continuous_obli<-function(X,P,PHI){

  X<-as.matrix(X)
  P<-as.matrix(P)

  n<-size(X)[1]

  m<-size(P)[1]
  r<-size(P)[2]

  M <- apply(X,2,mean)
  Sx <- apply(X,2,sd)
  DIF <- X - (matrix(1,n,1) %*% M)
  Z <- DIF / (matrix(1,n,1) %*% Sx)

  R <- cor(X)

  "%^%" <- function(mat,power){
    base = mat
    out = diag(nrow(mat))
    while(power > 1){
      if(power %% 2 == 1){
        out = out %*% base
      }
      base = base %*% base
      power  = power %/% 2
    }
    out %*% base
  }

  th <- t(PHI%*%t(P)%*%mpower(R,-1)%*%t(Z))

  U <- diag(diag(R - P%*%PHI%*%t(P)))
  VAR_E <- mpower((mpower(PHI,-1) + t(P)%*%mpower(U,-1) %*% P),-1)
  if (r!=1){
    se <- sqrt(diag(diag(VAR_E)))
  }
  else {
    se <- sqrt(VAR_E)
  }

  z90 <- 1.64485362695147

  # 90% approximate confidence interval

  th_li <- matrix(0,n,r)
  th_ls <- matrix(0,n,r)
  for (i in 1:n){

    for (j in 1:r){
      th_li[i,j] <- th[i,j] - z90*se[j,j]
      th_ls[i,j] <- th[i,j] + z90*se[j,j]
    }
  }

  reli <- diag(1-VAR_E)

  OUT<-list("th"=th,"th_li"=th_li,"th_ls"=th_ls,"se"=se,"reli"=reli)
  return(OUT)

}
