snlSubspaceIntersection <- function(nvec,Ub1,Ub2,condtolerscaling,d){
  
  flagred <- 0
  
  kb1 <- nvec[1]
  kb2 <- nvec[2]
  
  U1p <- Ub1[seq(1,kb1,1),]
  U1pp <- Ub1[seq(kb1+1,nrow(Ub1),1),]
  
  U2pp <- Ub2[seq(1,kb2,1),]
  U2p <- Ub2[seq(kb2+1,nrow(Ub2),1),]
  
  SVD1 <- svd(U1pp)
  SVD2 <- svd(U2pp)
  
  cond1 <- max(SVD1$d)/min(SVD1$d)
  cond2 <- max(SVD2$d)/min(SVD2$d)
  
  if(min(cond1,cond2) > condtolerscaling*kb2){
    flagred <- 1
  }else{
    if(cond1 < cond2){
      Ubar <- rbind(U1p %*%(qr.solve(U1pp, U2pp)), Ub2)
      if(kb2 >= d+1){
        U <- Ubar
      }else if(kb2 == d){
        temp <- svd(U1pp,nu=nrow(U1pp),nv=ncol(U1pp))
        temp2 <- temp$v
        u1 <- temp2[,-(seq(1,length(temp$d),1))]
        U <- cbind(Ubar, rbind(U1p %*% u1,matrix(rep(0,nrow(Ub2),ncol=1))))
      }else{
        flagred <- 1
      }
    }else{
      Ubar <- rbind(Ub1,U2p %*% (qr.solve(U2pp, U1pp)))
      if(kb2 >= d+1){
        U <- Ubar
      }else if(kb2 == d){
        temp <- svd(U2pp,nu=nrow(U2pp),nv=ncol(U2pp))
        temp2 <- temp$v
        u2 <- temp2[,-(seq(1,length(temp$d),1))]
        
        U <- cbind(Ubar, rbind(matrix(rep(0,nrow(Ub1),ncol=1)),U2p %*% u2))
      }else{
        flagred <- 1
      }
    }
  }
  
  if(flagred){
    U <- NULL
  }
  
  return(list(U=U, flagred=flagred))
}