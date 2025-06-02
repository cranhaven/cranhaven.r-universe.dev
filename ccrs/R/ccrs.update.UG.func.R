

update.G <- function(U=U,Y.hat=Y.hat){
  solve(t(U)%*%U)%*%t(U)%*%Y.hat
}

update.U <- function(G=G,Y.hat=Y.hat){

  n <- nrow(Y.hat)
  K <- nrow(G)
  Unew<-matrix(0,n,K)
  clsupdate.var<-rep(0,n)

  Y.hat.d <- dplyr::as_data_frame(Y.hat)
  G.d <- dplyr::as_data_frame(G)
  XG <- as.matrix(dplyr::bind_rows(Y.hat.d,G.d))
  #XG<-rbind(Y.hat,G)
  distXG<-as.matrix(stats::dist(XG))

  #take out upper part
  disXG.up<-distXG[c(1:n),c((n+1):(ncol(distXG)))]

  if(K>1) {
    clsupdate.var<-apply(disXG.up,1,which.min)
  }else{
    clsupdate.var<-rep(1,n)
  }

  Unew <- 1.0 * outer(clsupdate.var,c(1:K),"==")
  empty.cls <- ifelse((length(table(clsupdate.var))!=K),T,F)

  list(U=Unew,empty.cls=empty.cls)

}
