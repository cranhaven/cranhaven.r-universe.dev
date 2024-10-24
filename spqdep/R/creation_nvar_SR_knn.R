creation_nvar_SR_knn = function(W=W){
  # W puede ser una matrix de distancias para establecer el orden
  # W también puede ser un objeto de la clase nb
  
  # profvis({
  repmat = function(X,m,n){
    ## R equivalent of repmat (matlab)
    X<-as.matrix(X)
    mx = dim(X)[1]
    nx = dim(X)[2]
    matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)}
  R <- dim(W$nn)[1]
  
  NNB <- cbind(1:R,W$nn,repmat(-99,dim(W$nn)[1],1))
  B <- NNB
  end <- dim(B)[1]*dim(B)[2]
  B1 <- matrix(t(B),ncol = 1)
  B2 <- cbind(B1[1:(end-1)],B1[2:end])
  B2 <- rbind(B2,c(-99,-99))
  nn <- 0
  
  for (i in 1:dim(B2)[1]){
    # Identifico los que estan en la misma linea
    B3 <- B2
    dB2 <- dim(B)[2]
    k <- floor((i-1)/dB2)+1
    quito1 <- seq(1+(k-1)*dB2,(dB2+(k-1)*dB2))
    # Identifico los que contienen un -99
    quito2 <- ((B3[,2]==-99)*(1:dim(B3)[1]))
    quito2 <- quito2[quito2>0] #quito2(2:end);
    quito3 <- ((B3[,1]==-99)*(1:dim(B3)[1]))
    quito3 <- quito3[quito3>0] #quito2(2:end);
    quito23 <- (c(quito2,quito3))
    quito <- (c(quito1,quito23))
    B3 <- B3[-quito,]
    
    if (sum(quito23==i)==0){
      hh <- rowSums(B3==B2[i,1])+rowSums(B3==B2[i,2])
      nk <- c(sum(hh==0),sum(hh==1),sum(hh==2))
      nn=nn+nk
    }
  }
  return(nn)
  # })
}