creation_nvar_SR = function(listw=listw){
  # W puede ser una matrix de distancias para establecer el orden
  # W también puede ser un objeto de la clase nb

  # profvis({
  repmat = function(X,m,n){
    ## R equivalent of repmat (matlab)
    X<-as.matrix(X)
    mx = dim(X)[1]
    nx = dim(X)[2]
    matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)}

  # W <- listw
  if (inherits(listw, "knn")){
    n <- dim(listw$nn)[1]
    NNB <- cbind(1:n,listw$nn,repmat(-99,dim(listw$nn)[1],1))
  }

# Debe ser una matrix nb con elementos ordenados por DISTANCIA
  if (inherits(listw, "nb")){
    m <- rowSums(nb2mat(listw, style = 'B',
                        zero.policy = TRUE))
    n <- length(listw)
    NNB <- matrix(-99,ncol=max(m),nrow=n)
    for (i in 1:n){
      if (m[i] != 0){ # Si no tiene vecinos no hay que incluir elementos
    NNB[i,1:m[i]] <- listw[[i]]
    }
    }
    NNB <- cbind(1:n,NNB,repmat(-99,n,1))
    }

  ######
  # if (inherits(W, "sf")){
  #    W <- poly2nb(as(hexs.sf, "Spatial"), queen = FALSE)
  #    R <- length(W)
  #    co <- sf::st_coordinates(sf::st_centroid(hexs.sf))
  #    ang <- useful::cart2pol(co[,1],co[,2],degrees = T)[,2]$theta
  #    for (kk in 1:R){
  #    W[[kk]]<-hexs.nb[[kk]][sort(ang[hexs.nb[[kk]]],index.return=TRUE)$ix]
  #    }
  #   lnnb <- numeric()
  #   for (i in 1:R){
  #     lnnb[i] <- length(W[[i]])
  #   }
  #   mlnnb <- max(lnnb)
  #   NNB <- matrix(0,ncol = (mlnnb+1),nrow = R)
  #   for (i in 1:R){
  #     NNB[i,] <- c(i,W[[i]],repmat(-99,1,mlnnb-lnnb[i]))
  #   }
  #   NNB <- cbind(NNB,repmat(-99,dim(NNB)[1],1))
  # }
  ######
  if (inherits(listw, "matrix")){
      n <- dim(listw)[1]
      mlnnb <- max(rowSums(listw>0))+1
      NNB <- matrix(0,ncol = (mlnnb+1),nrow = n)
      lnnb <- matrix(0,ncol = 1,nrow = n)
      for (i in 1:n){
        tmp=sort(listw[i,],index.return=TRUE)
        a <- (tmp$x>0)*(tmp$ix)
        a <- rev(a[a>0])
        lnnb[i] <- length(a)
        NNB[i,] <- c(i,a,repmat(-99,1,mlnnb-lnnb[i]))
      }
    }

  B <- NNB
  end <- dim(B)[1]*dim(B)[2]
  B1 <- matrix(t(B),ncol = 1)
  B2 <- cbind(B1[1:(end-1)],B1[2:end])
  B2 <- rbind(B2,c(-99,-99))
  nn <- 0

  # Esto que estaba dentro del bucle puede sacarse fuera
  # Identifico los que contienen un -99
  quito2 <- ((B2[,2]==-99)*(1:dim(B2)[1]))
  quito2 <- quito2[quito2>0] #quito2(2:end);
  quito3 <- ((B2[,1]==-99)*(1:dim(B2)[1]))
  quito3 <- quito3[quito3>0] #quito2(2:end);
  quito23 <- unique(c(quito2,quito3))
  noquito = c(1:dim(B2)[1])
  noquito <- noquito[-quito23]
  for (i in noquito){ # 1:dim(B2)[1]){
    # Identifico los que estan en la misma linea
    B3 <- B2
    dB2 <- dim(B)[2]
    k <- floor((i-1)/dB2)+1
    quito1 <- seq(1+(k-1)*dB2,(dB2+(k-1)*dB2))
    # # Identifico los que contienen un -99
    # quito2 <- ((B3[,2]==-99)*(1:dim(B3)[1]))
    # quito2 <- quito2[quito2>0] #quito2(2:end);
    # quito3 <- ((B3[,1]==-99)*(1:dim(B3)[1]))
    # quito3 <- quito3[quito3>0] #quito2(2:end);
    # quito23 <- (c(quito2,quito3))
    quito <- unique(c(quito1,quito23))
    # B3 <- B3[(B3[,1]!=-99),]
    # B3 <- B3[(B3[,2]!=-99),]
    B3 <- B3[-quito,]

    #if (sum(quito23==i)==0){
      hh <- rowSums(B3==B2[i,1])+rowSums(B3==B2[i,2])
      nk <- c(sum(hh==0),sum(hh==1),sum(hh==2))
      nn=nn+nk
    #}
  }
  return(nn)
  # })
}
