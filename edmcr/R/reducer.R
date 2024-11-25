reducer <- function(X, Comp, type=0){
  
  Cq <- Comp$Cq
  num_atoms <- nrow(Cq)
  num_cliq <- ncol(Cq)
  
  if(type > 0){
    U <- c()
    cliq_dims <- rep(0, num_cliq)
    
    for(i in 1:num_cliq){
      cliq_index <- rep(FALSE, num_atoms)
      cliq_index[which(Cq[,i] == 1)] <- TRUE
      qX <- X[,cliq_index]
      dim <- pcarank(qX)
      cliq_dims[i] <- dim
    }
  }else{
    #compute U for each clique
    cliq_dims <- rep(0, num_cliq)
    qU <- matrix(list(), ncol=1,nrow=num_cliq)
    for(i in 1:num_cliq){
      cliq_index <- rep(FALSE,num_atoms)
      cliq_index[which(Cq[,i] == 1)] <- TRUE
      qX <- X[,cliq_index]
      qD <- distmex(qX)
      qK <- kdagger(qD)
      len_cliq <- sum(cliq_index)
      tU <- eigb(qK)$V
      dim <- pcarank(qX)
      cliq_dims[i] <- dim
      qU[[i,1]] <- tU[,1:dim]
      qU[[i,1]] <- cbind(qU[[i,1]], rep(1,len_cliq)/sqrt(len_cliq))
    }
    
    ## Intersection of Cliques ##
    #1-Intersection of cliques
    
    iSq <- t(Cq) %*% Cq
    rowU <- matrix(list(), nrow=num_cliq, ncol=1)
    for(i in 1:num_cliq){
      num_intersect <- which(iSq[i,] != 0)
      index <- which(num_intersect < i)
      if(length(index) > 0){
        num_intersect <- num_intersect[-index]
      }
      len_intersect <- length(num_intersect)
      if(len_intersect < 1){
        stop("At least one clique in the intersection")
      }
      rowU[[i,1]] <- qU[[i,1]]
      if(len_intersect >= 2){
        for(j in 2:len_intersect){
          tj <- num_intersect[j]
          out <- intersecter(rowU[[i,1]], qU[[tj,1]], Cq[,i], Cq[,tj])
          rowU[[i,1]] <- out$U
          Cq[,i] <- out$w
        }
      }
    }
    
    #2-intersection of row-wise intersection
    
    num_cliq_level <- num_cliq
    old_num_cliq_level <- num_cliq_level
    num_levels <- floor(log2(num_cliq)) + 1
    
    oldU <- rowU
    oldCq <- Cq
    for(i in 1:num_levels){
      num_cliq_level <- ceiling(num_cliq_level/2)
      tempU <- matrix(list(), nrow=num_cliq_level, ncol=1)
      tCq <- matrix(rep(0,num_atoms*num_cliq_level), nrow=num_atoms)
      for(j in 1:num_cliq_level){
        qU1 <- oldU[[(j-1)*2+1,1]]
        q1 <- oldCq[,(j-1)*2+1]
        tempU[[j,1]] <- qU1
        tCq[,j] <- q1
        if((j-1)*2 + 2 <= old_num_cliq_level){
          qU2 <- oldU[[(j-1)*2+2,1]]
          q2 <- oldCq[,(j-1)*2+2]
          out <- intersecter(qU1,qU2,q1,q2)
          tempU[[j,1]] <- out$U
          tCq[,j] <- out$w
        }
      }
      
      oldCq <- tCq
      oldU <- tempU
      #message(paste("dim(oldU[[1,1]]) = ", dim(oldU[[1,1]])))
      old_num_cliq_level <- num_cliq_level
    }
    U <- oldU[[1,1]]
  }
  
  if(nrow(U) != ncol(X) && type == 0){
    stop("U is incomplete")
  }
  
  return(list(U=U, cliq_dims = cliq_dims))
}

