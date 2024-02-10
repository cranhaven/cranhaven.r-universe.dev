#' @noRd
cij <- function(i,j,U){
  out <- 1;
  if(i == j){
    return(out)
  }else{
    for(l in i:(j-1)){
      out = out*U[[l]]$c
    }
    return(out)
  }
}

#' @noRd
microstrata <- function(pik){
  
  eps <- 1e-7
  
  ## INITIALIZING CONSTANT
  N <- length(pik)
  n <- round(sum(pik)) # ONLY to integer 
  n <- as.integer(n) # ensure that correctly integer
  
  
  # add eps to ensure that cumsum is rightly calculated
  Vk <- cumsum(pik + eps) 
  Vk_1 <- c(0,cumsum(pik + eps)) 
  Vk_1 <- Vk_1[-length(Vk_1)]
  
  ki <- which(diff(c(0,floor(cumsum(pik + eps)))) == 1)
  
  Vki <- Vk[ki]
  Vki_1 <- Vk[ki - 1]
  ai <- seq(from = 1, to = n,by = 1) - Vki_1
  bi <- Vki - seq(from = 1,to = n,by = 1)

  # CREATION OF MICROSTRATA
  U <- rep(list(rep(list(0),3)),n)
  
  for(i in 1:n){
    
    if(i == 1){
      U[[i]][[1]] <- seq(from = 1,to = ki[i],by = 1)
    }else{
      U[[i]][[1]] <- seq(from = ki[i-1],to = ki[i],by = 1)
    }
    
    U[[i]][[2]] = ai[i];
    U[[i]][[3]] = bi[i];
    names(U[[i]])[1] <- c("m")
    names(U[[i]])[2] <- c("a")
    names(U[[i]])[3] <- c("b")
    
    U[[i]][[4]] = Vk_1[U[[i]]$m]
    U[[i]][[5]] = Vk[U[[i]]$m]
    names(U[[i]])[4] <- c("Vk_1")
    names(U[[i]])[5] <- c("Vk")
    
    
    U[[i]][[6]] = ai[i]*bi[i]*( (1- ai[i])*(1-bi[i]))^(-1)
    names(U[[i]])[6] <- c("c")
  }
  
  return(U)
  
}



#' @title Deville's systematic
#' @name sys_deville
#' 
#' @description 
#' This function implements a method to select a sample using the Deville's systmatic algorithm.
#' 
#' @param pik A vector of inclusion probabilities.
#'
#' @return Return the selected indices in 1,2,...,N
#' 
#' @references Deville, J.-C. (1998), Une nouvelle méthode de tirage à probabilité inégales. Technical Report 9804, Ensai, France.
#' 
#'  Chauvet, G. (2012), On a characterization of ordered pivotal sampling, Bernoulli, 18(4):1320-1340
#' 
#' @examples 
#' set.seed(1)
#' pik <- c(0.2,0.5,0.3,0.4,0.9,0.8,0.5,0.4)
#' sys_deville(pik)
#' @export sys_deville
sys_deville <- function(pik){
  
  ## INITIALIZING CONSTANT
  N <- length(pik)
  n <- sum(pik)
  
  ## MICROSTRATA CREATION
  U <- microstrata(pik)
  
  ## SELECTION LOOP
  
  ## STEP 1
  w <- runif(1,0,1)
  k <- 1
  
  while(!(U[[1]]$Vk_1[k] <= w & w < U[[1]]$Vk[k])){
    k = k+1
  }
  U[[1]]$s <- U[[1]]$m[k]
  U[[1]]$w <- w
  
  ## STEP 2 TO n
  for(i in 2:n){
    if(any(U[[i-1]]$s == U[[i]]$m)){
      w = runif(1,U[[i-1]]$b,1)
    }else{
      p <- U[[i-1]]$a*U[[i-1]]$b*( (1 - U[[i-1]]$a)*( 1 - U[[i-1]]$b) )^(-1)
      if( runif(1) < p){
        w = runif(1,0,U[[i-1]]$b)
      }else{
        w = runif(1,0,1)
      }
    }
    k <- 1
    while(!(U[[i]]$Vk_1[k] <= (w + (i-1)) & (w + (i-1)) < U[[i]]$Vk[k])){
      k = k+1
    }
    U[[i]]$s <- U[[i]]$m[k]
    U[[i]]$w <- w
  }
  
  s <- do.call(c,lapply(U,function(x){x$s}))
  return(s)
}




#' @title Second order inclusion probabilities of Deville's systematic
#'
#' @param pik A vector of inclusion probabilities
#'
#' @description 
#' This function returns the second order inclusion probabilities of Deville's systematic. 
#' 
#' @return A matrix of second order inclusion probabilities.
#' 
#' @references Deville, J.-C. (1998), Une nouvelle méthode de tirage à probabilité inégales. Technical Report 9804, Ensai, France.
#' 
#'  Chauvet, G. (2012), On a characterization of ordered pivotal sampling, Bernoulli, 18(4):1320-1340
#'
#' @examples
#' set.seed(1)
#' N <- 30
#' n <- 4
#' pik <- as.vector(inclprob(runif(N),n))
#' PI <- sys_devillepi2(pik)
#' #image(as(as.matrix(PI),"sparseMatrix"))
#' 
#' pik <- c(0.2,0.5,0.3,0.4,0.9,0.8,0.5,0.4)
#' PI <- sys_devillepi2(pik)
#' #image(as(as.matrix(PI),"sparseMatrix"))
#' @export
sys_devillepi2 <- function(pik){
  
  ## INITIALIZING CONSTANT
  N <- length(pik)
  n <- sum(pik)
  out <-  matrix(rep(0,N*N),ncol = N,nrow = N)
  
  
  ## MICROSTRATA CREATION 
  U <- microstrata(pik)
  
  ## CROSS AND NONCROSS UNITS
  noncross <- lapply(U,function(x){
    if(length(x$m) < (2 + 1e-7) && x$b < 1e-7){
      return(x$m)
    }else if(length(x$m) > 2){
      if(x$b > 1e-7){
        return(x$m[seq(from = 2,to = (length(x$m)-1),by = 1)])  
      }else{
        return(x$m[seq(from = 2,to = length(x$m),by = 1)])  
      }
    }else{
      return(NULL)
    }
  })
  noncross[[1]] <- c(1,noncross[[1]])
  

  
  # noncross[[length(noncross)]] <- c(noncross[[length(noncross)]],N)
  
  
  cross <- lapply(U,function(x){
    if(x$b < 1e-7){
      return(NULL)
    }else{
      x$m[length(x$m)]
    }
  })
  cross <- cross[-length(cross)]
  
  if(any(cross[[length(cross)]] == noncross[[length(noncross)]])){
    noncross[[length(noncross)]] <- noncross[[length(noncross)]][-which(cross[[length(cross)]] == noncross[[length(noncross)]])]
    
  }
  
  # nonull <- do.call(c,lapply(cross,function(x){!is.null(x)}))
  # cross <- cross[nonull]
  
  
  # cross <- lapply(U,function(x){x$m[length(x$m)]})
  # cross <- cross[-length(cross)]
  
  # cross <- lapply(U,function(x){x$m[1]})
  # cross <- cross[-1]
  
  b <- lapply(U,function(x){x$b})
  a <- lapply(U,function(x){x$a})
  
  
  ##------------- k NONCROSSBORDER VS l NONCROSSBORDER
  nn <- data.frame(k = NULL,l = NULL, c = NULL)
  for(i in 1:(length(noncross)-1)){
    if(!is.null(noncross[[i]])){
      for(k in 1:length(noncross[[i]])){
        tmp <- data.frame(k = NULL,l = NULL, c = NULL)
        for(u in (i+1):n){
          # cat(i,u,"\n")
          if(!is.null(noncross[[u]])){
            tmp <- rbind(tmp,
                         data.frame( k = rep(noncross[[i]][k],length(noncross[[u]])),
                                     l = noncross[[u]],
                                     c = rep(cij(i,u,U),length(noncross[[u]]))))
          }
        }
        nn <- rbind(nn,tmp)
      }
    }
    
  }
  
  for(k in 1:nrow(nn)){
    out[nn[k,1],nn[k,2]] = pik[nn[k,1]]*pik[nn[k,2]]*(1- nn[k,3])
  }
  
  # image(as(as.matrix(out),"sparseMatrix"))
  
  ##------------- k CROSSBORDER UNIT l NONCROSSBORDER UNIT (row element of out)
  nn <- data.frame(k = NULL,l = NULL, c = NULL)
  for(i in 1:length(cross)){ # j >= i +1
    if(!is.null(cross[[i]])){
      for(u in (i+1):length(noncross)){
        if(!is.null(noncross[[u]])){
          # cat(i,u,"\n")
          nn <- rbind(nn,data.frame( k = rep(cross[[i]][1],length(noncross[[u]])),
                                     l = noncross[[u]],
                                     c = rep(cij(i+1,u,U),length(noncross[[u]])),
                                     b = rep(b[[i]],length(noncross[[u]]))))
          # nn <- rbind(nn,data.frame( k = rep(cross[[i]][1],length(noncross[[u]])),
          #                            l = noncross[[u]],
          #                            c = rep(cij(i,u-1,U),length(noncross[[u]])),
          #                            a = rep(b[[i]],length(noncross[[u]]))))
          
        }
      }
    }
  }
  
  for(k in 1:nrow(nn)){
    out[nn[k,1],nn[k,2]] = pik[nn[k,1]]*pik[nn[k,2]]*(1 - nn[k,4]*(1-pik[nn[k,1]])*(pik[nn[k,1]]*(1-nn[k,4]))^(-1)*nn[k,3])
  }
  
  # image(as(as.matrix(out),"sparseMatrix"))
  
  ##------------- k NONCROSSBORDER UNIT and l BORDER UNIT (column element of out)
  nn <- data.frame(k = NULL,l = NULL, c = NULL)
  for(i in 1:length(cross)){ # j >= i
    if(!is.null(cross[[i]])){
      for(u in i:1){
        # cat(u,i,"\n")
        if(!is.null(noncross[[u]])){
          nn <- rbind(nn,data.frame( k = noncross[[u]],
                                     l = rep(cross[[i]],length(noncross[[u]])),
                                     c = rep(cij(u,i,U),length(noncross[[u]])), # i+1 if bi
                                     a = rep(a[[i]],length(noncross[[u]]))))
        }
      }
    }
  }
  
  for(k in 1:nrow(nn)){
    out[nn[k,1],nn[k,2]] = pik[nn[k,1]]*pik[nn[k,2]]*(1 - (1-pik[nn[k,2]])*(nn[k,4])*(pik[nn[k,2]]*(1 - nn[k,4]))^(-1)*nn[k,3])
  }
  
  # image(as(as.matrix(out),"sparseMatrix"))
  
  ##------------- k CROSSBORDER UNIT l CROSSBORDER UNIT
  nn <- data.frame(k = NULL,l = NULL, c = NULL)
  for(i in 1:(length(cross)-1)){
    if(!is.null(cross[[i]])){
      for(u in (i+1):length(cross)){
        if(!is.null(cross[[u]])){
        # cat(i,u,"\n")
        nn <- rbind(nn,data.frame( k = cross[[i]],
                                   l = cross[[u]],
                                   c = cij(i+1,u,U),
                                   b1 = b[[i]],
                                   # b2 = b[[u]],
                                   # a1 = a[[i]],
                                   a2 = a[[u]]))
        }
      }
    }
  }
  
  for(k in 1:nrow(nn)){
    out[nn[k,1],nn[k,2]] = pik[nn[k,1]]*pik[nn[k,2]] - nn[k,3]*((1-pik[nn[k,1]])*(1-pik[nn[k,2]]) * ((nn[k,4])*(nn[k,5])) *((1-nn[k,4])*(1-nn[k,5]))^(-1))
  }
  
  
  ## return matrix
  out <- out + t(out)
  out <- out + diag(pik)
  
  # rowSums(out)
  
  return(out)
  
}

