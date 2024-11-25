chirality_correction <- function(X, Comp, chiral_err){
  
  seq <- Comp$seq
  num_seq <- Comp$num_seq
  atom_names <- Comp$atom_names
  atom_types <- Comp$atom_types
  residue_bias <- Comp$residue_bias
  
  num_res <- length(seq) - 1
  outX <- X
  
  for(i in 1:num_res){
    res <- seq[i]
    if(!chiral_err[i] && res != 8){
      chiral_atoms <- c("CA", "N", "C", "CB", "HA")
      chiral_atoms_index <- rep(NaN,5)
      temp_atoms <- which(Comp$residue == num_seq[i])
      temp_X <- X[,temp_atoms]
      temp_atom_names <- atom_names[temp_atoms]
      temp_atom_types <- atom_types[temp_atoms]
      for(j in 1:5){
        for(k in 1:length(temp_atom_names)){
          if(chiral_atoms[j] == temp_atom_names[k]){
            chiral_atoms_index[j] <- k
            break
          }
        }
      }
      
      cX <- temp_X[,chiral_atoms_index]
      
      v1 <- cX[,2] - cX[,1] #CA-N vector
      v2 <- cX[,3] - cX[,1] #CA-C vector
      n <- vector.cross(v1,v2) #CA-N-C plane normal vector
      n <- n/base::norm(n, type="2")
      
      side_chain <- which(temp_atom_types > 1)
      
      sChain <- temp_X[,side_chain]
      tSChain <- matrix(NaN, nrow=3, ncol=length(side_chain))
      for(j in 1:length(side_chain)){
        tSChain[,j] <- plane_reflector(cX[,1], n, sChain[,j])
      }
      outX[,(side_chain + residue_bias[i])] <- tSChain
    }
  }
  
  return(outX)
  
}

plane_reflector <- function(O,n,P){
  v <- P - O
  vn <- (t(v) %*% n) %*% n
  vp <- v - vn
  vr <- vp - vn
  out <- O + vr
  return(out)
}

hydrogen_mapper <- function(X, orig_rX, Comp, orig_Comp, A){
  
  seq <- Comp$seq
  num_res <- length(seq)
  num_atoms <- ncol(orig_rX)
  oX <- matrix(NaN, nrow=3, ncol=num_atoms)
  
  for(i in 1:(num_res-1)){
    index_orig <- which(orig_Comp$residue == Comp$num_seq[i])
    index_reduced <- which(Comp$residue == Comp$num_seq[i])
    
    cur_num_atoms <- length(index_orig)
    
    toX <- matrix(NaN, nrow=3, ncol=cur_num_atoms)
    tX <- X[,index_reduced]
    ind <- Comp$atoms_map[index_orig,]
    ind <- !is.nan(ind[,2])
    toX[,ind] <- tX
    
    sX <- orig_rX[,index_orig]
    
    num_anchors <- nrow(A[[seq[i]]]$anchors)

    if(length(num_anchors) > 0){
      for(j in 1:num_anchors){
        temp_missing <- A[[seq[i]]]$anchors[[j,1]]
        temp_available <- A[[seq[i]]]$anchors[[j,2]]
        
        refX <- toX[,temp_available]
        mapX <- sX[,temp_available]
        
        reflection <- 1
        out <- procrustesb(refX,mapX,reflection)
        R <- out$R
        t <- out$t
        err <- out$err
        
        if(err > 5e-1){
          warning("Error is too large")
        }
        
        temp_toX <- R %*% sX[,c(temp_missing, temp_available)]
        temp_toX <- sweep(temp_toX,1,t)
        
        toX[,temp_missing] <- temp_toX[,1:length(temp_missing)]
      }
    }
    oX[,index_orig] <- toX
  }
  
  return(oX)
  
}

hanso_post_processing <- function(ref_X, X0, Comp, lo_bounds, up_bounds, w, f){
  
  equality_cons <- equality_con_former(ref_X, Comp, 2)
  
  ow <- w
  w[4] <- 0.0
  
  obj <- obj_function(X0, equality_cons, lo_bounds, up_bounds, w, f)
  
  w[4] <- ow[4]*obj/(25*sum(diag(t(X0) %*% X0)))
  
  initialGuess <- as.vector(X0)
  maxiter <- 250
  toler <- 1e-9
  
  out <- optim(par=initialGuess, 
               fn = obj_function_wrapper, 
               gr = grad_function_wrapper,
               method = "BFGS",
               E = equality_cons,
               L = lo_bounds,
               U = up_bounds,
               w=w,
               f=f,
               nrow=nrow(X0),
               ncol = ncol(X0),
               control = list(maxit = maxiter))
  
  x <- out$par
  X <- matrix(x, nrow=nrow(X0), ncol=ncol(X0))
  return(X)
}

obj_function <- function(X, E, L, U, w, f){
  
  #X Info
  d <- nrow(X)
  n <- ncol(X)
  
  #Equality Constraint Info
  ne <- nrow(E)
  
  #Lower Bound Info
  nl <- nrow(L)
  
  #Upper Bound Info
  nu <- nrow(U)
  
  #w info
  we <- w[1]
  wl <- w[2]
  wu <- w[3]
  wr <- w[4]
  
  #f info
  
  f_hb <- f[1]
  f_tau <- f[1]
  f_tal <- f[2]
  f_vdw <- f[4]
  
  #Objective Function Value
  obj <- 0
  
  #Equality Constraints
  
  for(i in 0:(ne-1)){
    ti <- (E[i + 1] - 1) * d
    tj <- (E[i + ne + 1] - 1) * d
    target <- E[i + 2*ne + 1]
    
    dist <- 0
    
    for(j in 0:(d-1)){
      temp <- X[ti+j+1] - X[tj+j+1]
      dist <- dist + temp^2
    }
    dist <- sqrt(dist)
    temp <- target - dist
    obj <- obj + we*(temp)^2
  }
  
  #Lower Bounds
  
  for(i in 0:(nl-1)){
    ti <- (L[i + 1] - 1) * d
    tj <- (L[i + nl + 1] - 1) * d
    target <- L[i + 2*nl + 1]
    type <- L[i + 3*nl + 1]
    dist <- 0
    
    for(j in 0:(d-1)){
      temp <- X[ti + j + 1] - X[tj + j + 1]
      dist <- dist + temp^2
    }
    dist <- sqrt(dist)
    temp <- target - dist
    if(temp > 0){
      if(type == 1){
        obj <- obj + f_vdw*wl*temp^2
      }else if(type == -2){
        obj <- obj + f_tal*wl*temp^2
      }
    }
  }
  
  #Upper Bounds
  
  for(i in 0:(nu-1)){
    ti <- (U[i + 1] - 1) * d
    tj <- (U[i + nu + 1] - 1) * d
    target <- U[i + 2*nu + 1]
    type <- U[i + 3* nu + 1]
    dist <- 0
    
    for(j in 0:(d-1)){
      temp <- X[ti + j + 1] - X[tj + j + 1]
      dist <- dist + temp^2
    }
    
    dist <- sqrt(dist)
    temp <- target - dist
    
    if(temp < 0){
      if(type >= 0){
        obj <- obj + wu*temp^2
      }else if(type == -1){
        obj <- obj + f_hb*wu*temp^2
      }else if(type == -2){
        obj <- obj + f_tau*wu*temp^2
      }
    }
  }
  
  #Regularization
  
  for(i in 0:(n-1)){
    ti <- i*d
    dist <- 0
    for(j in 0:(d-1)){
      temp <- X[ti + j + 1]
      dist <- dist + temp^2
    }
    obj <- obj + wr*dist
  }
  
  return(obj)
  
}

grad_function <- function(X, E, L, U, w, f){
  
  #X Info
  d <- nrow(X)
  n <- ncol(X)
  
  #Equality Constraint Info
  ne <- nrow(E)
  
  #Lower Bound Info
  nl <- nrow(L)
  
  #Upper Bound Info
  nu <- nrow(U)
  
  #w info
  we <- 2*w[1]
  wl <- 2*w[2]
  wu <- 2*w[3]
  wr <- 2*w[4]
  
  #f info
  
  f_hb <- f[1]
  f_tau <- f[2]
  f_tal <- f[3]
  f_vdw <- f[4]
  
  G <- matrix(0, nrow=d, ncol=n)
  
  #Equality Constraints
  
  for(i in 0:(ne-1)){
    ti <- (E[i + 1] - 1) * d
    tj <- (E[i + ne + 1] - 1) * d
    target <- E[i + 2*ne + 1]
    dist <- 0
    
    for(j in 0:(d-1)){
      temp <- X[ti + j + 1] - X[tj + j + 1]
      dist <- dist + temp^2
    }
    
    sdist <- sqrt(dist)
    tempG <- 1 - target/sdist
    
    for(j in 0:(d-1)){
      G[ti + j + 1] <- G[ti + j + 1] + we*(X[ti+j+1] - X[tj + j + 1])*tempG
      G[tj + j + 1] <- G[tj + j + 1] - we*(X[ti+j+1] - X[tj + j + 1])*tempG
    }
  }
  
  #Lower Bounds
  
  for(i in 0:(nl-1)){
    ti <- (L[i + 1] - 1)*d
    tj <- (L[i + nl + 1] - 1)*d
    target <- L[i + 2*nl + 1]
    type <- L[i + 3*nl + 1]
    dist <- 0
    
    for(j in 0:(d-1)){
      temp <- X[ti + j + 1] - X[tj + j + 1]
      dist <- dist + temp^2
    }
    
    sdist <- sqrt(dist)
    tempG <- 1 - target/sdist
    if(sdist < target){
      for(j in 0:(d-1)){
        if(type == 1){
          G[ti + j + 1] <- G[ti + j + 1] + f_vdw*wl*(X[ti + j + 1] - X[tj + j + 1])*tempG
          G[tj + j + 1] <- G[tj + j + 1] - f_vdw*wl*(X[ti + j + 1] - X[tj + j + 1])*tempG
        }else if(type == -2){
          G[ti + j + 1] <- G[ti + j + 1] + f_tal*wl*(X[ti + j + 1] - X[tj + j + 1])*tempG
          G[tj + j + 1] <- G[tj + j + 1] - f_tal*wl*(X[ti + j + 1] - X[tj + j + 1])*tempG
        }else{
          stop("Error! Unknown Lower Bound!")
        }
      }
    }
  }
  
  #Upper Bounds
  
  for(i in 0:(nu-1)){
    ti <- (U[i + 1] - 1)*d
    tj <- (U[i + nu + 1] - 1)*d
    target <- U[i + 2*nu + 1]
    type <- U[i + 3*nu + 1]
    dist <- 0
    
    for(j in 0:(d-1)){
      temp <- X[ti + j + 1] - X[tj + j + 1]
      dist <- dist + temp^2
    }
    
    sdist <- sqrt(dist)
    tempG <- 1 - target/sdist
    if(sdist > target){
      for(j in 0:(d-1)){
        if(type >= 0){
          G[ti + j + 1] <- G[ti + j + 1] + wu*(X[ti + j + 1] - X[tj + j + 1])*tempG
          G[tj + j + 1] <- G[tj + j + 1] - wu*(X[ti + j + 1] - X[tj + j + 1])*tempG
        }else if(type == -1){
          G[ti + j + 1] <- G[ti + j + 1] + f_hb*wu*(X[ti + j + 1] - X[tj + j + 1])*tempG
          G[tj + j + 1] <- G[tj + j + 1] - f_hb*wu*(X[ti + j + 1] - X[tj + j + 1])*tempG
        }else if(type == -2){
          G[ti + j + 1] <- G[ti + j + 1] + f_tau*wu*(X[ti + j + 1] - X[tj + j + 1])*tempG
          G[tj + j + 1] <- G[tj + j + 1] - f_tau*wu*(X[ti + j + 1] - X[tj + j + 1])*tempG
        }else{
          stop("Error! Unknown Upper Bound!")
        }
      }
    }
  }
  
  #Regularization
  
  for(i in 0:(n-1)){
    ti <- i*d
    for(j in 0:(d-1)){
      G[ti+j+1] <- G[ti+j+1] + wr*X[ti+j+1]
    }
  }
  
  return(G)
  
}

obj_function_wrapper <- function(X, E, L, U, w, f, nrow, ncol){
  
  X <- matrix(X, nrow=nrow, ncol=ncol)
  out <- obj_function(X, E, L, U, w, f)
  return(out)
}

grad_function_wrapper <- function(X, E, L, U, w, f, nrow, ncol){
  X <- matrix(X, nrow=nrow, ncol=ncol)
  out <- grad_function(X, E, L, U, w, f)
  out <- as.vector(out)
  return(out)
}