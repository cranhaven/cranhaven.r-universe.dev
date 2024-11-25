upper_maker <- function(dist_con, Comp){
  
  max_i <- nrow(dist_con)
  upper_bounds <- matrix(NaN, nrow=max_i, ncol=4)
  
  max_res <- max(Comp$residue)
  min_res <- min(Comp$residue)
  
  for(i in 1:max_i){
    sres <- dist_con$sres[i]
    tres <- dist_con$tres[i]
    
    if(sres <= max_res && sres >= min_res && tres <= max_res && tres >= min_res){
      satom <- dist_con$satom[i]
      tatom <- dist_con$tatom[i]
      
      index_s <- which(Comp$residue == sres)
      index_t <- which(Comp$residue == tres)
      satom_names <- unlist(Comp$atom_names[index_s])
      tatom_names <- unlist(Comp$atom_names[index_t])
      
      local_s <- which(satom_names == satom)
      local_t <- which(tatom_names == tatom)
      
      global_s <- index_s[which(index_s != 0)][1:local_s]
      global_t <- index_t[which(index_t != 0)][1:local_t]
      
      upper_bounds[i,1] <- global_s[local_s]
      upper_bounds[i,2] <- global_t[local_t]
      
      if(is.nan(upper_bounds[i,1]*upper_bounds[i,2])){
        stop("Unknown atom name")
      }
      
      upper_bounds[i,3] <- dist_con$dist[i]
      upper_bounds[i,4] <- dist_con$peak[i]
    }
  }
  
  index_bad <- which(is.nan(rowSums(upper_bounds)))
  if(length(index_bad) > 0){
    upper_bounds <- upper_bounds[-index_bad,]
  }
  return(upper_bounds)
  
}

equality_con_former <- function(X, Comp, type=1){
  
  Cq <- Comp$Cq
  num_atoms <- nrow(Cq)
  num_cliq <- ncol(Cq)
  
  G <- matrix(0, nrow=num_atoms, ncol=num_atoms)
  #Dummy Constraint beween H and HA of first residue
  G[2,4] <- 1

  for(i in 1:num_cliq){
    
    index_cliq <- which(Cq[,i] != 0)
    dim <- Comp$cliq.dims[i]
    base <- BaseFormer(as.matrix(X[,index_cliq]), dim)
    base <- index_cliq[base]
    
    for(j in 1:(dim+1)){
      if((j+1) <= (dim+1)){
        for(k in (j+1):(dim+1)){
          tj <- base[j]
          tk <- base[k]
          G[tj,tk] <- 1
        }
      }
    }

    if(type > 1){
      num_points_cliq <- length(index_cliq)
      for(j in 1:num_points_cliq){
        if((j+1) <= num_points_cliq){
          for(k in (j+1):num_points_cliq){
            tj <- index_cliq[j]
            tk <- index_cliq[k]
            G[tj,tk] <- 1
          }
        }
      }
    }
    
    if(type < 0){
      index_cliq <- which(Cq[,i] != 0)
      dim <- Comp$cliq.dims[i]
      base <- BaseFormer(X[,index_cliq],dim)
      base <- index_cliq[base]
      for(j in 1:(dim+1)){
        if((j+1) <= (dim+1)){
          for(k in (j+1):(dim+1)){
            tj <- base[j]
            tk <- base[k]
            G[tj,tk] <- 1
          }
        }
      }
      index_nonbase <- index_cliq[!(index_cliq %in% base)]
      if(length(index_nonbase) > 0){
        for(j in 1:(dim+1)){
          for(k in 1:length(index_nonbase)){
            tj <- base[j]
            tk <- index_nonbase[k]
            G[tj,tk] <- 1
          }
        }
      }
    }
  }
  
  ij_index <- which(G != 0, arr.ind=TRUE)
  
  #row index
  I <- ij_index[,1]
  
  #column index
  J <- ij_index[,2]
    
  D <- as.matrix(dist(t(X)))
  Vals <- D[(J-1)*num_atoms + I]
  
  equality_cons <- cbind(I, J, Vals)
  return(equality_cons)
}

BaseFormer <- function(X,d){
  num <- ncol(X)
  
  if(num < (d+1)){
    out <- 1:num
  }else{
    rankX <- pcarank(X)
    rand_index <- sample(num)
    test_points <- X[,rand_index[1:(rankX+1)]]
    while(pcarank(test_points) < rankX){
      rand_index <- sample(num)
      test_points <- X[,rand_index[1:(rankX+1)]]
    }
    out <- rand_index[1:(rankX + 1)]
  }
  
  return(out)
  
}

vdw_bound_maker <- function(Comp){
  
  permitted_penetration <- .9
  
  num_atoms <- ncol(Comp$info)
  G <- matrix(1,nrow=num_atoms, ncol=num_atoms)
  
  Cq <- Comp$Cq
  num_cliques <- ncol(Cq)
  
  pseudo_atoms <- (Comp$info[5,] == 0)
  G[pseudo_atoms,] <- 0
  G[,pseudo_atoms] <- 0
  
  for(q in 1:num_cliques){
    index_cliq <- which(Cq[,q] != 0)
    cliq_size <- length(index_cliq)
    for(i in 1:cliq_size){
      for(j in (i+1):cliq_size){
        G[index_cliq[i],index_cliq[j]] <- 0
      }
    }
  }
  
  G <- pmin(G,t(G))
  G[lower.tri(G, diag=TRUE)] <- 0
  
  ij.index <- which(G != 0, arr.ind=TRUE)
  I <- ij.index[,1]
  J <- ij.index[,2]
  max_i <- length(I)
  lo_bounds <- matrix(0,nrow=max_i, ncol=4)
  for(i in 1:max_i){
    lo_bounds[i,1:3] <- c(I[i], J[i], Comp$info[2,I[i]] + Comp$info[2,J[i]])
  }
  lo_bounds[,3] <- lo_bounds[,3]*permitted_penetration
  lo_bounds[,4] <- 1
  
  return(lo_bounds)
}

map_bounds <- function(in_bounds, atoms_map){
  
  num_bounds <- nrow(in_bounds)
  num_cols <- ncol(in_bounds)
  
  bounds <- matrix(NaN,nrow=num_bounds, ncol=num_cols)
  
  for(i in 1:num_bounds){
    
    if(in_bounds[i,1]*in_bounds[i,2] == 0){
      stop("There is an error in mapping atoms")
    }
    bounds[i,c(1,2)] <- atoms_map[in_bounds[i,c(1,2)],2]
    bounds[i,3:num_cols] <- in_bounds[i,3:num_cols]
  }
  
  if(any(is.nan(bounds))){
    stop("There is an error in mapping atoms")
  }
  
  return(bounds)
  
}