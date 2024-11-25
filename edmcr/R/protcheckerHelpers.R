bondcheck <- function(X,E){
  
  d <- nrow(X)
  n <- ncol(X)
  ne <- nrow(E)
  
  err <- matrix(0, ne,1)
  
  for(i in 0:(ne-1)){
    ti <- (E[i+1] - 1) * d
    tj <- (E[i + ne + 1] - 1) * d
    target <- E[i+ 2*ne + 1]
    dist <- 0
    
    for(j in 0:(d-1)){
      temp <- X[ti + j + 1] - X[tj + j + 1]
      dist <- dist + temp^2
    }
    
    dist <- sqrt(dist)
    temp <- abs(target - dist)
    if(temp > 0.01){
      err[i+1] <- temp
    }
    
  }
  return(err)
}

lobound <- function(X, L){
  
  d <- nrow(X)
  n <- ncol(X)
  nl <- nrow(L)
  
  err <- matrix(0, nl,1)
  
  for(i in 0:(nl-1)){
    ti <- (L[i+1] - 1) * d
    tj <- (L[i + nl + 1] - 1) * d
    target <- L[i+ 2*nl + 1]
    dist <- 0
    
    for(j in 0:(d-1)){
      temp <- X[ti + j + 1] - X[tj + j + 1]
      dist <- dist + temp^2
    }
    
    dist <- sqrt(dist)
    temp <- target - dist
    if(temp > 0){
      err[i+1] <- temp
    }
    
  }
  return(err)
}

upbound <- function(X, U){
  d <- nrow(X)
  n <- ncol(X)
  nu <- nrow(U)
  
  err <- matrix(0, nu,1)
  
  for(i in 0:(nu-1)){
    ti <- (U[i+1] - 1) * d
    tj <- (U[i + nu + 1] - 1) * d
    target <- U[i+ 2*nu + 1]
    dist <- 0
    
    for(j in 0:(d-1)){
      temp <- X[ti + j + 1] - X[tj + j + 1]
      dist <- dist + temp^2
    }
    
    dist <- sqrt(dist)
    temp <- target - dist
    if(temp < 0){
      err[i+1] <- -temp
    }
  }
  return(err)
}

chirality_check <- function(X, Comp, print_flag = 0){
  
  any_error <- 0
  
  seq <- Comp$seq
  num_seq <- Comp$num_seq
  atom_names <- Comp$atom_names
  
  num_res <- length(seq) - 1
  out <- rep(TRUE, num_res)
  
  for(i in 1:num_res){
    res <- seq[i]
    if(res != 8){
      chiral_atoms <- c("CA", "N", "C", "CB")
      chiral_atoms_index <- rep(NaN, 4)
      
      temp_atoms <- (Comp$residue == num_seq[i])
      temp_X <- X[,temp_atoms]
      temp_atom_names <- atom_names[temp_atoms]
      for(j in 1:4){
        for(k in 1:length(temp_atom_names)){
          if(chiral_atoms[j] == temp_atom_names[[k]]){
            chiral_atoms_index[j] <- k
            break
          }
        }
      }

      chir_X <- temp_X[,chiral_atoms_index]
      chir_angle <- dicalc(chir_X)
      if(chir_angle < 0){
        out[i] <- FALSE
        any_error <- 1
      }
    }
  }
  return(out)
}

ang_checker <- function(X, Comp, plot_flag){
  
  num_planes <- nrow(Comp$planes)
  
  num_residues <- length(unique(Comp$residue))
  phi <- matrix(NaN, num_residues,1)
  psi <- matrix(NaN, num_residues,1)
  
  psi_X <- cbind(X[,1], X[,Comp$planes[1,c(1,2,4)]])
  psi[1] <- dicalc(psi_X)
  
  for(i in 1:(num_planes-1)){
    if(i < (num_planes-1)){
      psi_atoms <- cbind(Comp$planes[i,c(4,6)], Comp$planes[i+1,c(2,4)])
      psi_X <- X[,psi_atoms]
      psi[i+1] <- dicalc(psi_X)
    }
    
    phi_atoms <- cbind(Comp$planes[i,c(2,4,6)], Comp$planes[i+1,2])
    phi_X <- X[,phi_atoms]
    phi[i+1] <- dicalc(phi_X)
  }
  
  psi <- 180*psi/pi
  phi <- 180*phi/pi
  
  return(list(psi=psi, phi=phi))
  
}

vector.cross <- function(a, b) {
  if(length(a)!=3 || length(b)!=3){
    stop("Cross product is only defined for 3D vectors.");
  }
  i1 <- c(2,3,1)
  i2 <- c(3,1,2)
  return (a[i1]*b[i2] - a[i2]*b[i1])
}

dicalc <- function(X){
  
  v1 <- X[,2] - X[,1]
  v2 <- X[,3] - X[,2]
  v3 <- X[,4] - X[,3]
  
  arg1 <- norm(v2, type = "2")*v1 %*%  vector.cross(v2, v3)
  arg2 <- vector.cross(v1, v2) %*% vector.cross(v2, v3)
  
  angle <- atan2(arg1, arg2)
  
  return(angle)
  
}

