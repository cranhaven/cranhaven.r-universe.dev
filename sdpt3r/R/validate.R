validate <- function(blk, At, C, b, par=NULL, parbarrier=NULL){
  
  if(!is.null(par)){
    spdensity <- par$spdensity
  }else{
    spdensity <- 0.4
  }
##
  if(!is.matrix(blk)){
    stop("validate: blk must be a matrix array")
  }
  
  if(ncol(blk) < 2){
    stop("validate: blk must be a matrix array with at least 2 columns")
  }
  
  if(!is.matrix(At) | !is.matrix(C)){
    stop("validate: At, C must be matrix arrays")
  }
  
  if(nrow(At) != nrow(blk)){
    if(ncol(At) == nrow(blk)){
      At <- t(At)
    }else{
      stop("validate: At not compatible with blk")
    }
  }
  
  if(nrow(C) != nrow(blk)){
    if(ncol(C) == nrow(blk)){
      C = t(C)
    }else{
      stop("C is not compatible with blk")
    }
  }
  
  if(min(nrow(b),ncol(b)) > 1){
    stop("b must be a vector")
  }
  if(nrow(b) < ncol(b)){
    b = t(b)
  }
  
  m <- length(b)
  
  ###################################
  ####### VALIDATE blk, At,C ########
  ###################################
  ntotal <- c()
  for(p in 1:nrow(blk)){
    
    if(nrow(as.matrix(blk[[p,2]])) > ncol(as.matrix(blk[[p,2]]))){
      blk[[p,2]] <- t(blk[[p,2]])
    }
    n <- sum(blk[[p,2]])
    numblk <- length(blk[[p,2]])
    if(blk[[p,1]] == "s"){
      m1 <- ncol(At[[p,1]])
      n2 <- sum(blk[[p,2]] * blk[[p,2]])
      n22 <- sum(blk[[p,2]] * (blk[[p,2]] + 1))/2
      ntotal <- c(ntotal, n22)
      if(!all(dim(as.matrix(C[[p,1]])) == n)){
        stop("blk and C not compatible")
      }

      if(Matrix::norm(C[[p,1]] - t(C[[p,1]]), type="I") > 1e-13 * Matrix::norm(C[[p,1]], type="I")){
        stop("C is not symmetric")
      }
      if(all(dim(At[[p,1]]) == c(m1, n22)) & m1 != n22){
        At[[p,1]] <- t(At[[p,1]])
      }
      if(length(At[[p,1]]) > 0 & nrow(At[[p,1]]) != n22){
        stop("blk and At not compatible")
      }
      
      if(sum(At[[p,1]] == 0) < spdensity*n22*m1){
        At[[p,1]] <- Matrix(At[[p,1]], sparse = TRUE)
      }
      if(ncol(blk) > 2){
        len <- sum(blk[[p,3]])
        
        if(ncol(blk[[p,3]]) < nrow(blk[[p,3]])){
          blk[[p,3]] <- t(blk[[p,3]])
        }
        
        if(any((dim(At[[p,2]]) - c(n,len)) != 0)){
          stop("low rank structure specified in blk and At not compatible")
        }
        
        if(ncol(At) > 2 & length(At[[p,3]]) > 0){
          if(all(rep(ncol(At[[p,3]]),2) - c(1,4) != 0)){
            stop("low rank structure specified in At[[p,3]] not specified correctly")
          }
          if(ncol(At[[p,3]]) == 1){
            if(nrow(At[[p,3]]) < ncol(At[[p,3]])){
              At[[p,3]] <- t(At[[p,3]])
            }
            lenn <- max(dim(At[[p,3]]))
            constrnum <- mexexpand(blk[[p,3]], c(1:max(dim(blk[[p,3]]))))
            At[[p,3]] <- cbind(constrnum, 1:lenn, 1:lenn, At[[p,3]])
          }else if(ncol(At[[p,3]]) == 4){
            dd <- At[[p,3]]
            idxsort <- order(dd[,1])
            dd <- dd[idxsort,]
            lenn <- nrow(dd)
            idxD <- c(0, which(diff(dd[,1]) != 0), lenn)
            ii <- rep(0,lenn)
            jj <- rep(0, lenn)
            ss <- c(0, colSums(blk[[p,3]]))
            for(k in 1:max(dim(blk[[p,3]]))){
              idx <- c((idxD[k]+1):(idxD[k+1]))
              ii[idx] <- dd[idx,2] + ss[k]
              jj[idx] <- dd[idx,3] + ss[k]
            }
            At[[p,3]] <- rbind(dd[,1], ii,jj,dd[,4])
          }
        }else{
          constrnum <- mexexpand(blk[[p,3]], c(1:max(dim(blk[[p,3]]))))
          At[[p,3]] <- cbind(constrnum, 1:len, 1:len, rep(1,len))
        }
      }
      
      if(length(which(C[[p]] == 0)) < spdensity*n2 | numblk > 1){
        C[[p]] <- Matrix(C[[p]], sparse=TRUE)
      }else{
        if(is(C[[p]], 'sparseMatrix')){
          C[[p]] <- as.matrix(C[[p]])
        }
      }
      
    }else if(blk[[p,1]] == "q" | blk[[p,1]] == "l" | blk[[p,1]] == "u"){
      ntotal <- c(ntotal,n)
      if(min(dim(as.matrix(C[[p]]))) != 1 | max(dim(as.matrix(C[[p]]))) != n){
        stop("validate: blk and C are not compatible")
      }
      
      if(nrow(as.matrix(C[[p]])) < ncol(as.matrix(C[[p]]))){
        C[[p]] <- t(C[[p]])
      }
      
      if(all(dim(At[[p,1]]) == c(m,n)) & m != n){
        At[[p,1]] <- t(At[[p,1]])
      }
      
      if(!all(dim(At[[p,1]]) == c(n,m))){
        stop("validate: blk and At not compatible")
      }
      
      if(!is(At[[p,1]], 'sparseMatrix')){
        At[[p,1]] <- Matrix(At[[p,1]], sparse=TRUE)
      }
      
      if(length(which(C[[p]] == 0)) < spdensity*n){
        
        if(!is(C[[p]], 'sparseMatrix')){
          C[[p]] <- Matrix(C[[p]], sparse=TRUE)
        
        }else{
          C[[p]] <- matrix(C[[p]])
        }
        
      }
      
    }else{
      stop("blk: some fields are not specified correctly")
    }
  }
  
  if(sum(ntotal) < m){
    stop("total dimension of C should be > length(b)")
  }
  
  ###########################
  #### PROBLEM DIMENSION ####
  ###########################
  
  dim <- rep(0,4)
  nnblk <- rep(0,2)
  nn <- rep(0, nrow(blk))
  for(p in 1:nrow(blk)){
    
    if(blk[[p,1]] == "s"){
      dim[1] <- dim[1] + sum(blk[[p,2]])
      nnblk[1] <- nnblk[1] + length(blk[[p,2]])
      nn[p] <- sum(blk[[p,2]])
    }else if(blk[[p,1]] == "q"){
      dim[2] <- dim[2] + sum(blk[[p,2]])
      nnblk[2] <- nnblk[2] + length(blk[[p,2]])
      nn[p] <- sum(blk[[p,2]])
    }else if(blk[[p,1]] == "l"){
      dim[3] <- dim[3] + sum(blk[[p,2]])
      nn[p] <- sum(blk[[p,2]])
    }else if(blk[[p,1]] == "u"){
      dim[4] <- dim[4] + sum(blk[[p,2]])
      nn[p] <- sum(blk[[p,2]])
    }
  }
  
  #########################################
  ######## VALIDATE PARBARRIER ############
  #########################################
  
  if(!is.null(parbarrier)){
    if(!is.matrix(parbarrier)){
      stop("parbarrier must be a matrix array")
    }
  }
  
  if(ncol(parbarrier) > nrow(parbarrier)){
    parbarrier <- t(parbarrier)
  }
  
  for(p in 1:nrow(blk)){
    
    if(nrow(as.matrix(parbarrier[[p]])) > ncol(as.matrix(parbarrier[[p]]))){
      parbarrier[[p]] <- t(parbarrier[[p]])
    }
    
    len <- max(dim(as.matrix(parbarrier[[p]])))
    if(blk[[p,1]] == "s" | blk[[p,1]] == "q"){
      if(len == 1){
        parbarrier[[p]] <- parbarrier[[p]] %*% matrix(1, 1, max(dim(as.matrix(blk[[p,2]])))) 
      }else if(len == 0){
        parbarrier[[p]] <- matrix(0, 1, max(dim(as.matrix(blk[[p,2]]))))
      }else if(len != max(dim(as.matrix(blk[[p,2]])))){
        stop("blk and parbarrier not compatible")
      }
    }else if(blk[[p,1]] == "l"){
      if(len == 1){
        parbarrier[[p]] <- parbarrier[[p]] %*% matrix(1, 1, sum(blk[[p,2]])) 
      }else if(len == 0){
        parbarrier[[p]] <- matrix(0, 1, sum(blk[[p,2]]))
      }else if(len != sum(blk[[p,2]])){
        stop("blk and parbarrier not compatible")
      }
    }else if(blk[[p,1]] == "u"){
      parbarrier[[p]] <- matrix(0, 1, sum(blk[[p,2]]))
    }
  }
  
  return(list(blk=blk, At=At, C=C, b=b, dim=dim, nnblk = nnblk, parbarrier = parbarrier))
  
}