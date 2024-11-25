validate_startpoint <- function(blk, X0, Z0, spdensity=.4, iscmp=0){
  
  if(!is.matrix(X0) | !is.matrix(Z0)){
    stop("validate_startpoint: X0 and Z0 must be matrix arrays")
  }
  
  if(min(dim(X0)) != 1 | min(dim(Z0)) != 1){
    stop("validate_startpoint: matrix array X, Z can only have 1 column or row")
  }
  
  if(ncol(X0) > nrow(X0)){
    X0 <- t(X0)
  }
  
  if(ncol(Z0) > nrow(Z0)){
    Z0 <- t(Z0)
  }
  
  for(p in 1:nrow(blk)){
    n <- sum(blk[[p,2]])
    n2 <- sum(blk[[p,2]]*blk[[p,2]])
    numblk <- max(dim(as.matrix(blk[[p,2]])))
    
    if(blk[[p,1]] == "s"){
      if(iscmp){
        X0[[p]] <- rbind(cbind(Re(X0[[p]]), -Im(X0[[p]])), cbind(Im(X0[[p]]), Re(X0[[p]])))
        Z0[[p]] <- rbind(cbind(Re(Z0[[p]]), -Im(Z0[[p]])), cbind(Im(Z0[[p]]), Re(Z0[[p]])))
      }
      
      if(!all(dim(X0[[p]]) == n) | !all(dim(Z0[[p]]) == n)){
        stop("validate_startpoint: blk and X0,Z0 are not compatible")
      }
      
      if(norm(cbind(X0[[p]] - t(X0[[p]]), Z0[[p]] - t(Z0[[p]])), type="I") > 2e-13){
        stop("validate_startpoint: X0, Z0 not symmetric")
      }
      
      if(length(which(X0[[p]] != 0)) < spdensity*n2 | numblk > 1){
        if(!is(X0[[p]], 'sparseMatrix')){
          X0[[p]] <- Matrix(X0[[p]], sparse=TRUE)
        }
      }else{
        if(is(X0[[p]], 'sparseMatrix')){
          X0[[p]] <- matrix(X0[[p]], ncol=ncol(X0[[p]]), nrow=nrow(X0[[p]]))
        }
      }
      
      if(length(which(Z0[[p]] != 0)) < spdensity*n2 | numblk > 1){
        if(!is(Z0[[p]], 'sparseMatrix')){
          Z0[[p]] <- Matrix(Z0[[p]], sparse=TRUE)
        }
      }else{
        if(is(Z0[[p]], 'sparseMatrix')){
          Z0[[p]] <- matrix(c(Z0[[p]]), nrow(Z0[[p]]),ncol(Z0[[p]]))
        }
      }
    }else if(blk[[p,1]] == "q" | blk[[p,1]] == "l" | blk[[p,1]] == "u"){
      
      if(!all(ncol(X0[[p]]) == 1, ncol(Z0[[p]]) == 1)){
        stop("X0 or Z0 not specified correctly")
      }
      
      if(!all(nrow(X0[[p]]) == n, nrow(Z0[[p]]) == n)){
        stop("blk, X0 and Z0 not compatible")
      }
      
      if(length(which(X0[[p]] != 0)) < spdensity*n){
        if(!is(X0[[p]], 'sparseMatrix')){
          X0[[p]] <- Matrix(X0[[p]], sparse=TRUE)
        }
      }else{
        if(is(X0[[p]], 'sparseMatrix')){
          X0[[p]] <- matrix(X0[[p]], ncol=ncol(X0[[p]]), nrow=nrow(X0[[p]]))
        }
      }
      
      if(length(which(Z0[[p]] != 0)) < spdensity*n){
        if(!is(Z0[[p]], 'sparseMatrix')){
          Z0[[p]] <- Matrix(Z0[[p]], sparse=TRUE)
        }
      }else{
        if(is(Z0[[p]], 'sparseMatrix')){
          Z0[[p]] <- matrix(Z0[[p]], nrow(Z0[[p]]),ncol(Z0[[p]]))
        }
      }
    }
    
    if(blk[[p,1]] == "q"){
      s <- 1 + c(0, cumsum(blk[[p,2]]))
      len <- max(dim(as.matrix(blk[[p,2]])))
      if(any(X0[[p]][s[1:len]] < 1e-12) | any(Z0[[p]][s[1:len]] < 1e-12)){
        stop("X0 or Z0 is not in socp cone")
      }
    }
  }
  return(list(X=X0, Z=Z0))
}