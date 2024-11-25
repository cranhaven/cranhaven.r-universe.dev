Prod2 <- function(blk,p,A,B,options=0){
  
  iscellA <- is.list(A)
  iscellB <- is.list(B)
  
  if(!iscellA & !iscellB){
    if(blk[[p,1]] == "s"){
      C <- A %*% B
      if(options == 1){
        C <- .5*(C + t(C))
      }
    }else if(blk[[p,1]] == "q" | blk[[p,1]] == "l" | blk[[p,1]] == "u"){
      C <- A * B
    }
  }else{
    stop("Error in Prod2: A, B must be matrices")
  }
  return(C)
}