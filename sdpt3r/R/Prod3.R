Prod3 <- function(blk, p, A, B, C, sym=0, nzlistQ=NULL){
  checkcell <- c(is.list(A), is.list(B), is.list(B))
  if(!is.null(nzlistQ)){
    checkcell <- c(checkcell, is.list(nzlistQ))
  }else{
    nzlistQ <- Inf
  }
  ##
  if(any((checkcell - 1) != 0)){
    if(blk[[p,1]] == "s"){
      len <- nrow(as.matrix(nzlistQ))
      len2 <- ncol(as.matrix(nzlistQ))
      if(is.null(len)){
        len <- 0
      }
      if(is.null(len2)){
        len2 <- 0
      }
      
      if(len == 0){
        nzlistQ <- Inf
        len2 <- 1
      }
      if(len2 == 1 & any(is.infinite(nzlistQ))){
        tmp <- Prod2(blk,p,A,B,0)
        Q <- Prod2(blk,p,tmp,C,sym)
      }else{
        tmp <- Prod2(blk,p,B,C,0)
        Q <- mexProd2nz(blk,p,A,tmp,nzlistQ)
        if(sym){
          Q <- 0.5*(Q + t(Q))
        }
      }
    }else if(blk[[p,1]] == "q" | blk[[p,1]] == "l" | blk[[p,1]] == "u"){
      Q <- A * B * C
    }
  }
  return(Q)
}