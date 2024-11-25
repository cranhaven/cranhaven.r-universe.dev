#' Create a Symmetrix Matrix
#'
#'\code{smat} takes a vector and creates a symmetrix matrix
#'
#'@param blk Lx2 matrix detailing the type of matrices ("s", "q", "l", "u"), and the size of each matrix
#'@param p Row of blk to be used during matrix creation
#'@param At vector to be turned into a symmetric matrix
#'@param isspM if At is sparse, isspx = 1, 0 otherwise. Default is to assume M is dense.
#'
#'@return
#'\item{M}{A Symmetric Matrix}
#'
#'@examples
#'
#' y <- c(1,0.00000279, 3.245, 2.140, 2.44, 2.321, 4.566)
#' 
#' blk <- matrix(list(),1,2)
#' blk[[1,1]] <- "s"
#' blk[[1,2]] <- 3
#' 
#' P <- smat(blk,1, y)
#'
#' @export
smat <- function(blk,p,At,isspM=NULL){
  
  if(is.null(isspM)){
    isspM <- rep(0,nrow(blk))
  }
  if(!is.list(At)){
    if(blk[1] == "s"){
      M <- mexsmat(blk,At,isspM)
    }else{
      M <- At
    }
  }else{
    M <- matrix(list(), nrow(blk),1)
    if(length(isspM) == 1){
      isspM <- rep(isspM, nrow(blk))
    }
    for(p in 1:nrow(blk)){
      if(blk[[p,1]] == "s"){
        M[[p]] <- mexsmat(blk[p,,drop=FALSE],At[p,,drop=FALSE],isspM[p])
      }else{
        M[[p]] <- At[[p]]
      }
    }
  }
  return(M)
  
}
