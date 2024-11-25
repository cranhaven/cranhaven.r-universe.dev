#'Control Theory
#'
#'\code{control_theory} creates input for sqlp to solve the Control Theory Problem
#'
#'@details
#' Solves the control theory problem. Mathematical and implementation
#' details can be found in the vignette
#' 
#' @param B a matrix object containing square matrices of size n
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' B <- matrix(list(),2,1)
#' B[[1]] <- matrix(c(-.8,1.2,-.5,-1.1,-1,-2.5,2,.2,-1),nrow=3,byrow=TRUE)
#' B[[2]] <- matrix(c(-1.5,.5,-2,1.1,-2,.2,-1.4,1.1,-1.5),nrow=3,byrow=TRUE)
#'
#' out <- control_theory(B)
#'
#' @export
control_theory <- function(B){
  
  #Error Checking
  stopifnot(is.matrix(B))
  
  for(i in 1:nrow(B)){
    stopifnot(is.matrix(B[[i]]),nrow(B[[i]])==ncol(B[[i]]))
  }
  
  for(i in 2:nrow(B)){
    stopifnot(nrow(B[[i]]) == nrow(B[[i-1]]))
  }
  
  #Define Variables
  blk <- matrix(list(),1,2)
  C <- matrix(list(),1,1)
  
  L <- nrow(B)
  N <- nrow(B[[1]])
  
  m <- N*(N+1)/2+1
  n <- N*(L+2)
  
  blk[[1,1]] <- "s"
  blk[[1,2]] <- N*matrix(1,L+2)
  
  Ctmp <- Matrix(0,n,n)
  Ctmp[(L*N+1):((L+1)*N),(L*N+1):((L+1)*N)] <- Diagonal(N,1)
  C[[1]] <- Ctmp
  
  b <- rbind(matrix(0,m-1,1),1)
  
  A <- matrix(list(),1,N*(N+1)/2+1)
  count <- 1
  for(k in 1:N){
    for(j in k:N){
      ak <- c()
      aj <- c()
      row1 <- c()
      row2 <- c()
      col1 <- c()
      col2 <- c()
      for(l in 1:L){
        G <- B[[l]]
        ak <- rbind(ak,as.matrix(G[k,]))
        aj <- rbind(aj,as.matrix(G[j,]))
        col1 <- rbind(col1,((l-1)*N+j)*matrix(1,N,1))
        col2 <- rbind(col2,((l-1)*N+k)*matrix(1,N,1))
        row1 <- rbind(row1,matrix(((l-1)*N+1):(l*N),ncol=1))
        row2 <- rbind(row2,matrix(((l-1)*N+1):(l*N),ncol=1))
      }
      ek <- rbind(matrix(0,k-1,1),.5,matrix(0,N-k,1))
      ej <- rbind(matrix(0,j-1,1),.5,matrix(0,N-j,1))
      ak <- rbind(ak,ek,-ek)
      aj <- rbind(aj,ej,-ej)
      col1 <- rbind(col1,(L*N+j)*matrix(1,N,1),(L*N+N+j)*matrix(1,N,1))
      col2 <- rbind(col2,(L*N+k)*matrix(1,N,1),(L*N+N+k)*matrix(1,N,1))
      row1 <- rbind(row1, as.matrix((L*N+1):((L+1)*N)),as.matrix((L*N+N+1):((L+2)*N)))
      row2 <- rbind(row2, as.matrix((L*N+1):((L+1)*N)),as.matrix((L*N+N+1):((L+2)*N)))
      
      if(j != k){
        tmp <- Matrix(0,n,n)
        rowtmp <- rbind(row1,row2)
        coltmp <- rbind(col1,col2)
        atmp <- rbind(ak,aj)
        for(i in 1:length(rowtmp)){
          tmp[rowtmp[i],coltmp[i]] <- atmp[i]
        }
      }else if(j ==k){
        tmp <- Matrix(0,n,n)
        for(i in 1:length(row1)){
          tmp[row1[i],col1[i]] <- ak[i]
        }
      }
      A[[count]] <- tmp + t(tmp)
      count <- count + 1
    }
  }
  
  tmp <- Matrix(0,n,n)
  tmp[((L+1)*N+1):((L+2)*N),((L+1)*N+1):((L+2)*N)] <- Diagonal(N,1)
  A[[N*(N+1)/2+1]] <- tmp
  At <- svec(blk,A,matrix(1,nrow(blk),1))
  
  out <- sqlp_base(blk=blk, At=At, b=b, C=C, OPTIONS = list())
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
}