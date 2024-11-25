#' Distance Weighted Discrimination
#'
#'\code{dwd} creates input for sqlp to solve the Distance Weighted Discrimination problem -
#'Given two sets of points An and Ap, find an optimal classification rule to group the points as accurately
#'as possible for future classification.
#'
#'@details
#'
#' Solves the distance weighted discrimination problem. Mathematical and implementation
#' details can be found in the vignette
#' 
#' @param Ap An nxp point configuration matrix
#' @param An An nxp point configuration matrix
#' @param penalty A real valued scalar penalty for moving points across classification rule
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' data(Andwd)
#' data(Apdwd)
#' penalty <- 0.5
#' 
#' #Not Run
#' #out <- dwd(Apdwd,Andwd,penalty)
#' 
#' @export
dwd <- function(Ap,An,penalty){
  
  #Error Checking
  stopifnot(is.matrix(Ap), is.matrix(An), is.numeric(Ap), is.numeric(An), is.numeric(penalty), nrow(Ap)==nrow(An), ncol(Ap)==ncol(An))
  
  #Data input as nxp, program needs it to be pxn
  Ap <- t(Ap)
  An <- t(An)
  
  #Define Variables
  np <- nrow(Ap)
  mp <- ncol(Ap)
  nn <- nrow(An)
  mn <- ncol(An)
  n <- np
  nv <- 1 + n + 2 + 3*(mp + mn) + mp + mn
  nc <- 1 + 2*(mp + mn)
##
  blk <- matrix(list(),2,2)
  At <- matrix(list(),2,1)
  C <- matrix(list(),2,1)
##  
  
  blk[[1,1]] <- "q"
  blk[[1,2]] <- matrix(c(n+1,2,3*rep(1,mp+mn)),nrow=1)
  blk[[2,1]] <- "l"
  blk[[2,2]] <- mp + mn
##
  A <- Matrix(0,nc,nv-mp-mn,sparse=TRUE)
  A[1:mp,2:(n+3)] <- cbind(t(Ap),matrix(0,mp,1),matrix(1,mp,1))
  A[(mp+1):(mp+mn), 2:(n+3)] <- -cbind(t(An),matrix(0,mn,1), matrix(1,mn,1))
  A[1:mp,seq(n+4,n+3+3*mp,3)] <- Diagonal(mp,-1)
  A[1:mp,seq(n+6,n+5+3*mp,3)] <- Diagonal(mp,-1)
  A[(mp+1):(mp+mn),seq(3*mp+n+4,3*mp+n+3+3*mn,3)] <- Diagonal(mn,-1)
  A[(mp+1):(mp+mn),seq(3*mp+n+6,3*mp+n+5+3*mn,3)] <- Diagonal(mn,-1)
  A[mp+mn+1,1] <- 1
  A[(mp+mn+2):(mp+mn+1+mp),seq(n+5,n+4+3*mp,3)] <- Diagonal(mp,1)
  A[(mp+mn+1+mp+1):(mp+mn+1+mp+mn),seq(3*mp+n+5,3*mp+n+4+3*mn,3)] <- Diagonal(mn,1)

  At[[1,1]] <- A
  At[[2,1]] <- rbind(as.matrix(Diagonal(mp+mn,1)),matrix(0,1+mp+mn,mp+mn))
  b <- rbind(matrix(0,mp+mn,1),matrix(1,1+mp+mn,1))
##
  ctmp <- matrix(0,nv-mp-mn,1)
  ctmp[seq(n+4,n+3+3*mp,3)] <- rep(1,mp)
  ctmp[seq(n+6,n+5+3*mp,3)] <- -rep(1,mp)
  ctmp[seq(3*mp+n+4,3*mp+n+3+3*mn,3)] <- rep(1,mn)
  ctmp[seq(3*mp+n+6,3*mp+n+5+3*mn,3)] <- -rep(1,mn)
  
  C[[1,1]] <- ctmp
  C[[2,1]] <- penalty*matrix(1,mp+mn,1)
##
  out <- sqlp_base(blk=blk, At=At, b=b, C=C, OPTIONS = list())
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
}