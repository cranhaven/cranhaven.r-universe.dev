NTpred <- function(blk,At,par,rp,Rd,sigmu,X,Z,Zchol,invZchol){
  
  ##
  ## compute NT scaling matrix
  ##
  out <- NTscaling(blk,X,Z,Zchol,invZchol)
  par$W <- out$W
  par$G <- out$G
  par$sv <- out$sv
  par$gamx <- out$gamx
  par$gamz <- out$gamz
  par$dd <- out$dd
  par$ee <- out$ee
  par$ff <- out$ff
  ##
  ## compute schur matrix
  ##
  m <- length(rp)
  schur <- matrix(0,m,m)
  UU <- c()
  EE <- c()
  Afree <- c()
  dX <- matrix(list(),nrow(blk),1)
  dy <- c()
  dZ <- matrix(list(),nrow(blk),1)
  ##
  for(p in 1:nrow(blk)){
    if(blk[[p,1]] == "l"){
      out <- schurmat_lblk(blk,At,par,schur,UU,EE,p,par$dd)
      schur <- out$schur
      UU <- out$UU
      EE <- out$EE
    }else if(blk[[p,1]] == "q"){
      out <- schurmat_qblk(blk,At,par,schur,UU,EE,p,par$dd,par$ee)
      schur <- out$schur
      UU <- out$UU
      EE <- out$EE
    }else if(blk[[p,1]] == "s"){
      if(length(get("schurfun", pos=sys.frame(which = -2))[[p]]) == 0){
        schur <- schurmat_sblk(blk,At,par,schur,p,par$W)
      }else if(is.character(get("schurfun", pos=sys.frame(which = -2))[[p]])){
        schurtmp <- Matrix(0,m,m,sparse=TRUE)
        if(length(par$permZ[[p]]) > 0){
          Wp <- par$W[[p]][par$permZ[[p]], par$permZ[[p]]]
        }else{
          Wp <- par$W[[p]]
        }
        schurfun_input <- get("schurfun", pos=sys.frame(which = -2))
        schurfun_tmp <- match.fun(get(schurfun_input[[p]], pos=sys.frame(which = -2)))
        schurtmp <- schurfun_tmp(Wp,Wp,get("schurfun_par", pos=sys.frame(which = -2))[p,])
        schur <- schur + schurtmp
      }
    }else if(blk[[p,1]] == "u"){
      Afree <- cbind(Afree,t(At[[p]]))
    }
  }
  ##
  ## compute rhs
  ##
  out <- NTrhsfun(blk,At,par,X,Z,rp,Rd,sigmu)
  rhs <- out$rhs
  EinvRc <- out$EinvRc
  hRd <- out$hRd
  ##
  ## solve linear system
  ##
  out <- linsysolve(par, schur, UU, Afree, EE, rhs)
  xx <- out$xx
  coeff <- out$coeff
  L <- out$L
  ##
  ## compute dX & dZ
  ##
  out <- NTdirfun(blk,At,par,Rd,EinvRc,xx,m)
  dX <- out$dX
  dy <- out$dy
  dZ <- out$dZ
  
  return(list(par=par,dX=dX,dy=dy,dZ=dZ,coeff=coeff,L=L,hRd=hRd))
}