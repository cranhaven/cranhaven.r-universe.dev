HKMcorr <- function(blk,At,par,rp,Rd,sigmu,hRd,dX,dZ,coeff,L,X,Z){
  
  out <- HKMrhsfun(blk,At,par,X,Z,rp,Rd,sigmu,hRd,dX,dZ)
  rhs <- out$rhs
  EinvRc <- out$EinvRc
  
  m <- length(rp)
  ncolU <- ncol(coeff$mat12)
  if(is.null(ncolU)){
    ncolU <- 0
  }
  
  rhs <- rbind(rhs,matrix(0,m+ncolU-length(rhs),1))
  
  ##
  assign("solve_ok",1, pos=sys.frame(which = -2))
  resnrm <- base::norm(rhs,type="2")
  if(get("matfct_options",pos=sys.frame(which = -2)) == "chol" | 
     get("matfct_options",pos=sys.frame(which = -2)) == "spchol" | 
     get("matfct_options",pos=sys.frame(which = -2)) == "ldl" | 
     get("matfct_options",pos=sys.frame(which = -2)) == "spldl"){
    out <- symqmr(coeff,rhs,L,c(),c())
    xx <- as.matrix(out$xx)
    resnrm <- out$resnrm
    assign("solve_ok",out$solve_ok, pos=sys.frame(which = -2))
    if(get("solve_ok", pos=sys.frame(which = -2)) <= 0){
      warning("symqmr fails")
    }
  }else{
    out <- mybicgstab(coeff,rhs,L,c(),c())
    xx <- as.matrix(out$xx)
    resnrm <- out$resnrm
    print(length(resnrm)-1)
    assign("solve_ok",out$flag, pos=sys.frame(which = -2))
    if(get("solve_ok", pos=sys.frame(which = -2)) <= 0){
      warning("mybicgstab fails")
    }
  }
  out <- HKMdirfun(blk,At,par,Rd,EinvRc,X,xx,m)
  dX <- out$dX
  dy <- out$dy
  dZ <- out$dZ
  
  return(list(dX = dX, dy=dy, dZ=dZ, resnrm=resnrm,EinvRc = EinvRc))
}
