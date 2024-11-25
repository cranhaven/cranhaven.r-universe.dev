sqlpmisc <- function(blk,At,C,b,X,y,Z,permZ,param){
  
  termcode    <- param$termcode
  iter        <- param$iter 
  obj         <- param$obj
  relgap      <- param$relgap 
  prim_infeas <- param$prim_infeas
  dual_infeas <- param$dual_infeas
  homRd       <- param$homRd 
  homrp       <- param$homrp 
  AX          <- param$AX
  ZpATynorm   <- param$ZpATynorm 
  m0          <- param$m0    
  indeprows   <- param$indeprows
  normX0      <- param$normX0
  normZ0      <- param$normZ0
  inftol      <- param$inftol
  maxit       <- param$maxit 
  scale_data  <- param$scale_data 
  printlevel  <- param$printlevel
  ##
  resid <- c()
  reldist <- c()
  msg <- c()
  if(scale_data){
    #Do nothing, data is not scaled
  }else{
    normA <- 1
    normC <- 1
    normb <- 1
  }
  
  Anorm <- ops(At, "norm")
  xnorm <- ops(X, "norm")
  ynorm <- base::norm(y, type="2")
  infeas <- max(prim_infeas, dual_infeas)
  ##
  if(iter >= maxit){
    termcode <- -6
    msg <- "max number of iterations reached"
  }
  if(termcode <= 0){
    ##
    ## detect near infeasibility
    ##
    err <- max(infeas, relgap)
    iflag <- 0
    if(obj[2] > 0){
      if(homRd < 0.1*sqrt(err*inftol)){
        iflag <- 1
        termcode <- 1
      }
    }
    if(obj[1] < 0){
      if(homrp < 0.1*sqrt(err*inftol)){
        iflag <- 1
        termcode <- 2
      }
    }
    if(iflag == 0){
      if(scale_data == 1){
        #Do nothing, data is not scaled
      }
    }
  }
  
  if(termcode == 1 & iter > 3){
    rby <- 1/(t(b) %*% y)
    y <- rby * y
    Z <- ops(Z,"*",rby)
    resid <- ZpATynorm * rby
    reldist <- ZpATynorm/(Anorm*ynorm)
  }
  if(termcode == 2 & iter > 3){
    if(scale_data == 1){
      #Do nothing, data is not scaled
    }
    tCX <- blktrace(blk,C,X)
    X <- ops(X,"*", 1/(-tCX))
    resid <- base::norm(AX,type="2")/(-tCX)
    reldist <- base::norm(AX,type="2")/(Anorm*xnorm)
  }
  if(termcode == 3){
    maxblowup <- max(ops(X,"norm")/normX0, ops(Z,"norm")/normZ0)
  }
  out <- unperm(blk,permZ,X,Z)
  X <- out$X
  Z <- out$Z
  
  if(length(indeprows) > 0){
    ytmp <- matrix(0,m0,1)
    ytmp[indeprows] <- y
    y <- ytmp
  }
  return(list(X=X, y=y, Z=Z, termcode=termcode, resid=resid,reldist = reldist, msg=msg))
}