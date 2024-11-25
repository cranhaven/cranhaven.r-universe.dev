sqlp_base <- function(blk=NULL, At=NULL, C=NULL, b=NULL, OPTIONS=NULL, X0=NULL, y0=NULL, Z0=NULL){
  
  if((is.null(blk) | is.null(At) | is.null(C) | is.null(b))){
    stop("Error: Improper input methods")
  }
  
  b <- as.matrix(b)
  
  ##########################################
  ######## Define Local Variables  #########
  ##########################################
  
  #Replace .GlobalEnv with sys.frame(which = ) for each

  idxdenAl <- numeric(0)
  idxdenAq <- numeric(0)
  nnzschur_qblk <- numeric(0)
  nnzschur <- numeric(0)
  nzlistschur <- numeric(0)
  schurfun <- numeric(0)
  schurfun_par <- numeric(0)
  
  diagR <- numeric(0)
  diagRold <- numeric(0)
  exist_analytic_term <- numeric(0)
  existlowrank <- numeric(0)
  matfct_options <- numeric(0)
  matfct_options_old <- numeric(0)
  nnzmat <- numeric(0)
  nnzmatold <- numeric(0)
  numpertdiashschur <- numeric(0)
  printlevel <- numeric(0)
  smallblkdim <- numeric(0)
  solve_ok <- numeric(0)
  spdensity <- numeric(0)
  use_LU <- numeric(0)
  
  ##################################
  
  isemptyAtb <- 0
  if(is.null(At) & is.null(b)){
    #Add Redundant Constraint
    b <- 0
    At <- ops(ops(blk, "identity"), "*", -1)
    numblk <- nrow(blk)
    blk[[numblk+1, 1]] <- "l"
    blk[[numblk+1, 2]] <- 1
    At[[numblk+1,1]] <- 1
    C[[numblk+1,1]] <- 0
    isemptyAtb <- 1
  }
  
  #Set default parameters from sqlparameters (OPTIONS input not used)
  
  par <- list(vers = 0,
              gam = 0,
              predcorr = 1,
              expon = 1,
              gaptol = 1e-8,
              inftol = 1e-8,
              steptol = 1e-6,
              maxit = 100,
              printlevel = 3,
              stoplevel = 1,
              scale_data = 0,
              spdensity = 0.4,
              rmdepconstr = 0,
              smallblkdim = 50,
              parbarrier = c(),
              schurfun = matrix(list(),nrow=nrow(blk),ncol=1),
              schurfun_par = matrix(list(),nrow=nrow(blk),ncol=1),
              blkdim = c(),
              ublksize = c(),
              depconstr = c(),
              AAt = c(),
              normAAt = c(),
              numcolAt = c(),
              permA = c(),
              permZ = c(),
              isspA = c(),
              nzlist = c(),
              nzlistAsum = c(),
              isspAy = c(),
              nzlistAy = c(),
              iter = c(),
              obj = c(),
              relgap = c(),
              pinfeas = c(),
              dinfeas = c(),
              rp = c(),
              y = c(),
              dy = c(),
              normX = c(),
              ZpATynorm = c())
  
  ##
  parbarrier <- matrix(list(),nrow=nrow(blk),ncol=1)
  for(p in 1:nrow(blk)){
    pblk <- blk[[p,1]]
    if(pblk == "s" | pblk == "q"){
      parbarrier[[p,1]] <- matrix(0, nrow=1, ncol=length(blk[[p,2]]))
    }else if(pblk == "l" | pblk == "u"){
      parbarrier[[p,1]] <- matrix(0, nrow=1, ncol=sum(blk[[p,2]]))
    }
  }
  parbarrier_0 <- parbarrier
  
  if(!is.null(OPTIONS) | length(OPTIONS) > 0){
    if(!is.null(OPTIONS$vers)) par$vers <- OPTIONS$vers
    if(!is.null(OPTIONS$predcorr)) par$predcorr <- OPTIONS$predcorr
    if(!is.null(OPTIONS$gam)) par$gam <- OPTIONS$gam
    if(!is.null(OPTIONS$expon)) par$expon <- OPTIONS$expon
    if(!is.null(OPTIONS$gaptol)) par$gaptol <- OPTIONS$gaptol
    if(!is.null(OPTIONS$inftol)) par$inftol <- OPTIONS$inftol
    if(!is.null(OPTIONS$steptol)) par$steptol <- OPTIONS$steptol
    if(!is.null(OPTIONS$maxit)) par$maxit <- OPTIONS$maxit
    if(!is.null(OPTIONS$printlevel)) par$printlevel <- OPTIONS$printlevel
    if(!is.null(OPTIONS$stoplevel)) par$stoplevel <- OPTIONS$stoplevel
    if(!is.null(OPTIONS$scale_data)) par$scale_data <- OPTIONS$scale_data
    if(!is.null(OPTIONS$spdensity)) par$spedensity <- OPTIONS$spdensity
    if(!is.null(OPTIONS$rmdepconstr)) par$rmdepconstr <- OPTIONS$rmdepconstr
    if(!is.null(OPTIONS$smallblkdim)) par$smallblkdim <- OPTIONS$smallblkdim
    if(!is.null(OPTIONS$parbarrier)){
      parbarrier <- OPTIONS$parbarrier
      if(is.null(parbarrier)) parbarrier <- parbarrier_0
      if(!is.list(parbarrier)){
        tmp <- parbarrier
        parbarrier <- matrix(list(),1,1)
        parbarrier[[1]] <- tmp
      }
      if(max(dim(as.matrix(parbarrier))) < nrow(blk)){
        len <- max(dim(as.matrix(parbarrier)))
        parbarrier_1 <- matrix(list(),nrow(blk),1)
        for(i in 1:len){
          parbarrier_1[[i]] <- parbarrier[[i]]
        }
        for(i in (len+1):nrow(blk)){
          parbarrier_1[[i]] <- parbarrier_0[[i]]
        }
        parbarrier <- parbarrier_1
      }
    }
  }
  
  if(ncol(blk) > 2){
    par$smallblkdim <- 0
  }
  
  ######################
  ##Validate SQLP data##
  ######################
  
  out <- validate(blk,At,C,b,par,parbarrier)
  blk <- out$blk
  At <- out$At
  C <- out$C
  b <- out$b
  blkdim <- out$dim
  numblk <- out$nnblk
  parbarrier <- out$parbarrier
  
  out <- convertcmpsdp(blk, At, C, b)
  blk <- out$bblk
  At <- out$AAt
  C <- out$CC
  b <- out$bb
  iscmp <- out$iscmp
  
  if(is.null(X0) | is.null(y0) | is.null(Z0)){
  #create a starting point
  out <- infeaspt(blk, At, C, b)
  X0 <- out$X0
  y0 <- out$y0
  Z0 <- out$Z0
  par$startpoint <- 1
  }else{
    par$startpoint <- 2
    out <- validate_startpoint(blk, X0,Z0,par$spdensity,iscmp)
    X0 <- out$X
    Z0 <- out$Z
  }
  
  ##############################
  ##DETECT UNRESTRICTED BLOCKS##
  ##############################
  
  user_supplied_schurfun <- 0
  
  for(p in 1:nrow(blk)){
    if(!is.null(par$schurfun[[p]])){
      user_supplied_schurfun <- 1
    }
  }
  
  if(user_supplied_schurfun == 0){
    out <- detect_ublk(blk,At,C,parbarrier,X0,Z0)
    blk2 <- out$blk2
    At2 <- out$At2
    C2 <- out$C2
    ublkinfo <- out$ublkinfo
    parbarrier2 <- out$parbarrier2
    X02 <- out$X2
    Z02 <- out$Z2
  }else{
    blk2 <- blk
    At2 <- At
    C2 <- C
    parbarrier2 <- parbarrier
    X02 <- X0
    Z02 <- Z0
    ublkinfo <- matrix(list(), nrow(blk3), 1)
  }
  
  ublksize <- blkdim[4]
  for(p in 1:nrow(ublkinfo)){
    if(!is.null(ublkinfo[[p,1]])){
      ublksize <- ublksize + max(dim(ublkinfo[[p,1]]))
    }
  }
  
  ################################
  #####Detect diagonal blocks#####
  ################################
  
  if(user_supplied_schurfun == 0){
    out <- detect_lblk(blk2,At2,C2,b,parbarrier2,X02,Z02)
    blk3 <- as.matrix(out$blk)
    At3 <- as.matrix(out$At)
    C3 <- as.matrix(out$C)
    diagblkinfo <- out$diagblkinfo
    diagblkchange <- out$blockchange
    parbarrier3 <- as.matrix(out$parbarrier)
    X03 <- as.matrix(out$X)
    Z03 <- as.matrix(out$Z)
  }else{
    blk3 <- blk2
    At3 <- At2
    C3 <- C2
    parbarrier3 <- parbarrier2
    X03 <- X02
    Z03 <- Z02
    diagblkchange <- 0
    diagblkinfo <- matrix(list(), nrow(blk3), 1)
  }
  
  #################################
  ######### MAIN SOLVER ###########
  #################################
  
  exist_analytic_term <- 0
  for(p in 1:nrow(blk3)){
    idx <- which(parbarrier3[[p,1]] > 0)
    if(length(idx) > 0){
      exist_analytic_term <- 1
    }
  }
  
  if(par$vers == 0){
    if(blkdim[1]){
      par$vers <- 1
    }else{
      par$vers <- 2
    }
  }
  
  par$blkdim <- blkdim
  par$ublksize <- ublksize
  
  out <- sqlp_main(blk3, At3, C3, b, par, parbarrier3, X03, y0, Z03)
  
  obj <- out$obj
  X3 <- out$X
  y <- out$y
  Z3 <- out$Z
  info <- out$info
  runhist <- out$runhist
  pobj <- info$obj[1]
  dobj <- info$obj[2]
  
  ################################################
  #Recover Semidefinite Blocks from Linear Blocks#
  ################################################
  
  if(any(diagblkchange == 1)){
    X2 <- matrix(list(),nrow(blk),1)
    Z2 <- matrix(list(),nrow(blk),1)
    count <- 0
    
    for(p in 1:nrow(blk)){
      n <- sum(blk[[p,2]])
      blkno <- diagblkinfo[[p,1]]
      idxdiag <- diagblkinfo[[p,2]]
      idxnondiag <- diagblkinfo[[p,3]]
      
      if(length(idxdiag) > 0){
        len <- length(idxdiag)
        Xtmp <- rbind(cbind(idxdiag,idxdiag,X3[[nrow(X3)]][count+c(1:len)]))
        Ztmp <- rbind(cbind(idxdiag,idxdiag,Z3[[nrow(Z3)]][count+c(1:len)]))
        if(length(idxnondiag) > 0){
          tmp <- which(X3[[blkno]] != 0, arr.ind=TRUE)
          ii <- tmp[,1]
          jj <- tmp[,2]
          vv <- X3[[blkno]][ which(X3[[blkno]] != 0)]
          Xtmp <- rbind(Xtmp,cbind(idxnondiag[ii],idxnondiag[jj],vv))
          
          tmp <- which(Z3[[blkno]] != 0, arr.ind=TRUE)
          ii <- tmp[,1]
          jj <- tmp[,2]
          vv <- Z3[[blkno]][ which(Z3[[blkno]] != 0)]
          Ztmp <- rbind(Ztmp,cbind(idxnondiag[ii],idxnondiag[jj],vv))
        }
        X2[[p]] <- matrix(0,n,n)
        for(i in 1:nrow(Xtmp)){
          X2[[p]][Xtmp[i,1],Xtmp[i,2]] <- Xtmp[i,3]
        }
        
        Z2[[p]] <- matrix(0,n,n)
        for(i in 1:nrow(Ztmp)){
          Z2[[p]][Ztmp[i,1],Ztmp[i,2]] <- Ztmp[i,3]
        }
        count <- count + len
      }else{
        X2[[p]] <- X3[[blkno]]
        Z2[[p]] <- Z3[[blkno]]
      }
    }
  }else{
    X2 <- X3
    Z2 <- Z3
  }
  
  ################################################
  # Recover linear block from unrestricted block #
  ################################################
  
  numblk <- nrow(blk)
  numblknew <- numblk
  X <- matrix(list(),numblk,1)
  Z <- matrix(list(),numblk,1)
  
  for(p in 1:numblk){
    n <- blk[[p,2]]
    if(is.null(ublkinfo[[p,1]])){
      X[[p]] <- X2[[p]]
      Z[[p]] <- Z2[[p]]
    }else{
      Xtmp <- matrix(0,n,1)
      Ztmp <- matrix(0,n,1)
      
      Xtmp[ublkinfo[[p,1]]] <- pmax(0,X2[[p]])
      Xtmp[ublkinfo[[p,2]]] <- pmax(0,-X2[[p]])
      Ztmp[ublkinfo[[p,1]]] <- pmax(0,Z2[[p]])
      Ztmp[ublkinfo[[p,2]]] <- pmax(0,-Z2[[p]])
      
      if(!is.null(ublkinfo[[p,3]])){
        numblknew <- numblknew + 1
        Xtmp[ublkinfo[[p,3]]] <- X2[[numblknew]]
        Ztmp[ublkinfo[[p,3]]] <- Z2[[numblknew]]
      }
      X[[p]] <- Xtmp
      Z[[p]] <- Ztmp
    }
  }
  
  output <- list(X=X, y=y, Z=Z, pobj=pobj, dobj=dobj)
  
  return(output)
  
}