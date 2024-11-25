sqlp_main <- function(blk, At, C, b, par, parbarrier, X0, y0, Z0){

  ##parameters
  vers <- par$vers
  predcorr <- par$predcorr
  gam <- par$gam
  expon <- par$expon
  gaptol <- par$gaptol
  inftol <- par$inftol
  steptol <- par$steptol
  maxit <- par$maxit
  stoplevel <- par$stoplevel
  scale_data <- par$scale_data
  
  rmdepconstr <- par$rmdepconstr
  ublksize <- par$ublksize
  
  assign("spdensity",par$spdensity, pos=sys.frame(which = -1))
  assign("smallblkdim",par$smallblkdim, pos=sys.frame(which = -1))
  assign("schurfun",par$schurfun, pos=sys.frame(which = -1))
  assign("schurfun_par",par$schurfun_par, pos=sys.frame(which = -1))
  assign("printlevel",par$printlevel, pos=sys.frame(which = -1))
  
  X <- X0
  y <- y0
  Z <- Z0
  for(p in 1:nrow(blk)){
    if(blk[[p,1]] == "u"){
      Z[[p,1]] <- matrix(0, nrow=blk[[p,2]], ncol=1)
    }
  }
  
  ################################################
  #### Convert unrestricted blk to linear blk ####
  ################################################
  
  convertlen <- 0
  out <- sqlpu2lblk(blk,At,C,X,Z,par,convertlen)
  blk <- out$blk
  At <- out$At
  C <- out$C
  X <- out$X
  Z <- out$Z
  u2lblk <- out$u2lblk
  ublkidx <- out$ublkidx
  
  for(p in 1:nrow(blk)){
    if(u2lblk[p] == 1){ 
      n <- 2*blk[[p,2]]
      blk[[p,1]] <- "l"
      blk[[p,2]] <- n
      parbarrier[[p,1]] <- matrix(0, nrow=1, ncol=n)
      At[[p,1]] <- rbind(At[[p,1]], -At[[p,1]])
      tau <- max(1, base::norm(C[[p,1]], type="2"))
      C[[p,1]] <- rbind(C[[p,1]], -C[[p,1]])
      
      b2 <- 1 + abs(t(b))
      normCtmp <- 1 + base::norm(C[[p,1]], type="2")
      normAtmp <- 1 + sqrt(colSums(At[[p,1]] * At[[p,1]]))
      
      if(n > 1000){
        const <- sqrt(n)
      }else{
        const <- n
      }
      if(par$startpoint == 1){
        X[[p,1]] <- const * max(c(1, b2/normAtmp)) * matrix(1,n,1)
        Z[[p,1]] <- const * max(c(1, normAtmp/sqrt(n), normCtmp/sqrt(n))) * matrix(1,n,1)
        X[[p,1]] <- X[[p,1]] * (1+1e-10 * randmat(n,1,0,"u"))
        Z[[p,1]] <- Z[[p,1]] * (1+1e-10 * randmat(n,1,0,"u"))
      }else{
        const <- max(abs(X[[p,1]])) + 100
        X[[p,1]] <- rbind(X[[p,1]] + const, const*rep(1,n/2))
        Z[[p,1]] <- rbind(abs(Z0[[p,1]]), abs(Z0[[p,1]])) + 1e-4
      }
    }
  }
  
  #######################################################
  #check Whether {A1, ..., Am} are linearly independent#
  #######################################################
  
  m0 <- length(b)
  out <- checkdepconstr(blk,At,b,y,rmdepconstr)
  At <- out$At
  b <- out$b
  y <- out$y
  indeprows <- out$indeprows
  par$depconstr <- out$depconstr
  feasible <- out$feasible
  par$AAt <- out$AAt
  
  if(!feasible){
    stop("SQLP is not feasible")
  }
  
  par$normAAt <- Matrix::norm(par$AAt, type="F")
  
  ####################################################
  ############### SCALE SQLP DATA ####################
  ####################################################
  
  normA2 <- 1 + ops(At, "norm")
  normb2 <- 1 + base::norm(b, type="2")
  normC2 <- 1 + ops(C, "norm")
  normX0 <- 1 + ops(X0, "norm")
  normZ0 <- 1 + ops(Z0, "norm")
  
  if(scale_data){
    out <- scaling(blk,At,C,b,X,y,Z)
    At <- out$At
    C <- out$C
    b <- out$b
    normA <- out$normA
    normC <- out$normC
    normb <- out$normb
    X <- out$X0
    y <- out$y0
    Z <- out$Z0
  }else{
    normA <- 1
    normC <- 1
    normb <- 1
  }
  
  ###################################
  ##### Find combined list of #######
  ##### non-zero elements in Aj #####
  ###################################
  par$numcolAt <- length(b)
  out <- sortA(blk, At, C, b, X, Z)
  At <- out$At
  C <- out$C
  X <- out$X
  Z <- out$Z
  par$permA <- out$permA
  par$permZ <- out$permZ
  
  out <- nzlist(blk,At,par)
  par$isspA <- out$isspA
  par$nzlistA <- out$nzlistA
  par$nzlistAsum <- out$nzlistAsum
  par$isspAy <- out$isspAy
  par$nzlistAy <- out$nzlistAy
  
  ##########################################
  ## Create artificial non-negative block ##
  ##########################################
  
  numblkold <- nrow(blk)
  nn <- 0
  for(p in 1:nrow(blk)){
    idx <- which(parbarrier[[p,1]] == 0)
    if(length(idx) > 0){
      if(blk[[p,1]] == "l"){
        nn <- nn + length(idx)
      }else if(blk[[p,1]] == "q"){
        nn <- nn + sum(blk[[p,2]][idx])
      }else if(blk[[p,1]] == "s"){
        nn <- nn + sum(blk[[p,2]][idx])
      }
    }
  }
  if(nn == 0){
    analytic_prob <- 1
    numblk <- nrow(blk) + 1
    bkl[[numblk,1]] <- "l"
    blk[[numblk,2]] <- 1
    At[[numblk,1]] <- matrix(0, 1, length(b))
    C[[numblk,1]] <- 1
    X[[numblk,1]] <- 1e3
    Z[[numblk,1]] <- 1e3
    parbarrier[[numblk,1]] <- 0
    u2lblk[numblk,1] <- 0
    nn <- nn + 1
  }else{
    analytic_prob <- 0
  }
  ##
  assign("exist_analytic_term",0, pos=sys.frame(which = -1))
  for(p in 1:nrow(blk)){
    idx <- which(parbarrier[[p,1]] > 0)
    if(length(idx) > 0){
      assign("exist_analytic_term",1, pos=sys.frame(which = -1))
    }
  }
  
  ########################################
  ######## INITIALIZATION ################
  ########################################
  
  EE <- ops(blk, "identity")
  normE2 <- ops(EE, "norm")
  Zpertold <- 1
  
  normCC <- rep(0, nrow(blk))
  normEE <- rep(0, nrow(blk))
  
  for(p in 1:nrow(blk)){
    normCC[p] <- 1 + ops(C[[p,1]], "norm")
    normEE[p] <- 1 + ops(EE[[p,1]], "norm")
  }
  
  indef <- c(0,0)
  
  out <- blkcholfun(blk, X=X)
  Xchol <- out$Xchol
  indef[1] <- out$indef
  
  out <- blkcholfun(blk, X=Z)
  Zchol <- out$Xchol
  indef[2] <- out$indef
  
  if(any(indef == 1)){
    stop("X or Z not positive definite")
  }

  AX <- AXfun(blk, At, par$permA,X)
  rp <- b - AX
  ZpATy <- ops(Z, "+", Atyfun(blk,At, par$permA,par$isspAy,y))
  ZpATynorm <- ops(ZpATy, "norm")
  Rd <- ops(C, "-", ZpATy)
  objadd0 <- 0
  
  if(scale_data){
    for(p in 1:nrow(blk)){
      objadd0 <- objadd0 + sum(parbarrier[[p]] * blk[[p,2]]*log(normA[[p]]))
    }
  }
  
  objadd <- blkbarrier(blk, X, Z, Xchol, Zchol, parbarrier) + objadd0
  obj <- (normb*normC)*c(blktrace(blk,C,X), as.matrix(t(b) %*% y)) + objadd
  gap <- (normb*normC)*blktrace(blk,X,Z) - diff(objadd)
  relgap <- gap/(1+sum(abs(obj)))
  prim_infeas <- base::norm(rp, type="2")/normb2
  dual_infeas <- ops(Rd,"norm")/normC2
  infeas <- max(prim_infeas, dual_infeas)
  
  infease_org <- c(0,0)
  if(scale_data){
    infeas_org[1] <- prim_infeas*normb
    infeas_org[2] <- dual_infeas*normC
  }
  
  trXZ <- blktrace(blk,X,Z,parbarrier)
  if(nn > 0){
    mu <- trXZ/nn
  }else{
    mu <- gap/ops(X, "getM")
  }
  normX <- ops(X, "norm")
  
  ##
  termcode <- 0
  restart <- 0
  pstep <- 1
  dstep <- 1
  pred_convg_rate <- 1
  corr_convg_rate <- 1
  prim_infeas_min <- prim_infeas
  dual_infeas_min <- dual_infeas
  prim_infeas_best <- prim_infeas
  dual_infeas_best <- dual_infeas
  infeas_best <- infeas
  relgap_best <- relgap
  homRd <- Inf
  homrp <- Inf
  dy <- matrix(0, nrow=length(b), ncol=1)
  
  runhist <- list(pobj = obj[1],
                  dobj = obj[2],
                  gap = gap,
                  relgap = relgap,
                  pinfeas = prim_infeas,
                  dinfeas = dual_infeas,
                  infeas = infeas,
                  pstep = 0,
                  dstep = 0,
                  step = 0,
                  normX = normX)
  
  ##########################################
  ############ MAIN LOOP ###################
  ##########################################
  
  param <- list(termcode = termcode,
                iter = 0,
                obj = obj,
                relgap = relgap, 
                prim_infeas = prim_infeas,
                dual_infeas = dual_infeas,
                homRd = homRd,
                homrp = homrp,
                AX = AX,
                ZpATynorm = ZpATynorm,
                normA = normA,
                normb = normb,
                normC = normC,
                normX0 = normX0,
                normZ0 = normZ0,
                m0 = m0,
                indeprows = indeprows,
                prim_infeas_bad = 0,
                dual_infeas_bad = 0,
                prim_infeas_min = prim_infeas,
                dual_infeas_min = dual_infeas,
                gaptol = gaptol,
                inftol = inftol,
                maxit = maxit,
                scale_data = scale_data,
                printlevel = par$printlevel,
                ublksize = ublksize)
  
  Xbest <- X
  ybest <- y
  Zbest <- Z
  
  ##
  mupredhist <- rep(0, maxit)
  update_best <- 0

  for(iter in 1:maxit){
    #print(c(iter-1, runhist$pstep[iter], runhist$dstep[iter], runhist$pinfeas[iter], runhist$dinfeas[iter], runhist$gap[iter], runhist$pobj[iter], runhist$dobj[iter]))
    
    update_iter <- 0
    breakyes <- 0
    pred_slow <- 0
    corr_slow <- 0
    step_short <- 0
    
    par$parbarrier <- parbarrier
    par$iter <- iter
    par$obj <- obj
    par$relgap <- relgap
    par$pinfeas <- prim_infeas
    par$dinfeas <- dual_infeas
    par$rp <- rp
    par$y <- y
    par$dy <- dy
    par$normX <- normX
    par$ZpATynorm <- ZpATynorm
    
    if(iter == 1 | restart){
      Cpert <- min(1, normC2/ops(EE, "norm"))
    }
    
    if(runhist$dinfeas[1] > 1e-3 & !get("exist_analytic_term", pos=sys.frame(which = -1)) & relgap > 1e-4){
      if(par$normX > 5e3 & iter < 20){
        Cpert <- Cpert*0.5
      }else if(par$normX > 5e2 & iter < 20){
        Cpert <- Cpert * 0.3
      }else{
        Cpert <- Cpert * 0.1
      }
      Rd <- ops(Rd, "+", EE, Cpert)
    }
    
    ###########################################
    ######### PREDICTOR STEP ##################
    ###########################################
    #print("Predictor Step")
    if(predcorr){
      sigma <- 0
    }else{
      sigma <- 1 - 0.9*(min(pstep, dstep))
      if(iter == 1){
        sigma = 0.5
      }
    }
    sigmu <- matrix(list(), nrow=nrow(blk), ncol=1)
    for(p in 1:nrow(blk)){
      sigmu[[p,1]] <- pmax(sigma*mu, t(parbarrier[[p,1]]))
    }
    invXchol <- matrix(list(), nrow=nrow(blk), ncol=1)
    invZchol <- ops(Zchol, "inv")
    if(vers == 1){
      out <- HKMpred(blk, At, par, rp, Rd, sigmu, X, Z, invZchol)
      par <- out$par
      dX <- out$dX
      dy <- out$dy
      dZ <- out$dZ
      coeff <- out$coeff
      L <- out$L
      hRd <- out$hRd
      
      if(any(is.null(dX))){
        break
      }
      
    }else if(vers == 2){
      out <- NTpred(blk, At, par, rp, Rd, sigmu, X, Z,Zchol, invZchol)
      par <- out$par
      dX <- out$dX
      dy <- out$dy
      dZ <- out$dZ
      coeff <- out$coeff
      L <- out$L
      hRd <- out$hRd
      
      if(any(is.null(dX))){
        break
      }
    }
    
    if(get("solve_ok", pos=sys.frame(which = -1)) <= 0){
      termcode <- -4
    }
    
    #####################################
    ## STEP LENGTHS FOR PREDICTOR STEP ##
    #####################################
    #print("Step Length for Predictor Step")
    if(gam == 0){
      gamused <- 0.9 + .09*min(pstep, dstep)
    }else{
      gamused <- gam
    }
    
    out <- steplength(blk, X, dX, Xchol, invXchol)
    Xstep <- out$xstep
    invXchol <- out$invXchol
    pstep <- min(1, gamused*Xstep)
    
    Zstep <- steplength(blk,Z,dZ,Zchol,invZchol)$xstep
    dstep <- min(1, gamused*Zstep)
    trXZnew <- trXZ + pstep*blktrace(blk,dX,Z,parbarrier) + dstep*blktrace(blk,X,dZ,parbarrier) + pstep*dstep*blktrace(blk,dX,dZ,parbarrier)
    if(nn > 0){
      mupred <- trXZnew/nn
    }else{
      mupred <- 1e-16
    }
    
    mupredhist[iter] <- mupred
    
    ##########################################
    ## STOPPING CRITERIA FOR PREDICTOR STEP ##
    ##########################################
    #print("Stopping Criteria for Predictor Step")
    if(min(pstep,dstep) < steptol & stoplevel & iter > 10){
      termcode <- -2
      breakyes <- 1
    }
    
    if(!predcorr){
      if(iter >= 2){
        idx <- max(2,iter-2):iter
        pred_slow <- all(mupredhist[idx] / mupredhist[idx-1] > 0.4)
        idx <- max(2,iter-5):iter
        pred_conv_rate <- mean(mupredhist[idx] / mupredhist[idx-1])
        pred_slow <- pred_slow + (mupred/mu > 5*pred_convg_rate)
      }
      if(max(mu,infeas) < 1e-6 & pred_slow & stoplevel){
        termcode <- -2
        breakyes <- 1
      }else{
        update_iter <- 1
      }
    }
    
    #######################################
    ########## CORRECTOR STEP #############
    #######################################
    #print("Corrector Step")
    if(predcorr & !breakyes){
      
      step_pred <- min(pstep,dstep)
      if(mu > 1e-6){
        if(step_pred < 1/sqrt(3)){
          expon_used <- 1
        }else{
          expon_used <- max(expon, 3*step_pred^2)
        }
      }else{
        expon_used <- max(1, min(expon, 3*step_pred^2))
      }
      
      if(nn == 0){
        sigma = 0.2
      }else if(mupred < 0){
        sigma = 0.8
      }else{
        sigma <- min(1, (mupred/mu)^expon_used)
      }
      
      sigmu <- matrix(list(), nrow=nrow(blk), ncol=1)
      for(p in 1:nrow(blk)){
        sigmu[[p,1]] <- pmax(sigma*mu, t(parbarrier[[p,1]]))
      }
      if(vers == 1){
        out <- HKMcorr(blk,At,par,rp,Rd,sigmu,hRd,dX,dZ,coeff,L,X,Z)
        dX <- out$dX
        dy <- out$dy
        dZ <- out$dZ
      }else if(vers == 2){
        out <- NTcorr(blk,At,par,rp,Rd,sigmu,hRd,dX,dZ,coeff,L,X,Z)
        dX <- out$dX
        dy <- out$dy
        dZ <- out$dZ
      }
      if(get("solve_ok", pos=sys.frame(which = -1)) <= 0){
        runhist$pinfeas[iter + 1] <- runhist$pinfeas[iter]
        runhist$dinfeas[iter + 1] <- runhist$dinfeas[iter]
        runhist$relgap[iter + 1] <- runhist$relgap[iter]
        termcode <- -4
        break
      }
      
      ########################################
      #### STEP LENGTH FOR CORRECTOR STEP ####
      ########################################
      #print("Step Length for Corrector Step")
      if(gam == 0){
        gamused <- 0.9 + .09*min(pstep, dstep)
      }else{
        gamused <- gam
      }
      
      out <- steplength(blk, X, dX, Xchol, invXchol)
      Xstep <- out$xstep
      pstep <- min(1, gamused*Xstep)
      
      out <- steplength(blk,Z,dZ,Zchol,invZchol)
      Zstep <- out$xstep
      dstep <- min(1, gamused*Zstep)
      trXZnew <- trXZ + pstep*blktrace(blk,dX,Z,parbarrier) + dstep*blktrace(blk,X,dZ,parbarrier) + pstep*dstep*blktrace(blk,dX,dZ,parbarrier)
      if(nn > 0){
        mucorr <- trXZnew/nn
      }else{
        mucorr <- 1e-16
      }
      
      ################################################
      ##### STOPPING CRITERIA FOR CORRECTOR STEP #####
      ################################################
      #print("Stopping Criteria for Corrector Step")
      if(iter >= 2){
        idx <- max(2,iter-2):iter
        corr_slow <- all(runhist$gap[idx] / runhist$gap[idx-1] > 0.8)
        idx <- max(2,iter-5):iter
        corr_convg_rate <- mean(runhist$gap[idx] / runhist$gap[idx-1])
        corr_slow <- corr_slow + (mucorr/mu > max(min(1,5*corr_convg_rate),0.8))
      }
      
      if(max(relgap, infeas) < 1e-6 & iter > 20 & corr_slow > 1 & stoplevel){
        termcode <- -2
        breakyes <- 1
      }else{
        update_iter <- 1
      }
    }
    
    ######################################
    ######## UPDATE ITERATE ##############
    ######################################
    #print("Update Iterate")
    indef <- c(1,1)
    if(update_iter){
      
      dXtmp <- dX
      for(i in 1:length(dXtmp)){
        dXtmp[[i]] <- as.matrix(dXtmp[[i]])
      }
      
      Xtmp <- X
      for(i in 1:length(Xtmp)){
        Xtmp[[i]] <- as.matrix(Xtmp[[i]])
      }
      
      for(t in 1:5){
        out <- blkcholfun(blk,X=ops(Xtmp,"+",dXtmp,pstep))
        Xchol <- out$Xchol
        indef[1] <- out$indef
        if(indef[1]){
          pstep <- 0.8*pstep
        }else{
          break
        }
      }
      if(t > 1){
        pstep <- gamused*pstep
      }
      
      dZtmp <- dZ
      for(i in 1:length(dZtmp)){
        dZtmp[[i]] <- as.matrix(dZtmp[[i]])
      }
      
      Ztmp <- Z
      for(i in 1:length(Ztmp)){
        Ztmp[[i]] <- as.matrix(Ztmp[[i]])
      }
      
      for(t in 1:5){
        out <- blkcholfun(blk,X=ops(Ztmp,"+",dZtmp,dstep))
        Zchol <- out$Xchol
        indef[2] <- out$indef
        if(indef[2]){
          dstep <- 0.8*dstep
        }else{
          break
        }
      }
      if(t > 1){
        dstep <- gamused*dstep
      }
      #####################
      AXtmp <- AX + pstep*AXfun(blk,At,par$permA,dX)
      prim_infeasnew <- base::norm(b-AXtmp, type="2")/normb2
      if(relgap < 5*infeas){
        alpha <- 1e2
      }else{
        alpha <- 1e3
      }
      if(any(indef == 1)){
        termcode <- -3
        breakyes <- 1
      }else if((prim_infeasnew > max(1e-8,relgap,20*prim_infeas) & iter > 10) | 
               (prim_infeasnew > max(1e-7, 1e3*prim_infeas, 0.1*relgap) & relgap < 1e-2) |
               (prim_infeasnew > alpha*max(1e-9, param$prim_infeas_min) & prim_infeasnew > max(3*prim_infeas, 0.1*relgap) & iter > 25 & dual_infeas < 1e-6 & relgap < 0.1) |
               ((prim_infeasnew > 1e3*prim_infeas & prim_infeasnew > 1e-12) & max(relgap, dual_infeas) < 1e-8)){
        if(stoplevel){
          termcode <- -7
          breakyes <- 1
        }
      }else if((trXZnew > 1.05*runhist$gap[iter]) & (!get("exist_analytic_term", pos=sys.frame(which = -1))) & ((infeas < 1e-5) & (relgap < 1e-4) & (iter > 20) | (max(infeas, relgap) < 1e-7) & (iter > 10))){
        if(stoplevel){
          termcode <- -8
          breakyes <- 1
        }
      }else{
        X <- ops(X, "+", dX, pstep)
        y <- y + dstep*dy
        Z <- ops(Z, "+", dZ, dstep)
      }
    }
    
    #######################################################
    ### ADJUST LINEAR blk ARISING FROM UNRESTRICTED blk ###
    #######################################################
    #print("Adjust Linear blk")
    if(!breakyes){
      for(p in 1:nrow(blk)){
        if(u2lblk[p] == 1){
          len <- blk[[p,2]]/2
          xtmp <- pmin(X[[p,1]][1:len],X[[p,1]][len+c(1:len)])
          alpha <- 0.8
          X[[p,1]][1:len] <- X[[p,1]][(1:len)] - alpha*xtmp
          X[[p,1]][len+c(1:len)] <- X[[p,1]][len+c(1:len)] - alpha*xtmp
          if(mu < 1e-4){
            Z[[p,1]] <- 0.5*mu/pmax(matrix(1,nrow=nrow(as.matrix(X[[p,1]])), ncol=ncol(as.matrix(X[[p,1]]))),as.matrix(X[[p,1]]))
          }else{
            ztmp <- min(1, pmax(Z[[p,1]][1:len], Z[[p,1]][len+(1:len)]))
            if(dual_infeas > 1e-4 & dstep < 0.2){
              beta <- 0.3
            }else{
              beta <- 0.0
            }
            Z[[p,1]][1:len] <- Z[[p,1]][1:len] + beta*ztmp
            Z[[p,1]][len+(1:len)] <- Z[[p,1]][len+(1:len)] + beta*ztmp
          }
        }
      }
    }
    
    ##############################
    ######## PERTURB Z ###########
    ##############################
    #print("Perturb Z")
    if(!breakyes & !get("exist_analytic_term", pos=sys.frame(which = -1))){
      trXZtmp <- blktrace(blk, X, Z)
      trXE <- blktrace(blk, X, EE)
      Zpert <- max(1e-12, 0.2*min(relgap,prim_infeas)) * normC2/normE2
      Zpert <- min(Zpert, 0.1*trXZtmp/trXE)
      Zpert <- min(1, Zpert, 1.5*Zpertold)
      if(infeas < 0.1){
        Z <- ops(Z,"+",EE,Zpert)
        
        Ztmp <- Z
        for(i in 1:length(Z)){
          Ztmp[[i]] <- as.matrix(Z[[i]])
        }
        
        out <- blkcholfun(blk,X=Ztmp)
        Zchol <- out$Xchol
        indef[2] <- out$indef
        if(any(indef[2] == 1)){
          termcode <- -3
          breakyes <- 1
        }
      }
      Zpertold <- Zpert
    }
    
    ############################################
    ### COMPUTE rp, Rd, infeasibilities, etc ###
    ############################################
    #print("Compute rp, Rd, ....")
    AX <- AXfun(blk, At, par$permA, X)
    rp <- b - AX
    ZpATy <- ops(Z, "+", Atyfun(blk, At, par$permA, matrix(0,1,length(par$isspAy)), y))
    ZpATynorm <- ops(ZpATy, "norm")
    Rd <- ops(C, "-", ZpATy)
    objadd <- blkbarrier(blk,X,Z,Xchol,Zchol,parbarrier) + objadd0
    obj <- (normb*normC)*c(blktrace(blk,C,X), as.numeric(t(b) %*% y)) + objadd
    gap <- (normb*normC)*blktrace(blk,X,Z) - diff(objadd)
    relgap <- gap/(1+sum(abs(obj)))
    prim_infeas <- base::norm(rp,type="2")/normb2
    dual_infeas <- ops(Rd, "norm")/normC2
    infeas <- max(prim_infeas, dual_infeas)
    if(scale_data){
      infeas_org[1] <- prim_infeas*normb
      infeas_org[2] <- dual_infeas*normC
    }
    homRd <- Inf
    homrp <- Inf
    if(ops(parbarrier, "norm") == 0){
      if(obj[2] > 0){
        homRd <- ZpATynorm/obj[2]
      }
      if(obj[1] < 0){
        homrp <- base::norm(AX, type="2")/(-obj[1])/(normC)
      }
    }
    trXZ <- blktrace(blk,X,Z,parbarrier)
    if(nn > 0){
      mu <- trXZ/nn
    }else{
      mu <- gap/ops(X, "getM")
    }
    
    normX <- ops(X, "norm")
    
    ##
    
    runhist$pobj <- c(runhist$pobj, obj[1])
    runhist$dobj <- c(runhist$dobj, obj[2])
    runhist$gap <- c(runhist$gap, gap)
    runhist$relgap <- c(runhist$relgap, relgap)
    runhist$pinfeas <- c(runhist$pinfeas, prim_infeas)
    runhist$dinfeas <- c(runhist$dinfeas, dual_infeas)
    runhist$infeas <- c(runhist$infeas, infeas)
    runhist$pstep <- c(runhist$pstep, pstep)
    runhist$dstep <- c(runhist$dstep, dstep)
    runhist$step <- c(runhist$step, min(pstep,dstep))
    runhist$normX <- c(runhist$normX, normX)
    
    ###################################################
    ############## CHECK CONVERGENCE ##################
    ###################################################
    #print("Check Convergence")
    param$use_LU <- get("use_LU", pos=sys.frame(which = -1))
    param$stoplevel <- stoplevel
    param$termcode <- termcode
    param$iter <- iter
    param$obj <- obj
    param$gap <- gap
    param$relgap <- relgap
    param$prim_infeas <- prim_infeas
    param$dual_infeas <- dual_infeas
    param$mu <- mu
    param$homRd <- homRd
    param$homrp <- homrp
    param$AX <- AX
    param$ZpATynorm <- ZpATynorm
    param$normX <- ops(X, "norm")
    param$normZ <- ops(Z, "norm")
    param$numpertdiagschur <- get("numpertdiagschur", pos=sys.frame(which = -1))
    
    if(!breakyes){
      out <- sqlpcheckconv(param,runhist)
      param <- out$param
      breakyes <- out$breakyes
      restart <- out$restart
      msg2 <- out$msg
    }
    if(restart){
      out <- infeaspt(blk,At,C,b,2,1e5)
      rp <- b - AXfun(blk,At,par$permA,X)
      ZpATy <- ops(Z,"+", Atyfun(blk,At,par$permA,matrix(0,1,length(par$isspAy)),y))
      Rd <- ops(C,"-",ZpATy)
      trXZ <- blktrace(blk,X,Z,parbarrier)
      mu <- trXZ/nn
      gap <- (normb*normC)*blktrace(blk,X,Z) - diff(objadd)
      prim_infeas <- norm(rp, type="2")/normb2
      dual_infeas <- ops(Rd,"norm")/normC2
      infeas <- max(prim_infeas,dual_infeas)
      
      out1 <- blkcholfun(blk,X)
      Xchol <- out1$Xchol
      indef[1] <- out1$indef
      
      out2 <- blkcholfun(blk,Z)
      Zchol <- out1$Xchol
      indef[2] <- out1$indef
      
      stoplevel <- 3
    }
    
    ######################################
    ######## CHECK FOR BREAK #############
    ######################################
    #print("Check for Break")
    if(((prim_infeas < 1.5*prim_infeas_best) |
       (max(relgap,infeas) < 0.8*max(relgap_best, infeas_best))) & 
       (max(relgap,dual_infeas) < 0.8*max(relgap_best,dual_infeas_best))){
      Xbest <- X
      ybest <- y
      Zbest <- Z
      prim_infeas_best <- prim_infeas
      dual_infeas_best <- dual_infeas
      relgap_best <- relgap
      infeas_best <- infeas
      update_best <- c(update_best,1)
    }else{
      update_best <- c(update_best,0)
    }
    
    
    if((max(relgap_best,infeas_best) < 1e-4) & norm(update_best[max(1,iter-1):(iter+1)], type="2") == 0){
      termcode <- -9
      breakyes <- 1
    }
    if(breakyes){
      break
    }
  }
  
  use_bestiter <- 1
  if(use_bestiter & param$termcode <= 0){
    X <- Xbest
    y <- ybest
    Z <- Zbest
    
    Xtmp <- X
    for(i in 1:length(X)){
      Xtmp[[i]] <- as.matrix(X[[i]])
    }
    
    Ztmp <- Z
    for(i in 1:length(Z)){
      Ztmp[[i]] <- as.matrix(Z[[i]])
    }
    
    Xchol <- blkcholfun(blk,X=Xtmp)$Xchol
    Zchol <- blkcholfun(blk,X=Ztmp)$Xchol
    AX <- AXfun(blk, At, par$permA, X)
    rp <- b - AX
    ZpATy <- ops(Z, "+", Atyfun(blk, At, par$permA, matrix(0,1,length(par$isspAy)), y))
    Rd <- ops(C, "-", ZpATy)
    objadd <- blkbarrier(blk,X,Z,Xchol,Zchol,parbarrier) + objadd0
    obj <- (normb*normC)*c(blktrace(blk,C,X), as.numeric(t(b) %*% y)) + objadd
    gap <- (normb*normC)*blktrace(blk,X,Z) - diff(objadd)
    relgap <- gap/(1+sum(abs(obj)))
    prim_infeas <- base::norm(rp,type="2")/normb2
    dual_infeas <- ops(Rd, "norm")/normC2
    infeas <- max(prim_infeas, dual_infeas)
    runhist$pobj <- c(runhist$pobj, obj[1])
    runhist$dobj <- c(runhist$dobj, obj[2])
    runhist$gap <- c(runhist$gap, gap)
    runhist$relgap <- c(runhist$relgap, relgap)
    runhist$pinfeas <- c(runhist$pinfeas, prim_infeas)
    runhist$dinfeas <- c(runhist$dinfeas, dual_infeas)
    runhist$infeas <- c(runhist$infeas, infeas)
  }
  
  ##########################################
  ##### UNSCALE AND INFEAS CERTIFICATE #####
  ##########################################
  #print("Unscale")
  if(iter >= 1){
    out <- sqlpmisc(blk,At,C,b,X,y,Z,par$permZ,param)
    X <- out$X
    y <- out$y
    Z <- out$Z
    termcode <- out$termcode
    resid <- out$resid
    reldist <- out$reldist
    msg3 <- out$msg
  }
  
  ###########################################
  ###### RECOVER UNRESTRICTED BLK ###########
  ###########################################
  #print("Recover Unrestricted blk")
  for(p in 1:nrow(blk)){
    if(u2lblk[p] == 1){
      n <- blk[[p,2]]/2
      X[[p,1]] <- X[[p,1]][1:n] - X[[p,1]][n+(1:n)]
      Z[[p,1]] <- Z[[p,1]][[1:n]]
    }
  }
  for(p in 1:nrow(ublkidx)){
    if(length(ublkidx[[p,2]]) > 0){
      n0 <- ublkidx[[p,1]]
      idxB <- setdiff(1:n0, ublkidx[[p,2]])
      tmp <- rep(0, n0)
      tmp[idxB] <- X[[p,1]]
      X[[p,1]] <- tmp
      
      tmp <- rep(0, n0)
      tmp[idxB] <- Z[[p,1]]
      Z[[p,1]] <- tmp
    }
  }
  if(analytic_prob){
    X <- X[1:numblkold]
    Z <- Z[1:numblkold]
  }
  
  ########################################
  ############## SUMMARY #################
  ########################################
  #print("Summary")
  maxC <- 1 + ops(ops(C,"abs"),"max")
  maxb <- 1 + max(abs(b))
  if(scale_data){
    #
  }else{
    dimacs <- as.matrix(c(prim_infeas*normb2/maxb, 0, dual_infeas*normC2/maxC, 0))
  }
  
  dimacs <- rbind(dimacs, as.matrix(c(-diff(obj), gap)/(1+sum(abs(obj)))))
  
  info <- list(dimacs = dimacs,
               termcode = termcode,
               iter = iter,
               obj = obj,
               gap = gap,
               relgap = relgap,
               pinfeas = prim_infeas,
               dinfeas = dual_infeas,
               resid = resid,
               reldist = reldist,
               normX = ops(X, "norm"),
               normy = base::norm(y, type="2"),
               normZ = ops(Z, "norm"),
               normb = normb2,
               maxb = maxb,
               normC = normC2,
               maxC = maxC,
               normA = normA2)
  
  return(list(obj=obj,X=X,y=y,Z=Z,info=info,runhist=runhist))
  
}