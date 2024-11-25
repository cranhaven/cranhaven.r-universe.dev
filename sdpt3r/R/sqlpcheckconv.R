sqlpcheckconv <- function(param, runhist){
  
  termcode    <- param$termcode 
  iter        <- param$iter 
  obj         <- param$obj
  relgap      <- param$relgap 
  gap         <- param$gap 
  prim_infeas <- param$prim_infeas
  dual_infeas <- param$dual_infeas 
  mu          <- param$mu  
  homrp       <- param$homrp
  homRd       <- param$homRd
  prim_infeas_bad <- param$prim_infeas_bad 
  dual_infeas_bad <- param$dual_infeas_bad 
  
  if(iter > 15){
    prim_infeas_min <- min(param$prim_infeas_min, max(prim_infeas,1e-10))        
    dual_infeas_min <- min(param$dual_infeas_min, max(dual_infeas,1e-10)) 
  }else{
    prim_infeas_min <- Inf 
    dual_infeas_min <- Inf 
  }
  
  printlevel  <- param$printlevel
  stoplevel   <- param$stoplevel 
  ublksize    <- param$ublksize 
  use_LU      <- param$use_LU 
  numpertdiagschur <- param$numpertdiagschur
  infeas      <- max(prim_infeas,dual_infeas) 
  restart     <- 0 
  breakyes    <- 0 
  msg         <- c()
  ##
  if(param$normX > 1e15*param$normX0 | param$normZ > 1e15*param$normZ0){
    termcode <- 3
    breakyes <- 1 
  }
  
  err <- max(infeas,relgap)
  idx <- c(max(2,iter-9):(iter+1))
  pratio <- (1-runhist$pinfeas[idx]/runhist$pinfeas[idx-1])/runhist$pstep[idx] 
  dratio <- (1-runhist$dinfeas[idx]/runhist$dinfeas[idx-1])/runhist$dstep[idx] 
  
  if((param$homRd < 0.1*sqrt(err*max(param$inftol,1e-13))) & (iter > 30 | termcode==3) & (mean(abs(dratio-1)) > 0.5)){ 
    termcode <- 1
    breakyes <- 1
  }
  
  if((param$homrp < 0.1*sqrt(err*max(param$inftol,1e-13))) & (iter > 30 | termcode==3) & (mean(abs(pratio-1)) > 0.5)){ 
    termcode <- 2
    breakyes <- 1
  }
  
  if((stoplevel) & (iter > 2) & (!breakyes)){
    prim_infeas_bad <- (prim_infeas > max(1e-10,1e2*prim_infeas_min) & (prim_infeas_min < 1e-2)) +
      #(prim_infeas > prod(1.5-runhist$step[(iter+1):(iter-1)])*runhist$pinfeas[iter-2])
      (prim_infeas > runhist$pinfeas[iter-2])
    dual_infeas_bad <- (dual_infeas > max(1e-8,1e3*dual_infeas_min) & (dual_infeas_min < 1e-2))
    
    if((mu < 1e-8) | (use_LU)){
      idx <- c(max(1,iter-1):iter)
    }else if(mu < 1e-4){
      idx <- c(max(1,iter-2):iter) 
    }else{
      idx <- c(max(1,iter-3): iter)
    }
    
    gap_progress_bad <- (infeas < 1e-4) & (relgap < 5e-3) & (gap > 0.9*exp(mean(log(abs(runhist$gap[idx])))))
    gap_progress_bad2 <- (infeas < 1e-4) & (relgap < 1) & (gap > 0.95*exp(mean(log(abs(runhist$gap[idx]))))) 
    gap_ratio <- runhist$gap[idx+1]/runhist$gap[idx] 
    idxtmp <- c(max(1,iter-4):iter) 
    gap_ratio_tmp <- runhist$gap[idxtmp+1]/runhist$gap[idxtmp]
    gap_slowrate <- min(0.8,max(0.6,2*mean(gap_ratio_tmp)))
    idx2 <- c(max(1,iter-10): iter) 
    gap_ratio2 <- runhist$gap[idx2+1]/runhist$gap[idx2]
    gap_slow  <- all(gap_ratio > gap_slowrate)  
    gap_slow2 <- all(gap_ratio2 > gap_slowrate) 
    
    
    if((iter > 20) & (infeas < 1e-4 | prim_infeas_bad) & (max(infeas,relgap) < 1) & !(min(runhist$step[idx]) > 0.2 & ublksize)){
      if((gap_slow & prim_infeas_bad & (relgap < 1e-3)) | (gap_slow2 & prim_infeas_bad & ublksize & (runhist$step[iter+1] > 0.2))){
        msg <- 'stop: progress is too slow' 
        termcode <- -5 
        breakyes <- 1
      }else if((max(infeas,relgap) < 1e-2) & (prim_infeas_bad)){ 
        if (relgap < max(0.2*prim_infeas,1e-2*dual_infeas)){ 
          msg <- 'stop: relative gap < infeasibility' 
          termcode <- -1
          breakyes <- 1 
        }
      }
    } 
    
    if((iter > 20) & (gap_progress_bad) & (prim_infeas_bad | any(runhist$step[idx] > 0.5))){
      msg <- 'stop: progress is bad' 
      termcode <- -5
      breakyes <- 1  
    }
    
    if((iter > 20) & (gap_progress_bad2) & (numpertdiagschur > 10)){
      msg <- 'stop: progress is bad' 
      termcode <- -5
      breakyes <- 1  
    }
    
    if((iter > 30) & (prim_infeas_bad) & (gap_slow) & (relgap < 1e-3) & (dual_infeas < 1e-5) & !(min(runhist$step[idx]) > 0.2 & ublksize)){
      msg <- 'stop: progress is bad*' 
      termcode <- -5
      breakyes <- 1 
    }
    
    if((iter > 30) & (dual_infeas_bad) & (relgap < 1e-3) & (dual_infeas < 1e-5) & !(min(runhist$step[idx]) > 0.2 & ublksize)){
      msg <- 'stop: dual infeas has deteriorated too much' 
      termcode <- -5
      breakyes <- 1 
    }
    
    if((iter > 50) & (prim_infeas/runhist$pinfeas[1] < 1e-6) & (dual_infeas/runhist$dinfeas[1] > 1e-3) & (runhist$step[iter+1] > 0.2) & (relgap > 1e-3)){ 
      msg <- 'stop: lack of progress in dual infeas' 
      termcode <- -5
      breakyes <- 1 
    }
    
    if((iter > 50) & (dual_infeas/runhist$dinfeas[1] < 1e-6) & (prim_infeas/runhist$pinfeas[1] > 1e-3) & (runhist$step[iter+1] > 0.2) & (relgap > 1e-3)){ 
      msg <- 'stop: lack of progress in primal infeas' 
      termcode <- -5
      breakyes <- 1 
    }
    
    if((min(runhist$infeas) < 1e-4 | (prim_infeas_bad & iter > 10)) & (max(runhist$infeas) > 1e-5) | (iter > 20)){ 
      relgap2 <- abs(diff(obj))/(1+sum(abs(obj))) 
      if (relgap2 < 1e-3){ 
        step_short <- all(runhist$step[c(iter:(iter+1))] < 0.05) 
      }else if((relgap2 < 1) | (use_LU)){
        idx <- c(max(1,iter-3):(iter+1))
        step_short <- all(runhist$step[idx] < 0.03) 
      }else{
        step_short <- 0 
      }
      
      if((step_short) & (relgap2 < 1e-2)){
        msg <- 'stop: steps too short consecutively' 
        termcode <- -5 
        breakyes <- 1      
      }
    }
    
    if((iter > 3 & iter < 20) & (infeas > 1) & (min(param$homrp,param$homRd) > min(1e-8,param$inftol)) & (max(runhist$step[max(1,iter-3):iter+1]) < 1e-3)){ 
      if (stoplevel == 2){
        msg <- 'stop: steps too short consecutively*' 
        termcode <- -5 
        breakyes <- 1             
      }else if(stoplevel == 3){
        msg <- 'stop: steps too short consecutively*' 
        termcode <- -5
        breakyes <- 1              
      }else{ 
        prim_infeas_min <- 1e20 
        prim_infeas_bad <- 0 
        restart <- 1 
      }
    }
  }
  
  if((max(relgap,infeas) < param$gaptol)){
    msg <- paste('stop: max(relative gap, infeasibilities) < %3.2e',param$gaptol)
    termcode <- 0
    breakyes <- 1
  }
  ##
  param$prim_infeas_bad <- prim_infeas_bad
  param$prim_infeas_min <- prim_infeas_min
  param$dual_infeas_bad <- dual_infeas_bad
  param$dual_infeas_min <- dual_infeas_min
  param$termcode <- termcode
  
  return(list(param=param, breakyes = breakyes, restart=restart, msg=msg))
}