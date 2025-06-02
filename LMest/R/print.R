# print.LMbasic <-function(x, ...){
#   cat("Call:\n")
#   print(x$call)
#   cat("\nConvergence info:\n")
#   print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
# }

print.LMbasic <-function(x, ...){
  cat("\nBasic Latent Markov model\n")
  if(!is.null(x$call))
  {
    cat("Call:\n")
    print(x$call)
  }
  cat("\nAvailable objects:\n")
  print(names(x))
  if(is.null(x$TT)){
    cat("\nConvergence info:\n")
    warning("est_lm_basic function is no longer maintained. Please look at lmest function",call. = FALSE)
    print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
  }else{
    if(!is.null(x$lk)){
      cat("\nConvergence info:\n")
      print(data.frame(LogLik=x$lk,np=x$np, k = x$k, AIC=x$aic, BIC=x$bic, n = x$n, 
                       TT=x$TT, row.names=" "))
    }
  }

}


# print.LMbasiccont <-function(x, ...){
#   cat("Call:\n")
#   print(x$call)
#   cat("\nConvergence info:\n")
#   print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
# }

print.LMbasiccont <-function(x, ...){
  cat("\nBasic Latent Markov model for continuous outcomes\n")
  if(!is.null(x$call))
  {
    cat("Call:\n")
    print(x$call)
  }
  cat("\nAvailable objects:\n")
  print(names(x))
  if(is.null(x$TT)){
    cat("\nConvergence info:\n")
    warning("est_lm_basic_cont function is no longer maintained. Please look at lmestCont function",call. = FALSE)
    print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
  }else{
    if(!is.null(x$lk)){
      cat("\nConvergence info:\n")
      print(data.frame(LogLik=x$lk,np=x$np, k = x$k, AIC=x$aic, BIC=x$bic, n = x$n, TT = x$TT, row.names = " "))
    }
 }
}

print.LMmanifestcont <-function(x, ...){
  cat("\nBasic Latent Markov model for continuous outcomes with covariates 
      affecting the manifest distribution\n")
  if(!is.null(x$call))
  {
    cat("Call:\n")
    print(x$call)
  }
  cat("\nAvailable objects:\n")
  print(names(x))
  if(is.null(x$TT)){
    cat("\nConvergence info:\n")
    warning("est_lm_basic_cont function is no longer maintained. Please look at lmestCont function",call. = FALSE)
    print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
  }else{
    if(!is.null(x$lk)){
      cat("\nConvergence info:\n")
      print(data.frame(LogLik=x$lk,np=x$np, k = x$k, AIC=x$aic, BIC=x$bic, n = x$n, TT = x$TT, row.names = " "))
    }
  }
}

print.LMbasiccontMISS <-function(x, ...){
  cat("\nBasic Latent Markov model for continuous outcomes and missing responses (MAR) \n")
  if(!is.null(x$call))
  {
    cat("Call:\n")
    print(x$call)
  }
  cat("\nAvailable objects:\n")
  print(names(x))
 
  if(is.null(x$TT)){
    cat("\nConvergence info:\n")
    warning("est_lm_basic_cont function is no longer maintained. Please look at lmestCont function",call. = FALSE)
    print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
  }else{
    if(!is.null(x$lk)){
      cat("\nConvergence info:\n")
      print(data.frame(LogLik=x$lk,np=x$np, k = x$k, AIC=x$aic, BIC=x$bic, n = x$n, TT = x$TT, row.names = " "))
    }
  }
}

# print.LMlatent <-function(x,...){
#   cat("Call:\n")
#   print(x$call)
#   cat("\nConvergence info:\n")
#   print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
# }

print.LMlatent <-function(x,...){
  cat("\nBasic Latent Markov model with covariates in the latent model\n")
  if(!is.null(x$call))
  {
    cat("Call:\n")
    print(x$call)
  }
  cat("\nAvailable objects:\n")
  print(names(x))
  if(is.null(x$TT)){
    cat("\nConvergence info:\n")
    warning("est_lm_cov_latent function is no longer maintained. Please look at lmest function",call. = FALSE)
    print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
  }else{
    if(!is.null(x$lk)){
      cat("\nConvergence info:\n")
      print(data.frame(LogLik=x$lk,np=x$np, k = x$k, AIC=x$aic, BIC=x$bic, n = x$n, TT = x$TT, row.names = " "))
    }
  }

}

# print.LMlatentcont <-function(x,...){
#   cat("Call:\n")
#   print(x$call)
#   cat("\nConvergence info:\n")
#   print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
# }

print.LMlatentcont <-function(x,...){
  cat("\nBasic Latent Markov model for continuous outcomes with covariates in the latent model\n")
  if(!is.null(x$call)){
    cat("Call:\n")
    print(x$call)
  }
  cat("\nAvailable objects:\n")
  print(names(x))
  if(is.null(x$TT)){
    cat("\nConvergence info:\n")
    warning("est_lm_cov_latent_cont function is no longer maintained. Please look at lmestCont function",call. = FALSE)
    print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
  }else{
    if(!is.null(x$lk)){ 
      cat("\nConvergence info:\n")
      print(data.frame(LogLik=x$lk,np=x$np, k = x$k, AIC=x$aic, BIC=x$bic, n = x$n, 
                       TT=x$TT, row.names=" "))
    }
  }
}

print.LMlatentcontMISS <-function(x,...){
  cat("\nBasic Latent Markov model for continuous outcomes with covariates in the latent model and missing responses (MAR)\n")
  if(!is.null(x$call))
  {
    cat("Call:\n")
    print(x$call)
  }
  cat("\nAvailable objects:\n")
  print(names(x))
  if(is.null(x$TT)){
    cat("\nConvergence info:\n")
    warning("est_lm_cov_latent_cont function is no longer maintained. Please look at lmestCont function",call. = FALSE)
    print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
  }else{
    if(!is.null(x$lk)){ 
      cat("\nConvergence info:\n")
      print(data.frame(LogLik=x$lk,np=x$np, k = x$k, AIC=x$aic, BIC=x$bic, n = x$n, TT = x$TT, row.names = " "))
    }
  }
}
# print.LMmanifest <-function(x,...){
#   cat("Call:\n")
#   print(x$call)
#   cat("\nConvergence info:\n")
#   print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
# }

print.LMmanifest <-function(x,...){
  cat("\nBasic Latent Markov model with covariates in the measurement model\n")
  if(!is.null(x$call))
  {
    cat("Call:\n")
    print(x$call)
  }
  cat("\nAvailable objects:\n")
  print(names(x))
  cat("\nConvergence info:\n")
  if(is.null(x$TT))
  {
    warning("est_lm_cov_manifest function is no longer maintained. Please look at lmest function",call. = FALSE)
    print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
  }else{
    if(!is.null(x$lk)){ 
      cat("\nConvergence info:\n")
      print(data.frame(LogLik=x$lk,np=x$np, k = x$k, AIC=x$aic, BIC=x$bic, n = x$n, TT = x$TT, row.names = " "))
    }
  }
}

# print.LMmixed <-function(x, ...){
#   cat("Call:\n")
#   print(x$call)
#   cat("\nConvergence info:\n")
#   print(cbind(LogLik=x$lk,np=x$np,BIC=x$bic))
# }

print.LMmixed <-function(x, ...){
  cat("\nMixed Latent Markov model\n")
  if(!is.null(x$call))
  {
    cat("Call:\n")
    print(x$call)
  }
  cat("\nAvailable objects:\n")
  print(names(x))
  if(is.null(x$TT)){
    cat("\nConvergence info:\n")
    warning("est_lm_mixed function is no longer maintained. Please look at lmestMixed function",call. = FALSE)
    print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
  }else{
    if(!is.null(x$lk)){ 
      cat("\nConvergence info:\n")
      print(data.frame(LogLik=x$lk,np=x$np, k1 = x$k1, k2 = x$k2, AIC=x$aic, BIC=x$bic, n = x$n, TT = x$TT, row.names = " "))
    }
  }
}

print.LMsearch <-function(x, modSel = "BIC",...){
  if(!is.null(x$call))
  {
    cat("Call:\n")
    print(x$call)
  }
  cat("\nAvailable objects:\n")
  print(names(x))
  #print(rbind(lkv=x$lkv,aicv=x$aicv,bicv=x$bicv))
  # cat("lk = ",x$lkv,"\n")
  # cat("aic = ",x$aicv,"\n")
  # cat("bic = ",x$bicv,"\n")
  # cat("\nTable:\n")
  k = sapply(x[[1]], function(l) l$k)
  	if(!is.null(x$aicv)){
	  Aic = x$aicv
	  Bic = x$bicv
	}else{
	  Aic = x$Aic
	  Bic = x$Bic
	}
  dt = data.frame(states=x$k,lk=x$lkv,aic=Aic,bic=Bic)
#  dt <- data.frame(lk = x$lkv, aic = x$aicv, bic = x$bicv, row.names = paste0("k=",k))
  cat("\nBest model:\n")
  bs <- ifelse(modSel == "AIC", which.min(dt$aic), which.min(dt$bic))
  modbs <- x[[1]][[bs]]
  if(is.null(modbs$TT)){
    warning("search.model.LM function is no longer maintained. Please look at lmestSearch function",call. = FALSE)
    print(cbind(k=x$k[bs],LogLik=modbs$lk,np=modbs$np,AIC=modbs$aic, BIC=modbs$bic))
  }else{
    print(data.frame(k=x$k[bs],LogLik=modbs$lk,np=modbs$np, k = modbs$k, AIC=modbs$aic, BIC=modbs$bic, n = modbs$n, TT = modbs$TT, row.names = " "))
  }
}

# print.MCbasic <-function(x, ...){
#   cat("Call:\n")
#   print(x$call)
#   cat("\nConvergence info:\n")
#   print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
# }

print.MCbasic <-function(x, ...){
  cat("\nBasic Markov chain model\n")
  if(!is.null(x$call))
  {
    cat("Call:\n")
    print(x$call)
  }
  cat("\nAvailable objects:\n")
  print(names(x))
  cat("\nConvergence info:\n")
  if(is.null(x$TT))
  {
    warning("est_mc_basic function is no longer maintained. Please look at lmest function",call. = FALSE)
    print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
  }else{
    print(data.frame(LogLik=x$lk,np=x$np, AIC=x$aic, BIC=x$bic, n = x$n, TT = x$TT, row.names = " "))
  }
}

# print.MCcov <-function(x,...){
#   cat("Call:\n")
#   print(x$call)
#   cat("\nConvergence info:\n")
#   print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
# }

print.MCcov <-function(x,...){
  cat("\nMarkov Chain model with covariates\n")
  if(!is.null(x$call))
  {
    cat("Call:\n")
    print(x$call)
  }
  cat("\nAvailable objects:\n")
  print(names(x))
  cat("\nConvergence info:\n")
  if(is.null(x$TT))
  {
    warning("est_mc_cov function is no longer maintained. Please look at lmest function",call. = FALSE)
    print(cbind(LogLik=x$lk,np=x$np,AIC=x$aic, BIC=x$bic))
  }else{
    print(data.frame(LogLik=x$lk,np=x$np, AIC=x$aic, BIC=x$bic, n = x$n, TT = x$TT, row.names = " "))
  }
}