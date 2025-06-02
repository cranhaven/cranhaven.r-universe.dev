summary.LMsearch<-function(object,...){

  if(!is.null(object$call))
  {
    cat("Call:\n")
    print(object$call)
    cat("\n")
  }
	kdim = length(object$kv)
	np = lk = AIC = BIC= rep(0,kdim)
	cont=0
	kv <- object$k
	for(k in kv){
		cont=cont+1
		np[cont] = object$out.single[[cont]]$np
		lk[cont] = object$lkv[cont]
		#AIC[cont] = object$Aic[k]
		#BIC[cont] = object$Bic[k]
	}
	if(!is.null(object$aicv))
	{
	  Aic = object$aicv
	  Bic = object$bicv
	}else{
	  Aic = object$Aic
	  Bic = object$Bic
	}
	out <- data.frame(states=object$k,lk=lk,np=np,AIC=Aic,BIC=Bic)
	print(out, row.names = FALSE)

}
