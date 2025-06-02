coef.est_multi_poly_within <-function(object, ...){
	
# preliminaries
	out = object
# print output	
    cat("\nEstimated abilities for the 1st latent variable:\n")
    print(round(out$Th1,4))    
    cat("\nEstimated abilities for the 2nd latent variable:\n")
    print(round(out$Th2,4))
    cat("\nEstimated item parameters:\n")
    Tmp = out$Bec
    for(j in 1:ncol(out$Bec)) colnames(Tmp)[j] = paste("beta",j,sep="")
    Items = cbind(gamma1=out$ga1c,gamma2=out$ga2c,Tmp)
    print(round(Items,4))                
    cat("\nEstimated regression coefficients for the 1st latent variable:\n")
    print(round(out$De1,4))
    cat("\nEstimated regression coefficients for the 2nd latent variable:\n")
    print(round(out$De2,4))
    cat("\n")
# output
	out = list(Th1=out$Th1,Th2=out$Th2,Items=out$Items,De1=out$De1,De2=out$De2)    
	
}