coef.est_multi_poly_between <-function(object, ...){
	
# preliminaries
	out = object
# print output	
    cat("\nEstimated abilities:\n")
    print(round(out$Th,4))    
    cat("\nEstimated item parameters:\n")
    Tmp = out$Bec
    for(j in 1:ncol(out$Bec)) colnames(Tmp)[j] = paste("beta",j,sep="")
    Items = cbind(gamma=out$gac,Tmp)
    print(round(Items,4))                
    cat("\nEstimated regression coefficients:\n")
    print(round(out$De,4))
    cat("\n")
# output
	out = list(Th=out$Th,Items=out$Items,De=out$De)    
	
}