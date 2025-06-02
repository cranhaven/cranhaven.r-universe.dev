confint.est_multi_poly_within <-function(object, parm, level=0.95, ...){
	
# preliminaries
	parm = NULL
	out = object
# print output
	if(is.null(out$seTh1)) stop("Use option out_se=TRUE in est_multi_poly_within()")
	za = qnorm(1-(1-level)/2)	
    cat("\nConfidence interval for abilities for the 1st latent variable:\n")
    L1Th1 = out$Th1-za*out$seTh1
    L2Th1 = out$Th1+za*out$seTh1
    Tmp = cbind(L1Th1,L2Th1)
    ind = NULL
    for(j in 1:ncol(out$Th1)) ind = c(ind,j,j+ncol(out$Th1))
	Tmp = matrix(Tmp[,ind],nrow(out$Th1),2*ncol(out$Th1))
    dimnames(Tmp) = list(dimension=1:nrow(out$Th1),class=paste((1:ncol(out$Th1))%x%c(1,1),c(1,2),sep="_"))
    print(round(Tmp,4))    
    cat("\nConfidence interval for abilities for the 2nd latent variable:\n")
    L1Th2 = out$Th2-za*out$seTh2
    L2Th2 = out$Th2+za*out$seTh2
    Tmp = cbind(L1Th2,L2Th2)
    ind = NULL
    for(j in 1:ncol(out$Th2)) ind = c(ind,j,j+ncol(out$Th2))
	Tmp = matrix(Tmp[,ind],nrow(out$Th2),2*ncol(out$Th2))
    dimnames(Tmp) = list(dimension=1:nrow(out$Th2),class=paste((1:ncol(out$Th2))%x%c(1,1),c(1,2),sep="_"))
    print(round(Tmp,4))    
    cat("\nConfidence interval for the item parameters:\n")
    Tmp = out$Bec-za*out$seBec
    for(j in 1:ncol(out$Bec)) colnames(Tmp)[j] = paste("beta",j,"_1",sep="")
    L1Items = cbind(gamma1_1=out$ga1c-za*out$sega1c,gamma2_1=out$ga2c-za*out$sega2c,Tmp)
    Tmp = out$Bec+za*out$seBec
    for(j in 1:ncol(out$Bec)) colnames(Tmp)[j] = paste("beta",j,"_2",sep="")
    L2Items = cbind(gamma1_2=out$ga1c+za*out$sega1c,gamma2_2=out$ga2c+za*out$sega2c,Tmp)
    Tmp = cbind(L1Items,L2Items)
    ind = NULL
    for(j in 1:ncol(L1Items)) ind = c(ind,j,j+ncol(L1Items))
    Tmp = Tmp[,ind]
    print(round(Tmp,4))              
    cat("\nConfidence interval for the regression coefficients for the 1st latent variable:\n")
    L1De1 = out$De1-za*out$seDe1
    L2De1 = out$De1+za*out$seDe1
   	Tmp = cbind(L1De1,L2De1)
    names = dimnames(Tmp)
    ind = NULL
    for(j in 1:ncol(out$De1)) ind = c(ind,j,j+ncol(out$De1))
    Tmp = Tmp[,ind]
    if(is.vector(Tmp)) Tmp = t(Tmp)
    dimnames(Tmp) = list(names[[1]],logit=paste(names[[2]][ind],c(1,2),sep="_"))
    print(round(Tmp,4))
    cat("\nConfidence interval for the regression coefficients for the 2nd latent variable:\n")
    L1De2 = out$De2-za*out$seDe2
    L2De2 = out$De2+za*out$seDe2
   	Tmp = cbind(L1De2,L2De2)
    names = dimnames(Tmp)
    ind = NULL
    for(j in 1:ncol(out$De2)) ind = c(ind,j,j+ncol(out$De2))
    Tmp = Tmp[,ind]
    if(is.vector(Tmp)) Tmp = t(Tmp)
    dimnames(Tmp) = list(names[[1]],logit=paste(names[[2]][ind],c(1,2),sep="_"))
    print(round(Tmp,4))
    cat("\n")
# output
	out = list(L1Th1=L1Th1,L2Th1=L2Th1,L1Th2=L1Th2,L2Th2=L2Th2,L1Items=L1Items,L2Items=L2Items,L1De1=L1De1,L2De1=L2De1,L1De2=L1De2,L2De2=L2De2)    
	
}