# Call significant SNPs using given FDR threshold and responsibility matrix
## Input:
# wMat: responsibility matrix, a Gx3 matrix, 
#       each row is a SNP
#       each column represent probability of the SNP belong to a group of "-", "0","+" 
# fdr: FDR threshold used to call SNPs
## Output:
# result: SNPs cluster membership
callSNP <- function(wmat, fdr=0.05)
{
	result <- rep("0", nrow(wmat)) #initial values for cluster membership
	idx <- 1:nrow(wmat) # index of samples to keep order info
	wmax <- apply(wmat[,-2],1,max,na.rm=T) # find the largest prob of "-" or "+" groups
	ord <- order(wmax,decreasing=T) # order by max prob
	wmat <- wmat[ord,]  # sort data
	idx <- idx[ord]			# sort index
	fdrs <- cumsum(wmat[,2])/(1:nrow(wmat)) # calculate FDRs for different cutoff tau values
	n.sig <- max(which(fdrs<fdr))  #the number of SNPs are called as significant under FDR control
	if(!is.finite(n.sig)) return(result)  # if no fdrs is smaller than FDR threshold
	result[1:n.sig] <- ifelse(wmat[1:n.sig,1] > wmat[1:n.sig,3], "-", "+")
	return(result[order(idx)])
}

# # generate wmat and call SNPs
# wmat <- matrix(NA, 100, 3)
# colnames(wmat) <- c("-", "0", "+")
# rownames(wmat) <- paste0("g",1:100)
# wmat[,2] <- runif(100, 0,1)
# for(ii in 1:100) wmat[ii,1] <- runif(1, 0,1-wmat[ii,2])
# wmat[,3] <- 1- wmat[,1]- wmat[,2]
# 
# memb1 <- callSNP(wmat, fdr=0.1)
# memb2 <- callSNP(wmat, fdr=0.05)