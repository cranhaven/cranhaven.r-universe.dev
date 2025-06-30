## Write a function that takes same tree and frame
## arguments as to plotSigTree, and performs adonis test
## on p-values (or transformed z-values if z=TRUE);
## returns summary table in ANOVA-style format.
## argument make2sided is a logical indicating whether 1-sided p-values in
## the unsorted.pvalues data frame should be first converted to 2-sided
adonis.tree <- function(tree,unsorted.pvalues,seed=1234,perms=10000,z=TRUE,make2sided=TRUE)
  {
    # Error checking (same as in plotSigTree)
	if(class(tree)!="phylo")
	{
		return(cat("Error: Class of tree must be phylo.","\n"))
	}
    tree <- reorder(tree, order='cladewise')
	if(length(tree$tip.label)!=length(unsorted.pvalues[,1]))
	{
		return(cat("Error: There must be the same number of tip labels in tree as in unsorted.pvalues.","\n"))
	}else if(mean(sort(as.character(unique(unsorted.pvalues[,1])))==sort(as.character(unique(tree$tip.label))))!=1)
	{
		return(cat("Error: The tip labels in tree must have the same labels as the tip labels in unsorted.pvalues.","\n"))
	}	
	if(min(unsorted.pvalues[,2])<0 | max(unsorted.pvalues[,2])>1)
	{
		return(cat("Error: P-values in unsorted.pvalues must be between greater than or equal to 0 and less than or equal to 1.","\n"))
	}else
	{
		f.one <- 1-100*.Machine$double.eps
		f.zero <- 100*.Machine$double.eps
		t <- unsorted.pvalues[,2]>=f.one
		unsorted.pvalues[t,2] <- f.one
		t <- unsorted.pvalues[,2]<=f.zero
		unsorted.pvalues[t,2] <- f.zero
	}
    if(!is.logical(z))
	{
      return(cat("Error: z must be TRUE or FALSE","\n"))	
	 }
    if(!is.logical(make2sided))
	{
      return(cat("Error: make2sided must be TRUE or FALSE","\n"))	
	 }
	if(!is.numeric(seed)){return(cat("Error: seed must be numeric","\n"))}
    if(!is.numeric(perms)){return(cat("Error: perms must be numeric","\n"))}

	if(make2sided)
	  {
	      p1 <- unsorted.pvalues[,2]
	      t <- p1 <= 0.5 # T/F of left tail
		  p2 <- rep(NA,length(p1))
		  p2[t] <- 2*p1[t]
		  p2[!t] <- 2*(1-p1[!t])
		  unsorted.pvalues[,2] <- p2
	    }
	
	# Do test and return summary result
     set.seed(seed)
     dmat <- cophenetic(tree)
     M <- as.dist(dmat)
     f1 <- data.frame(OTU=colnames(dmat))
	 use.tframe <- data.frame(OTU=unsorted.pvalues[,1],pval=unsorted.pvalues[,2])
     tempF <- merge(f1,use.tframe,sort=FALSE)
     x <- tempF$pval

    # Repeat numerical stability correction on these p-values  (added to SigTree version 1.10.5)
     f.one <- 1-100*.Machine$double.eps
     f.zero <- 100*.Machine$double.eps
     t <- x >= f.one
     x[t] <- f.one
     t <- x[2]<=f.zero
     x[t] <- f.zero

     if(z){x <- qnorm(x)}
     res <- adonis(M~x,permutations=perms)
	 return(res$aov.tab[1,6])
     #aov.mat <- round(as.matrix(res$aov.tab),4)
	 #aov.mat[is.na(aov.mat)] <- ""
	 #return(as.data.frame(aov.mat))
   }
