#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
#-------------------------------------------------------------------------------#

lower.upper = function(y){
	cutoffs <- cutoffs(y)
	levels	<- unique(sort(unlist(y)))
	n.levels<- length(levels)
	n <- nrow(y)
	p <- ncol(y)
	lower = matrix(nrow=n,ncol=p)
	upper = matrix(nrow=n,ncol=p)
	for (i in 1:n){
		sel <- match(y[i,],levels)
		lower[i,]<-apply(cbind(sel,cutoffs),1,function(x){x[x[1]+1]})
		upper[i,]<-apply(cbind(sel,cutoffs),1,function(x){x[x[1]+2]})
	}
	lower[is.na(lower)] <- -Inf 
	upper[is.na(upper)] <- Inf	
	
	return(list(lower=lower,upper=upper))
}