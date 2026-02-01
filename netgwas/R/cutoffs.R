#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
#-------------------------------------------------------------------------------#

cutoffs = function(y){
	p<-ncol(y)
	n<-nrow(y)
	k<-unique(sort(unlist(y)))
	n.levels<-length(k)
	q<-matrix(nrow=p,ncol=n.levels)
	for(i in 1:p){
		X=factor(y[,i],levels=k)
		No<-tabulate(X, nbins=n.levels)
		q[i,]<-qnorm(cumsum(No)/n)
	}
	q[ ,n.levels] <- Inf
	q<-cbind(-Inf,q)
	return(q)
}