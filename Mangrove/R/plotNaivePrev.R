plotNaivePrev <-
function(ped,K,maxN=NULL,...){
	N <- length(ped[,6])
	x <- sum(ped[,6] == 2)
	temp <- dbinom(0:N,N,K)
	if (is.null(maxN)){
		maxX <- max(min(which(cumsum(temp) > 0.99) + 2),x)
	} else {
		maxX <- maxN
	}
	barplot(dbinom(0:maxX,N,K),names.arg=0:maxX,...)
	abline(v=0.7 + (K*N)*1.2,col="green",lwd=2,lty=2)
	abline(v=0.7 + (x)*1.2,col="red",lwd=2,lty=2)
	}

