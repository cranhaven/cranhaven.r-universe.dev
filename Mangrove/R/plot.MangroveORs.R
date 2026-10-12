plot.MangroveORs <-
function(x,K=NULL,...) {
	
	if (is.null(K)){
		temp1 <- sapply(1:length(x$rsID),function(i) .getVarExp_loci(0.1,x$ORhet[i],x$ORhom[i],x$Freq[i]))
		ids <- x$rsID[order(temp1)]
		temp1 <- cumsum(sort(temp1,decreasing=T))/(sum(temp1) + 1)
		temp2 <- sapply(1:length(x$rsID),function(i) .getVarExp_loci(0.005,x$ORhet[i],x$ORhom[i],x$Freq[i]))
		temp2 <- cumsum(sort(temp2,decreasing=T))/(sum(temp2) + 1)
		dat <- rbind(temp1,temp2)
		dimnames(dat)[[1]] <- c("10% disease","1% disease")
		dimnames(dat)[[2]] <- ids
		barplot(dat,beside=T,col=c("blue","darkgreen"),xlab="Cumulative variants",ylab="Variance explained")
		legend(0.7,max(c(temp1,temp2)),c("10% disease","0.5% disease"),col=c("blue","darkgreen"),pch=15)	
	} else {
		temp1 <- sapply(1:length(x$rsID),function(i) .getVarExp_loci(K,x$ORhet[i],x$ORhom[i],x$Freq[i]))
		ids <- x$rsID[order(temp1)]
		temp1 <- cumsum(sort(temp1,decreasing=T))/(sum(temp1) + 1)
		names(temp1) <- ids
		barplot(temp1,beside=T,col=c("blue"),xlab="Cumulative variants",ylab="Variance explained")			
		}
}

