plot.MangroveBetas <- function(x,...) {
	
	temp1 <- 2*x$Freq*(1 - x$Freq)*x$beta^2
	ids <- x$rsID[order(temp1)]
	temp1 <- cumsum(sort(temp1,decreasing=T))/(sum(temp1) + 1)
	names(temp1) <- ids
	barplot(temp1,beside=T,col=c("blue"),xlab="Cumulative variants",ylab="Variance explained")
}
