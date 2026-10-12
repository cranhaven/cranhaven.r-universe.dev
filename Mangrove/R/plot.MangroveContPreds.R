plot.MangroveContPreds <- function(x,...) {
	temp <- x
	h1 <- hist(temp,axes=F,xlab=deparse(substitute(x)),main="Histogram of continuous predictions",col="grey",...)
	axis(2)
	axis(1,at=h1$breaks,labels=round(exp(h1$breaks),2))
	}
