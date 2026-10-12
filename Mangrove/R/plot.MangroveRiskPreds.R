plot.MangroveRiskPreds <-
function(x,...) {
	temp <- log(x)
	h1 <- hist(temp,axes=F,xlab="log(OR)",main="Histogram of log risks",col="grey",...)
	axis(2)
	axis(1,at=h1$breaks,labels=round(exp(h1$breaks),2))
	}

