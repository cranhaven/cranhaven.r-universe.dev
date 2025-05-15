hct_hybrid <-
function(pvalue, p,n)
{
	pt(mean(c(qt(hct_empirical(pvalue, p)/2,df=n-2, lower.tail=F), qt(hct_beta(pvalue, p)/2,df=n-2, lower.tail=F))), df=n-2, lower.tail=F)*2
	#pnorm(mean(c(qt(hct_empirical(pvalue, p)/2,sd=1, lower.tail=F), qnorm(hct_beta(pvalue, p)/2,sd=1, lower.tail=F))), lower.tail=F)*2
}

