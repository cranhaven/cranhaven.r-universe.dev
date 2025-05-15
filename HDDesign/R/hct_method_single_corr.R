hct_method_single_corr <-
function(dummy, mu0, p, m, n, hct, alpha0, p1, ss, pcorr, chol.rho, sampling.p)
{
  	n1=ceiling(n*sampling.p)
  	n2=n-n1
 
#generate data for correlated features only, and thus get correlated pvalues for important features
	part.left.mean.vector= c(  rep(mu0,m), rep(0, pcorr-m) )

	part1=rnorm(pcorr*n1, mean=0, sd=1)
	part1=matrix(part1, nrow=n1, ncol=pcorr)%*% chol.rho + rep(1,n1) %*%  t(part.left.mean.vector) 
	part2=rnorm(pcorr*(n2), mean=0, sd=1)
	part2=matrix(part2, nrow=n2, ncol=pcorr)%*% chol.rho -  rep(1,n2) %*% t(part.left.mean.vector) 
 
	y=c(rep(1, n1), rep(-1, n2))
 
	d1=part1
	d2=part2
	n1=nrow(d1)
	n2=nrow(d2)
	mean1=apply(d1,2,mean)
	mean2=apply(d2,2,mean)
	xsq1=apply(d1^2,2,sum)
	xsq2=apply(d2^2,2,sum)
	s1= xsq1-n1*mean1^2
	s2= xsq2-n2*mean2^2
	pool.sd= sqrt((s1+s2)/(n-2))
	zscore_alt= (mean1-mean2)/( pool.sd*sqrt(1/n1+1/n2))
	pvalue_alt=2*pt(abs(zscore_alt),df=n-2, lower.tail=F)

	pvalue_null=runif(p-pcorr)
     zscore_null=qt(pvalue_null/2, df=n-2, ncp=0, lower.tail = F, log.p = FALSE) #[(pcorr+1):null.p.length]

	pvalue=c(pvalue_alt, pvalue_null)  # [(pcorr+1):null.p.length])
	pvaluesmall=sort(pvalue, method='quick')[1:ceiling(p*alpha0)]

	pvalue_threshold=hct(pvaluesmall, p, n)
	u=(sum(zscore_alt[1:m]>=0 & pvalue_alt[1:m]<=pvalue_threshold)-sum(zscore_alt[1:m] < 0 & pvalue_alt[1:m]<=pvalue_threshold))*mu0
	v=sum(pvalue<=pvalue_threshold)

 
	weights=get_weight_zp(c(zscore_alt,zscore_null),pvalue,pvalue_threshold)

	new.v1=sum(  ( t(weights[1:pcorr])%*%t(chol.rho) )^2 )
	length.pvalue=length(pvalue)
	new.v2=sum( pvalue[(pcorr+1):length.pvalue]<=pvalue_threshold)  
	new.v=new.v1+new.v2

	get_pcc_ss(u, new.v, p1, ss)
}
