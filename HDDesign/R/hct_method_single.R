hct_method_single <-
function(dummy, mu0, p, m, n, hct, alpha0, p1, ss, sampling.p)
{
  n1=ceiling(n*sampling.p)
  n2=n-n1
  tau=2*mu0/(sqrt(1/n1+1/n2)) 

	zscore_alt=rt(m, df=n-2, ncp=tau)
	pvalue_alt=2*pt(abs(zscore_alt),df=n-2, lower.tail=F)

	pvalue_null=gen_sim_pvalue_null(p, m, alpha0)

	pvalue=c(pvalue_alt, pvalue_null)
	pvalue=sort(pvalue, method='quick')[1:ceiling(p*alpha0)]

	pvalue_threshold=hct(pvalue, p, n)
	u=(sum(zscore_alt>=0 & pvalue_alt<=pvalue_threshold)-sum(zscore_alt < 0 & pvalue_alt<=pvalue_threshold))*mu0
	v=sum(pvalue<=pvalue_threshold)

	get_pcc_ss(u, v, p1, ss)	
}

