hct_beta <-
function(pvalue, p, n)
{

	j=1:length(pvalue)
	nj1=p+1-j
	beta_pvalue=apply(cbind(pvalue, j, nj1), 1, beta_prob)
	k=which.max(beta_pvalue)
	pvalue[k]


}
