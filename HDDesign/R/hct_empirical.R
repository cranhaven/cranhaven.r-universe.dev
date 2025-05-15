hct_empirical <-
function(pvalue, p, n)
{
	
	j=1:length(pvalue)
	jp=j/p
	k=which.max((jp-pvalue)/sqrt(jp*(1-jp)))
	pvalue[k]
}
