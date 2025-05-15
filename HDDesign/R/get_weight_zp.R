get_weight_zp <-
function(zscore, pvalue, pvalue_threshold)
{
	weight=rep(0, length(zscore))
	weight[pvalue<=pvalue_threshold & zscore>0]=1
	weight[pvalue<=pvalue_threshold & zscore<0]=-1
	weight
}
