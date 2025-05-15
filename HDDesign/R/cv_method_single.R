cv_method_single <-
function(mu0, p, m, n, alpha_list, p1, ss, sampling.p=0.5)
{
	data=gen_data(mu0, p, m, n, p1,sampling.p)

	d1=subset(data$x, data$y==1)
	d2=subset(data$x, data$y==-1)
	n1=nrow(d1)
	n2=nrow(d2)

	cv_data=data

	# 10 fold CV
     K=10 #10 fold CV  unless one group has less than 10, then smaller fold:
     if(min(n1,n2)<10) K=min(n1,n2) 

     cv_ids1= sample(rep(1:K, ceiling(n1/K))[1:n1], n1, replace=F)
     cv_ids2= sample(rep(1:K, ceiling(n2/K))[1:n2], n2, replace=F)


	cv_data$cv_idx=c( cv_ids1,  cv_ids2)

	cv_pcc=sapply(alpha_list, get_cv_pcc, cv_data, p1)
	pvalue_threshold=alpha_list[which.max(cv_pcc)]
 
	mean1=apply(d1,2,mean)
	mean2=apply(d2,2,mean)
	xsq1=apply(d1^2,2,sum)
	xsq2=apply(d2^2,2,sum)
	s1= xsq1-n1*mean1^2
	s2= xsq2-n2*mean2^2
	pool.sd= sqrt((s1+s2)/(n-2))
	zscore= (mean1-mean2)/( pool.sd*sqrt(1/n1+1/n2))
	pvalue=2*pt(abs(zscore),df=n-2, lower.tail=F)
	weight=get_weight_zp(zscore, pvalue, pvalue_threshold)

        #below is a short cut to estimate PCC so that at the design stage we don't have to generate test data 
        #in analysis, this would have to be replaced by a test data (see cv_method_single_analysis)

	u=sum(weight[1:m])*mu0
	v=sum(abs(weight))
	get_pcc_ss(u, v, p1, ss)	

}

