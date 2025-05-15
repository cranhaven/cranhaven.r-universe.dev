get_cv_pcc_single <-
function(idx, alpha, cv_data, p1)
{
	data=select_cv_data(cv_data, cv_data$cv_idx!=idx)


	d1=subset(data$x, data$y==1)
	d2=subset(data$x, data$y==-1)
	n1=nrow(d1)
	n2=nrow(d2)
n=n1+n2
	mean1=apply(d1,2,mean)
	mean2=apply(d2,2,mean)
	xsq1=apply(d1^2,2,sum)
	xsq2=apply(d2^2,2,sum)
	s1= xsq1-n1*mean1^2
	s2= xsq2-n2*mean2^2
	pool.sd= sqrt((s1+s2)/(n-2))
    	pool.var= (s1+s2)/(n-2)  
	zscore= (mean1-mean2)/( pool.sd*sqrt(1/n1+1/n2))
	pvalue=2*pt(abs(zscore),df=n-2, lower.tail=F)
	weight=get_weight_zp(zscore, pvalue, alpha)

	test_data=select_cv_data(cv_data, cv_data$cv_idx==idx) 
	if (all(weight==0))
	{
		pred=rbinom(length(test_data$y), size=1, prob=p1) 
	}
	else
	{
           mu0hat= dot( abs(weight), abs((mean1-mean2))/2) / (dot(weight, weight))
           av.pool.var=mean( pool.var)

           cl_cutoff= (1/2) * log((1-p1)/p1) * (av.pool.var/mu0hat)
           pred=as.numeric((test_data$x %*% weight)>cl_cutoff)  
	}
	
	# convert (0,1) to (-1, 1)
	pred=pred*2-1
	sum(pred==test_data$y) 
}

