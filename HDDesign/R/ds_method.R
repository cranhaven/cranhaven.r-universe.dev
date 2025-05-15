ds_method <-
function(mu0, p, m, n, p1=0.5, lmax=1, ss=F,sampling.p=0.5)
{
	temp=optimize(get_ds_pcc_alpha, interval=c(-80, -0.001), mu0=mu0, p=p, m=m, n=n, p1=p1, lmax=lmax, ss=F, sampling.p=sampling.p, maximum=T)
	get_ds_pcc_alpha(temp$maximum, mu0=mu0, p=p, m=m, n=n, p1=p1, lmax=lmax, ss=ss, sampling.p=sampling.p) 
}


 
