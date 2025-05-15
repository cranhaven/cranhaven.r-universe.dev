gen_data_corr <-
function(mu0, p, m, n, pcorr, chol.rho, sampling.p=0.5)
{
  n1=ceiling(n*sampling.p)
  n2=n-n1

  part.left.mean.vector= c(  rep(mu0,m), rep(0, pcorr-m) )

	part1=rnorm(pcorr*n1, mean=0, sd=1)
	part1=matrix(part1, nrow=n1, ncol=pcorr)%*% chol.rho + rep(1,n1) %*%  t(part.left.mean.vector) 
	part2=rnorm(pcorr*(n2), mean=0, sd=1)
	part2=matrix(part2, nrow=n2, ncol=pcorr)%*% chol.rho -  rep(1,n2) %*% t(part.left.mean.vector) 
	part_left=rbind(part1, part2)

	part_right=rnorm((p-pcorr)*n, mean=0, sd=1)
	part_right=matrix(part_right, nrow=n, ncol=p-pcorr)
	
	y=c(rep(1, n1), rep(-1, n2))
	list(x=cbind(part_left, part_right), y=y)
}
