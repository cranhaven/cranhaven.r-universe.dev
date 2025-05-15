gen_data <-
function(mu0, p, m, n,p1=0.5,sampling.p=0.5)
{
  n1=ceiling(n*sampling.p)
  n2=n-n1

	part1=rnorm(m*n1, mean=mu0, sd=1)
	part1=matrix(part1, nrow=n1, ncol=m)
	part2=rnorm(m*n2, mean=-mu0, sd=1)
	part2=matrix(part2, nrow=n2, ncol=m)
	part_left=rbind(part1, part2)

	part_right=rnorm((p-m)*n, mean=0, sd=1)
	part_right=matrix(part_right, nrow=n, ncol=p-m)
	
	y=c(rep(1, n1), rep(-1, n2))
	list(x=cbind(part_left, part_right), y=y)
}
