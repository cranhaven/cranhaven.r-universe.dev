r<-function(tab,eps=eps,maxiter=100)
{
# Calculation of rm
	R0=sum(tab$lx*tab$mx)
	Tc=sum(tab$x*tab$lx*tab$mx)
	r0=R0/Tc
	r=r0
	f=function(r,x,lx,mx) 1-sum(exp(-r*x)*lx*mx)
	fprim=function(r,x,lx,mx) sum(x*exp(-r*x)*lx*mx)
	# Newton method
iter=1
	while (abs(f(r,tab$x,tab$lx,tab$mx))>=eps & iter < maxiter) 
		{
		u=f(r,tab$x,tab$lx,tab$mx)
		v=fprim(r,tab$x,tab$lx,tab$mx)
		r=r-u/v
		# cat("iter = ",iter," r = ",r,"\n")
		iter=iter +1
		}
r
}
