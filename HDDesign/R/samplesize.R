samplesize <-
function(target, nmin, nmax, f, ...)
{
	nmin=nmin/2
	nmax=nmax/2

	mid=round((nmin+nmax)/2)

	while (nmin!=mid && nmax!=mid)
	{
		if (f(n=2*mid, ...) >= target)
			nmax=mid
		else
			nmin=mid
		mid=round((nmin+nmax)/2)
	}

	c(nmax*2, f(n=2*nmax, ...))
}


samplesize.uneven <-
function(target, nmin, nmax, f, ...)
{
	mid=round((nmin+nmax)/2)

	while (nmin!=mid && nmax!=mid)
	{
		if (f(n=mid, ...) >= target)
			nmax=mid
		else
			nmin=mid
		mid=round((nmin+nmax)/2)
	}

	c(nmax, f(n=nmax, ...))
}
