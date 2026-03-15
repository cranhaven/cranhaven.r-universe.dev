#$Author: sinnwell $
#$Date: 2008/02/28 21:38:19 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/chisq.power.q,v 1.2 2008/02/28 21:38:19 sinnwell Exp $
#$Locker:  $
#$Log: chisq.power.q,v $
#Revision 1.2  2008/02/28 21:38:19  sinnwell
#take out comments of old code that checked for no root from uniroot
#
#Revision 1.1  2008/02/28 15:59:27  sinnwell
#Initial revision
#

chisq.power <- function(n, nc, df, alpha)
{

   # power based on sample size n, non-centrality nc,
   # degrees of freedom df, and type-I error alpha

   1 - pchisq(qchisq(1-alpha, df), df, n*nc)
}

chisq.power.dif<- function(n, nc, df, alpha, power)
{
  # difference from target power -- for finding sample size
  p.dif <- chisq.power(n, nc, df, alpha) - power
  return(p.dif)

} 

chisq.sample.size <- function(nc, df=df, alpha, power, lower=20, upper=100000)
{
   # sample size from noncentrality nc, degrees of freedom df

  save.uniroot <- uniroot(chisq.power.dif, lower=lower, upper=upper,
		 nc=nc, df=df, alpha=alpha, power=power,
		 tol=0.0001)
  
  ss <- ceiling(save.uniroot$root)

  return(ss)
 }
