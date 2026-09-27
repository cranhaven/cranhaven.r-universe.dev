#$Author: sinnwell $
#$Date: 2008/02/28 15:59:27 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/f.power.q,v 1.1 2008/02/28 15:59:27 sinnwell Exp $
#$Locker:  $
#$Log: f.power.q,v $
#Revision 1.1  2008/02/28 15:59:27  sinnwell
#Initial revision
#
f.power <- function(n, nc, df1, alpha){

   # power based on sample size n, non-centrality nc,
   # degrees of freedom df1 and df2, and type-I error alpha

   df2 <- n - df1 -1
   if(df2 < 0) df2 <- 1

   1 - pf(qf(1-alpha, df1, df2), df1, df2, n*nc)
 }



f.power.dif <- function(n, nc, df1, alpha, power) {
   # difference from target power -- for finding sample size
  p.dif <- f.power(n, nc, df1, alpha) - power
  return(p.dif)
}


f.sample.size <- function(nc, df1, alpha, power,
		lower=20, upper=10000){
   # sample size from noncentrality nc, degrees of freedom df
   root <- ceiling(uniroot(f.power.dif, lower=lower, upper=upper,
                          nc=nc, df1=df1, alpha=alpha, power=power,
                          tol=0.0001)$root)
   return(root)
 }

