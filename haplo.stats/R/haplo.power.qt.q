#$Author: sinnwell $
#$Date: 2008/02/28 15:59:27 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.power.qt.q,v 1.1 2008/02/28 15:59:27 sinnwell Exp $
#$Locker:  $
#$Log: haplo.power.qt.q,v $
#Revision 1.1  2008/02/28 15:59:27  sinnwell
#Initial revision
#


haplo.power.qt <- function(haplo, haplo.freq, base.index, haplo.beta,
                           y.mu, y.var, alpha, sample.size=NULL, power=NULL) {

  
  if(is.null(power) & is.null(sample.size))
    {
      stop("Must specify either power or sample.size")
    }
  

  if(!is.null(power) & !is.null(sample.size))
    {
      stop("Must specify only one of power or sample.size")
    }

  
  ncp <- haplo.power.qt.ncp(haplo=haplo, haplo.freq=haplo.freq, base.index=base.index,
                      haplo.beta=haplo.beta, y.mu=y.mu, y.var=y.var)


  if(is.null(sample.size) & !is.null(power))
    {
      ss.phased.haplo   <- f.sample.size(nc=ncp$ncp.f.phased.haplo, df1=ncp$df,  alpha=alpha, power=power)
      ss.unphased.haplo <- f.sample.size(nc=ncp$ncp.f.unphased.haplo, df1=ncp$df, alpha=alpha, power=power)
      power.phased.haplo   <- power
      power.unphased.haplo <- power
    }


    if(is.null(power) & !is.null(sample.size))
    {
      power.phased.haplo   <- f.power(n=sample.size, nc=ncp$ncp.f.phased.haplo,df1=ncp$df, alpha=alpha)
      power.unphased.haplo <- f.power(n=sample.size, nc=ncp$ncp.f.unphased.haplo,df1=ncp$df, alpha=alpha)
      ss.phased.haplo   <- sample.size
      ss.unphased.haplo <- sample.size
    }

  return(list(ss.phased.haplo=ss.phased.haplo,
              ss.unphased.haplo=ss.unphased.haplo,
              power.phased.haplo=power.phased.haplo,
              power.unphased.haplo=power.unphased.haplo))
}
