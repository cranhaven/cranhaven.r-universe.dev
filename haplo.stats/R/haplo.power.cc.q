#$Author: sinnwell $
#$Date: 2008/02/28 15:59:27 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.power.cc.q,v 1.1 2008/02/28 15:59:27 sinnwell Exp $
#$Locker:  $
#$Log: haplo.power.cc.q,v $
#Revision 1.1  2008/02/28 15:59:27  sinnwell
#Initial revision
#


haplo.power.cc <- function(haplo, haplo.freq, base.index, haplo.beta, case.frac,
                           prevalence, alpha, sample.size=NULL, power=NULL){
   

  if(is.null(power) & is.null(sample.size))
    {
      stop("Must specify either power or sample.size")
    }
  

  if(!is.null(power) & !is.null(sample.size))
    {
      stop("Must specify only one of power or sample.size")
    }
  
  
  ncp <- haplo.power.cc.ncp(haplo, haplo.freq, base.index, haplo.beta, case.frac, prevalence)
      

  if(is.null(sample.size) & !is.null(power))
    {
      ss.phased.haplo    <- chisq.sample.size(nc=ncp$ncp.phased.haplo, df=ncp$df.phased.haplo, alpha=alpha, power=power)
      ss.unphased.haplo  <- chisq.sample.size(nc=ncp$ncp.unphased.haplo, df=ncp$df.unphased.haplo, alpha=alpha, power=power)
      power.phased.haplo   <- power
      power.unphased.haplo <- power
    }


  if(is.null(power) & !is.null(sample.size))
    {
      power.phased.haplo   <- chisq.power(n=sample.size, nc=ncp$ncp.phased.haplo,  df=ncp$df.phased.haplo, alpha=alpha)
      power.unphased.haplo <- chisq.power(n=sample.size, nc=ncp$ncp.unphased.haplo, df=ncp$df.unphased.haplo, alpha=alpha)
      ss.phased.haplo   <- sample.size
      ss.unphased.haplo <- sample.size
    }

  
  return(list(ss.phased.haplo=ss.phased.haplo,
              ss.unphased.haplo=ss.unphased.haplo,
              power.phased.haplo=power.phased.haplo,
              power.unphased.haplo=power.unphased.haplo))
  
}

