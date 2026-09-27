#$Author: sinnwell $
#$Date: 2011/11/10 15:29:40 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.glm.control.q,v 1.8 2011/11/10 15:29:40 sinnwell Exp $
#$Locker:  $
#$Log: haplo.glm.control.q,v $
#Revision 1.8  2011/11/10 15:29:40  sinnwell
#major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
#
#Revision 1.7  2008/04/04 15:56:23  sinnwell
#fix typo of haplo.min.count
#
#Revision 1.6  2008/04/04 15:48:18  sinnwell
#enforce haplo.freq.min as .01, and override haplo.min.count
#
#Revision 1.5  2008/04/01 21:23:40  sinnwell
#manage haplo.freq.min, which is overriden by haplo.min.count
#
#Revision 1.4  2005/01/04 17:36:47  sinnwell
#use haplo.min.count as more important than haplo.freq.min
#
#Revision 1.3  2004/03/02 16:34:07  sinnwell
#change T to TRUE
#
#Revision 1.2  2003/12/08 19:37:28  sinnwell
# changed F,T to FALSE,TRUE
#
#Revision 1.1  2003/09/16 16:01:29  schaid
#Initial revision
#
haplo.glm.control <- function(haplo.effect="add",
                              haplo.base = NULL,
                              haplo.min.count=NA,
                              haplo.freq.min=.01,
                              sum.rare.min=0.001,
                              haplo.min.info=0.001,
                              keep.rare.haplo=TRUE,
                              eps.svd=sqrt(.Machine$double.eps),
                              glm.c=glm.control(maxit=500),
                              em.c=haplo.em.control()){


  chk <- charmatch(haplo.effect, c("additive", "dominant", "recessive"))
  if(is.na(chk)) stop("Invalid haplo.effect")

  if(haplo.min.info < 0 | haplo.min.info > .9) {
    warning("The value of haplo.min.info is out of range, the default value of 0.001 is used instead")
    haplo.min.info <- 0.001
  }
  
  # 1/2005 JPS
  # enourage the use of selecting haplotypes to model by a minimum expected count of 5
  if(!is.null(match.call()$haplo.freq.min)) {
    if(haplo.freq.min < haplo.min.info | haplo.freq.min >= 1) {
      warning("invalid value for haplo.freq.min, setting to default of .01")
      haplo.freq.min <- .01
    }

    if(!is.null(match.call()$haplo.min.count)) {
      warning("Both control parameters haplo.freq.min and haplo.min.count given; haplo.freq.min will be used")
      haplo.min.count <- NA
    }

  } else {
    if(!is.na(haplo.min.count)) {
      haplo.freq.min <- NA
      if(haplo.min.count <= 1) {
        warning("The value of haplo.min.count is too small, the count will default to 5/(2*n.subjects)")
        haplo.freq.min <- NA
        haplo.min.count <- 5
      }
    }
  }
    
  if(sum.rare.min < 0 | sum.rare.min > .9) {
    warning("The value of sum.rare.min is out of range, the default value of 0.001 is used instead")
    sum.rare.min <- 0.001
  }

  
  if(keep.rare.haplo!=TRUE & keep.rare.haplo!=FALSE){
    warning("The value of keep.rare.haplo is invalid, the default value of TRUE is used instead")
    keep.rare.haplo=TRUE
  }

  if(eps.svd < .Machine$double.eps || eps.svd > 0.1) {
    warning(paste("The value of eps.svd is out of range, the default of ",
                  sqrt(.Machine$double.eps), " is being used.", sep=''))
    eps.svd <- sqrt(.Machine$double.eps)
  }
  
  return(list(haplo.effect=haplo.effect,
              haplo.base = haplo.base,
              haplo.min.count=haplo.min.count,
              haplo.freq.min=haplo.freq.min,
              sum.rare.min=sum.rare.min,
              haplo.min.info=haplo.min.info,
              keep.rare.haplo=keep.rare.haplo,
              epsilon=glm.c$epsilon,
              eps.svd=eps.svd,
              maxit=glm.c$maxit,
              trace=glm.c$trace,
              em.control=em.c))
}

