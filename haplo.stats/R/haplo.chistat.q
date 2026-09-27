#$Author: sinnwell $
#$Date: 2008/04/01 20:55:08 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.chistat.q,v 1.3 2008/04/01 20:55:08 sinnwell Exp $
#$Locker:  $
#$Log: haplo.chistat.q,v $
#Revision 1.3  2008/04/01 20:55:08  sinnwell
#add eps to Ginv
#
#Revision 1.2  2005/03/28 22:28:02  sinnwell
#changed from chistat
#
#Revision 1.1  2005/03/23 18:07:08  sinnwell
#Initial revision
#

haplo.chistat <- function(h1, h2, post, y, nrep) {
  # calculate a test statistic based on case/control status
  # based on the differences between mean haplotype frequency in 2 groups
  # it is like a chisquare stat, but distribution is not known,
    # b/c multiple testing occurs
  
  tbl <- table(y)
  n.c <- tbl[1]
  n.d <- tbl[2]
  n.total <- n.c + n.d

# set up the matrix for counts of unique haplotypes
  uhap <- sort(unique(c(h1,h2)))
  x <- 1*outer(h1, uhap, "==") +  1*outer(h2, uhap, "==")

  varx <- var(x)
  save.inv <- Ginv(varx, eps=sqrt(.Machine$double.eps))
  vinv <- save.inv$Ginv
  df <- save.inv$rank
  
  # repeat the group variable as many times as nreps,
  # to match posteriors
  yrep <- rep(y,nrep)
  
  # multiply each row by the posterior prob of the person's hap pairs
  # then find mean of haplotype counts in each group
  # the length is number of haps
  x.d <- apply(post[yrep==1] * x[yrep==1,], 2, mean)
  x.c <- apply(post[yrep==0] * x[yrep==0,], 2, mean)

  delta <- matrix(x.d - x.c, ncol=1)
 
  stat <- t(delta)%*%vinv%*%delta * (n.d*n.c/n.total)

  return(stat)
}

