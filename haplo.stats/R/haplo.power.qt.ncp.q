#$Author: sinnwell $
#$Date: 2008/04/29 17:15:39 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.power.qt.ncp.q,v 1.4 2008/04/29 17:15:39 sinnwell Exp $
#$Locker:  $
#$Log: haplo.power.qt.ncp.q,v $
#Revision 1.4  2008/04/29 17:15:39  sinnwell
#F to FALSE
#
#Revision 1.3  2008/04/29 14:36:36  sinnwell
#T to TRUE
#
#Revision 1.2  2008/03/10 19:01:07  sinnwell
# call get.hapPair instead of re-used code
#
#Revision 1.1  2008/02/28 15:59:27  sinnwell
#Initial revision
#

haplo.power.qt.ncp <- function(haplo, haplo.freq, base.index,
                               haplo.beta, y.mu, y.var){

 # compute noncentrality parameter (ncp) for score stat of
 # haplotype effects for a quantitative trait, for both 
 # phased haplotypes  and un-phased haplotypes

  # Create matrix of indices for all possible pairs of haplotypes

   haplo <- as.matrix(haplo)
   n.loci <- ncol(haplo)

#   haplo.indx <- expand.grid(1:n.haplo,1:n.haplo)
#   haplo.indx <- cbind(haplo.indx[,2],haplo.indx[,1])
#   haplo.indx <- haplo.indx[haplo.indx[,1] <= haplo.indx[,2],]

   # Set up regression design matrices and beta coeff vectors

   # Design matrix using base.index as baseline, and assuming
   # haplotype effects are additive

#   haplo.reg <- (1:n.haplo)[-base.index]
#   x.haplo <- 1*outer(haplo.indx[,1], haplo.reg,"==") +  1*outer(haplo.indx[,2], haplo.reg,"==")
 
  # Compute prior genotype probs (geno = pair of haplotypes)
#   p.g <-  haplo.freq[haplo.indx[,1]] * haplo.freq[haplo.indx[,2]]
#   p.g <-  p.g * ifelse(haplo.indx[,1] == haplo.indx[,2], 1, 2)

   
   hapPair.lst <- get.hapPair(haplo, haplo.freq, base.index)
   p.g <- hapPair.lst$p.g
   x.haplo <- hapPair.lst$x.haplo

   haplo.indx <- hapPair.lst$haplo.indx
   
   # number of degrees of freedom; excludes intercept
   df <- ncol(x.haplo)
   
   x.mu <- apply(x.haplo * p.g, 2, sum)

  # E[V] if no ambiguous haplotypes

  x.mu.mat <- matrix(rep(x.mu, nrow(x.haplo)), nrow=nrow(x.haplo), byrow=TRUE)
  delta.x <- x.haplo - x.mu.mat
  t2 <- delta.x * p.g
  vx.complete <- t(t2) %*% delta.x

  # Genotype matrix, ignoring haplotype phase information. 

  geno.mat <- NULL
  for(i in 1:n.loci){
    t1 <- haplo[haplo.indx[,1],i]
    t2 <- haplo[haplo.indx[,2],i]
    a1 <- ifelse(t1 < t2, t1, t2)
    a2 <- ifelse(t2 > t1, t2, t1)
    geno.mat <- cbind(geno.mat, a1, a2)
  }

  # create haplo.group which gives a code for pairs of
  # haplotypes that fall into the same genotype group when phase is unk

  geno.hash <- haplo.hash(geno.mat)

  haplo.group <- geno.hash$hash

  # order by haplo.group

  ord <- order(haplo.group)
  p.g <- p.g[ord]
  haplo.group <- haplo.group[ord]
  x.haplo <- x.haplo[ord,]


  # create posterior probabilities of haplotype pairs, 
  # conditional on haplo.group

  p.haplo.group <- tapply(p.g, haplo.group, sum)
  nrep <- tapply(haplo.group, haplo.group, length)
  denom <- rep(p.haplo.group, nrep)
  post <- p.g/denom


  # E[V] matrix and E[U] vector

  n.group <- length(unique(haplo.group))
  nx <- ncol(x.haplo)
  vx.incomplete <- matrix(numeric(nx^2), nrow=nx)

  for(i in 1:n.group){
    zed <- (haplo.group == i)
    tmp.x.mu <- as.vector(apply(x.haplo[zed,,drop=FALSE] * post[zed], 2, sum))
    tmp.delta <- (tmp.x.mu - x.mu)
    vx.incomplete <- vx.incomplete + ( (tmp.delta %o% tmp.delta) * p.haplo.group[i])
  }


  # Model R^2 for incomplete and complete observed haplotypes


  r2.phase.unknown <- (t(haplo.beta[-base.index]) %*% vx.incomplete %*% haplo.beta[-base.index])/y.var

  r2.phase.known   <- (t(haplo.beta[-base.index]) %*% vx.complete %*% haplo.beta[-base.index])/y.var

  # NCP's

  ncp.chi.phase.known   <- r2.phase.known
  ncp.chi.phase.unknown <- r2.phase.unknown

  ncp.f.phase.known   <- r2.phase.known/(1 - r2.phase.known)
  ncp.f.phase.unknown <- r2.phase.unknown/(1 - r2.phase.unknown)

  return(list(ncp.chi.phased.haplo = ncp.chi.phase.known,
              ncp.f.phased.haplo = ncp.f.phase.known, 
              ncp.chi.unphased.haplo = ncp.chi.phase.unknown,
              ncp.f.unphased.haplo = ncp.f.phase.unknown, 
              df=df))
}
