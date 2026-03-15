#$Author: sinnwell $
#$Date: 2008/04/29 14:34:32 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/find.haplo.beta.qt.q,v 1.5 2008/04/29 14:34:32 sinnwell Exp $
#$Locker:  $
#$Log: find.haplo.beta.qt.q,v $
#Revision 1.5  2008/04/29 14:34:32  sinnwell
#T to TRUE
#
#Revision 1.4  2008/03/10 19:01:15  sinnwell
# call get.hapPair instead of re-used code
#
#Revision 1.3  2008/03/05 15:28:07  sinnwell
#rm useless code
#
#Revision 1.2  2008/02/28 21:49:27  sinnwell
#take out commented check uniroot$message for failure
#
#Revision 1.1  2008/02/28 15:59:27  sinnwell
#Initial revision
#
find.haplo.beta.qt <- function(haplo, haplo.freq, base.index, haplo.risk, r2, y.mu=0, y.var=1){


  # Find beta's for risk haplotypes, for specified r2

  root.save <- uniroot(find.beta.qt.phase.known, lower=0, upper=10,
                         tol = 0.000001,
                         haplo.risk=haplo.risk, base.index=base.index, 
                         haplo=haplo, haplo.freq=haplo.freq, r2=r2, y.mu=y.mu, y.var=y.var)

  beta.target <- root.save$root
  
  beta <- numeric(nrow(haplo))
  beta[haplo.risk] <- beta.target
  beta.no.intercept <- beta[-base.index]
  beta[base.index] <- find.intercept.qt.phase.known(beta.no.intercept, base.index,
                                                 haplo, haplo.freq, y.mu)

  return(list(r2=r2, beta=beta, base.index=base.index, haplo.risk=haplo.risk))

}


find.beta.qt.phase.known <- function(beta.size, haplo.risk, base.index,
                                  haplo, haplo.freq, r2, y.mu, y.var){

  # compute noncentrality parameter (ncp) for score stat of
  # haplotype effects for a quantitative trait, for both 
  # genotypes (phased haplotypes) and diplotypes (un-phased haplotypes)

  # Create matrix of indices for all possible pairs of haplotypes

   haplo <- as.matrix(haplo)
   n.loci <- ncol(haplo)   
   n.haplo  <- nrow(haplo)
   
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
   
   # Set up vector of beta's (leave intercept =0, since ignored)
   beta <- numeric(n.haplo)
   beta[haplo.risk] <- beta.size
 
   x.mu <- apply(x.haplo * p.g, 2, sum)


  # E[V] if no ambiguous haplotypes

  x.mu.mat <- matrix(rep(x.mu, nrow(x.haplo)), nrow=nrow(x.haplo), byrow=TRUE)
  delta.x <- x.haplo - x.mu.mat
  t2 <- delta.x * p.g
  vx.complete <- t(t2) %*% delta.x


  # Model R^2 for phase known haplotypes

   
   r2.phase.known <- (t(beta[-base.index]) %*% vx.complete %*% beta[-base.index])/y.var

   r2.diff <- r2.phase.known - r2

   
   return(r2.diff)

}

find.intercept.qt.phase.known <- function(beta.no.intercept, base.index,
                                       haplo, haplo.freq, y.mu){

   # Create matrix of indices for all possible pairs of haplotypes

   haplo <- as.matrix(haplo)
 #  n.loci <- ncol(haplo)   
 #  n.haplo  <- nrow(haplo)
   
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
   
   x.mu <- apply(hapPair.lst$x.haplo * hapPair.lst$p.g, 2, sum)

  # Find intercept (beta for base.index haplotype)
   
   beta0 <- y.mu - t(beta.no.intercept) %*% x.mu
 
   return(beta0)

}

