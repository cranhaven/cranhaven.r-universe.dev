#$Author: sinnwell $
#$Date: 2008/03/10 19:01:28 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/get.hapPair.q,v 1.1 2008/03/10 19:01:28 sinnwell Exp $
#$Locker:  $
#$Log: get.hapPair.q,v $
#Revision 1.1  2008/03/10 19:01:28  sinnwell
#Initial revision
#

# get list for haplotype-pairs, or phased genotypes, under HWE
# return: 1) p.g:     P(geno|hfreq, HWE)
#         2) x.haplo: design matrix for haplotype pairs (additive)
#         3) haplo.indx: index of hap pairs, referring to rows of haplo

get.hapPair <- function(haplo, haplo.freq, base.index) {

   n.loci <- ncol(haplo)   
   n.haplo  <- nrow(haplo)
   
   haplo.indx <- expand.grid(1:n.haplo,1:n.haplo)
   haplo.indx <- cbind(haplo.indx[,2],haplo.indx[,1])
   haplo.indx <- haplo.indx[haplo.indx[,1] <= haplo.indx[,2],]

   # Set up regression design matrices and beta coeff vectors

   # Design matrix using base.index as baseline, and assuming
   # haplotype effects are additive

   haplo.reg <- (1:n.haplo)[-base.index]
   x.haplo <- 1*outer(haplo.indx[,1], haplo.reg,"==") +  1*outer(haplo.indx[,2], haplo.reg,"==")


  # Compute prior genotype probs (geno = pair of haplotypes)

   p.g <-  haplo.freq[haplo.indx[,1]] * haplo.freq[haplo.indx[,2]]
   p.g <-  p.g * ifelse(haplo.indx[,1] == haplo.indx[,2], 1, 2)

   return(list(p.g=p.g, x.haplo=x.haplo, haplo.indx=haplo.indx))
}
