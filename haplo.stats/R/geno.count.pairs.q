#$Author: sinnwell $
#$Date: 2003/12/08 19:33:33 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/geno.count.pairs.q,v 1.5 2003/12/08 19:33:33 sinnwell Exp $
#$Locker:  $
#$Log: geno.count.pairs.q,v $
#Revision 1.5  2003/12/08 19:33:33  sinnwell
#changed F to FALSE
#
#Revision 1.4  2003/08/26 16:39:04  sinnwell
#change license statement
#
#Revision 1.3  2003/04/14 17:23:00  schaid
#when no missing alleles, added drop=F for h1 and h2, for when there is only one subject without missing data
#
#Revision 1.2  2003/04/11 19:30:19  sinnwell
#move comments around
#
#Revision 1.1  2003/04/09 18:33:09  sinnwell
#Initial revision
#

# License: 
# 
# Copyright 2003 Mayo Foundation for Medical Education and Research. 
# 
# This program is free software; you can redistribute it and/or modify it under the terms of 
# the GNU General Public License as published by the Free Software Foundation; either 
# version 2 of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or 
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
# more details.
# 
# You should have received a copy of the GNU General Public License along with this 
# program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, 
# Boston, MA 02111-1307 USA
# 
# For other licensing arrangements, please contact Daniel J. Schaid.
# 
# Daniel J. Schaid, Ph.D.
# Division of Biostatistics
# Harwick Building Room 775
# Mayo Clinic
# 200 First St., SW
# Rochester, MN 55905
# 
# phone: 507-284-0639
# fax:      507-284-9542
# email: schaid@mayo.edu
# 

geno.count.pairs <- function(geno)
##  Developed by Schaid, DJ 4/2003
{

# Given a matrix of genotypes (pairs of columns for each locus),
# compute number of possible pairs of haplotypes for each subject (the rows
# of the geno matrix).
  
# Assume missing values are all coded to NA, done easily by geno.recode()

# define function to iteratively update number of hets
# across loci, accounting for all possible combinations
# of missing alleles across loci
  
  haplo.count.pairs <- function(nhet.vec,nhom.vec){

    n.loci <- length(nhet.vec)
    cum.het <- 0
    wt.het <- 1

    for(locus in 1:n.loci){
      n.het <- nhet.vec[locus]
      n.hom <- nhom.vec[locus]
      
      if(n.het>=1 & n.hom>=1){
        cum.het <- as.vector(rbind(cum.het+1, cum.het))
        wt.het  <- as.vector(rbind(wt.het*n.het, wt.het*n.hom))
      }

      if(n.het>=1 & n.hom==0){
        cum.het <- cum.het + 1
        wt.het <- wt.het * n.het
      }
    }
    
    cum.het <- ifelse(cum.het==0,1,cum.het) # patch in case no hets
    n.pairs <- sum(2^(cum.het-1)*wt.het)
    return(n.pairs)
  }

 # initial setup for counting pairs of haplotypes
 # assume missing values are all coded to NA
  
  nc <- ncol(geno)
  n.loci <- nc/2
  nr <- nrow(geno)
  n.pairs <- numeric(nr)
  odd <- seq(from=1,to=2*n.loci,by=2)
  
 # find number of alleles at each locus
  
  n.alleles <- numeric(n.loci)
  for (i in 1:n.loci) {
     a1 <- geno[,2*i-1]
     a2 <- geno[,2*i]
     temp <- c(a1,a2)
     temp <- temp[!is.na(temp)]
     n.alleles[i] <- length(unique(temp))
   }

 # count pairs of haplotypes for subjects without any missing alleles

  any.miss <- apply(is.na(geno),1,any)

  if(sum(!any.miss) > 0){
    indx.nomiss <- (1:nr)[!any.miss]
    geno.nomiss <- geno[indx.nomiss,,drop=FALSE]
    h1 <- geno.nomiss[,odd,drop=FALSE]
    h2 <- geno.nomiss[,(odd+1),drop=FALSE]
    n.het <- apply(h1!=h2,1,sum)
    n.het <- ifelse(n.het==0,1,n.het)
    n.pairs[indx.nomiss] <- 2^(n.het-1)
  }

 # count pairs for subjects with missing alleles

  if(sum(any.miss) > 0){

    indx.miss <- (1:nr)[any.miss]
    n.subj.miss <- length(indx.miss)
    geno.miss <- geno[indx.miss,,drop=FALSE]

  # count number of possible het, hom at each locus
    loc <- 0
    het.mat <- hom.mat <- NULL

    for(i in odd){
      a1 <- geno.miss[,i]
      a2 <- geno.miss[,(i+1)]
      loc <- loc+1
      n.a <- n.alleles[loc]
      n.miss <- 1*(is.na(a1)) + 1*(is.na(a2))
      n.het <- ifelse(a1!=a2,1,0)
      n.het <- ifelse(n.miss==1,(n.a-1), n.het)
      n.het <- ifelse(n.miss==2,n.a*(n.a-1)/2, n.het)

      n.hom <- ifelse(a1==a2,1,0)
      n.hom <- ifelse(n.miss==1,1,n.hom)
      n.hom <- ifelse(n.miss==2,n.a,n.hom)
      
      het.mat <- cbind(het.mat,n.het)
      hom.mat <- cbind(hom.mat,n.hom)
    }

    pairs.miss <- NULL
    for(i in 1:n.subj.miss){
      pairs.miss <- c(pairs.miss, haplo.count.pairs(het.mat[i,], hom.mat[i,]))
    }

    n.pairs[indx.miss] <- pairs.miss
  }

  return(n.pairs)
}


