#$Author: sinnwell $
#$Date: 2007/03/31 19:26:20 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.design.q,v 1.3 2007/03/31 19:26:20 sinnwell Exp $
#$Locker:  $
#$Log: haplo.design.q,v $
#Revision 1.3  2007/03/31 19:26:20  sinnwell
#error for col.rare empty, made an exception to fix it.
#
#Revision 1.2  2007/03/13 15:51:52  sinnwell
#change return obj back to data.frame, names work that way.
#
#Revision 1.1  2007/03/08 20:34:40  sinnwell
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


haplo.design <- function(obj, haplo.effect="additive",
                         hapcodes=NA,
                         min.count=5, haplo.base=NA) {

  # Set up a design matrix for haplotype effects
  #
  # 1. create a model matrix for all haplotypes in haplo.em object  
  # 2. expand by subject.id, weight by posterior probs of hap-pair, then collapse
  #    back down by subject.id
  # 3. select out the baseline
  # 4. remove columns not meeting minimum count

  # Input:
  # obj           haplo.em object
  #
  # hapcodes:     codes for haplo.em haps to make specific columns for
  # 
  # haplo.effect  the model matrix coding of haplotypes, additive, recessive, dominant
  #
  # haplo.base    the baseline haplotype
  #
  # min.count     the minimum count in the sample of the haplotype
  #               for inclusion as a distinct model covariate

  # Check haplo.effect parameter, match to 3 options
  effCode <- charmatch(haplo.effect, c("additive", "dominant", "recessive"))
  if(is.na(effCode)) stop("Invalid haplo.effect")

  # CREATE HAPLOTYPE EFFECT COLUMNS
  uniqueHaplo <- sort(unique(c(obj$hap1code, obj$hap2code)))
  hap.colname <- paste("hap", uniqueHaplo, sep=".")

  # if hapcodes given, reduce haplotypes to make cols for
  if(!is.na(hapcodes[1])) {
    hapmatch <- match(hapcodes, uniqueHaplo)
    uniqueHaplo <- uniqueHaplo[hapmatch]
    hap.colname <- hap.colname[hapmatch]
  }
  
  # by default, set up as additive coding
  hap.mat <-  1 * outer(obj$hap1code, uniqueHaplo, "==") +
              1 * outer(obj$hap2code, uniqueHaplo, "==")

  if(effCode==2) {
     # translate to dominant effect
     hap.mat <- 1*(hap.mat > 0)
  }
     
  if(effCode==3){    
     # translate to recessive effect
     hap.mat <- (hap.mat > 1) * 1
  }
  
  # weight by posteriors, collapse over subjects 
  nc <- ncol(hap.mat)
  effect.mat <- NULL
  for(j in 1:nc){
     effect.mat <- cbind(effect.mat, tapply(hap.mat[,j] * obj$post, obj$subj.id, sum) )
  }

  # if hapcodes not given, do other selection steps
  if(is.na(hapcodes[1])) {

    # if baseline is given, remove from model matrix
    # otherwise, keep all haplotype effects, leaving user to select out

    if(!is.na(haplo.base)) {    
      col.base <- which(uniqueHaplo == haplo.base)
      effect.mat <- effect.mat[, -col.base, drop=FALSE]
      hap.colname <- hap.colname[-col.base]
    }
    
  # select out columns not having minimum count
    if(min.count > 0) {
      col.sum <- apply(effect.mat, 2, sum)
      col.rare <- which(col.sum < min.count)
      if(length(col.rare)) {
        effect.mat <- effect.mat[,-col.rare, drop=FALSE]
        hap.colname <- hap.colname[-col.rare]
      }
    }
  }

  if(length(hap.colname)==0) stop("Design matrix has no columns")
  effect.mat <- as.data.frame(effect.mat)
  names(effect.mat) <- hap.colname
  
  return(effect.mat)

}
