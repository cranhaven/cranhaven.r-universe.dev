#$Author: sinnwell $
#$Date: 2011/11/22 20:29:04 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/summaryGeno.q,v 1.3 2011/11/22 20:29:04 sinnwell Exp $
#$Locker:  $
#$Log: summaryGeno.q,v $
#Revision 1.3  2011/11/22 20:29:04  sinnwell
#use setupGeno instead of geno.recode
#
#Revision 1.2  2008/01/04 21:18:59  sinnwell
#change column names of returned data.frame
#
#Revision 1.1  2004/02/26 22:07:13  sinnwell
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

summaryGeno <- function(geno, miss.val=0)
## created by Jason Sinnwell  
## Division of Biostatistics
## Mayo Clinic Rochester
## Genetics Team   11/2002

{

  # Get allele tallies for each individual.  Keep track of 0- 1- and 2-missing
  # alleles.  Also, find the rows needed to fully enumerate the
  # individual's possible haplotypes.  
  
  n.loci <- ncol(geno)/2
  nr <- nrow(geno)
  geno <- setupGeno(geno,miss.val)
  
  count.geno <- geno.count.pairs(geno)

  loc0 <- numeric(nr)
  loc1 <- numeric(nr)
  loc2 <- numeric(nr)
  for (i in 1:nr) {
    
    first.indx <- seq(1,(2*n.loci-1),by=2)  
    miss.one <- is.na(geno[i,first.indx]) | is.na(geno[i,first.indx+1])
    miss.two <- is.na(geno[i,first.indx]) & is.na(geno[i,first.indx+1])
    loc2[i] <- sum(miss.two)
    loc1[i] <- sum(miss.one-miss.two)
    loc0[i] <- sum(!miss.one)
  }

  tbl <- data.frame(missing0=loc0, missing1=loc1, missing2=loc2,
                    N.enum.rows=count.geno)

  return(tbl)
}

