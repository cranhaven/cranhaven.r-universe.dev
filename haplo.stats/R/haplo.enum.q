#$Author: sinnwell $
#
#$Date: 2003/08/26 16:39:04 $
#
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.enum.q,v 1.4 2003/08/26 16:39:04 sinnwell Exp $
#
#$Id: haplo.enum.q,v 1.4 2003/08/26 16:39:04 sinnwell Exp $
#
#$Locker:  $
#
#$Log: haplo.enum.q,v $
#Revision 1.4  2003/08/26 16:39:04  sinnwell
#change license statement
#
#Revision 1.3  2003/03/06 23:13:14  sinnwell
#add license text
#
#Revision 1.2  2002/12/13 19:07:03  sinnwell
#update to handle matrices
#
#Revision 1.1  2002/09/09 19:53:18  sinnwell
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

haplo.enum <- function(hmat) {
# Exact same as haplo.enum but made for handling one whole row of ugeno
# instead of haploids
  
# enumerate all possible haplotypes, given input vectors
# h1 and h2 (one possible set of haplotypes)
# and return in matrices h1 and h2 all possible haps.

# This algorithm sets up the h1.mtx and h2.mtx matrices with NA
# values, then moves across the loci which are heterozygous 
# (after the 1st het locus), flipping alleles at heterozygous
# locations, and saving results in rows moving down the matrices.

  # next three commands added 8/23 jps
        hmat <- matrix(hmat,nrow=1)
        h1 <- hmat[,seq(1,(ncol(hmat)-1),by=2)]
        h2 <- hmat[,seq(2,ncol(hmat),by=2)]
  # the rest is the same as before
	het  <- h1 != h2
	nhet <- sum(het)	# only enumerate if > 1 het loci
	if(nhet <= 1) {
		return(list(h1 = matrix(h1, nrow = 1), h2 = matrix(h2, nrow = 1
			)))
	}

# only need to flip at heterozygous loci, after 1st het locus:
	nloci <- length(h1)
	which <- (1:nloci)[het]
	which <- which[-1]
	h1.mtx <- h2.mtx <- matrix(NA, nrow = 2^(nhet - 1), ncol = nloci)
	h1.mtx[1,  ] <- h1
	h2.mtx[1,  ] <- h2
	indx.row <- 1
	for(i in which) {
		nr <- sum(!is.na(h1.mtx[, 1]))
		for(j in 1:nr) {
			indx.row <- indx.row + 1	
	# used to move down to the next row of matrix
        # now for flipping alleles across loci
			if(i < nloci) {
				h1.mtx[indx.row,  ] <- c(h1.mtx[j, 1:(i - 1)], 
				  h2.mtx[j, i], h1.mtx[j, (i + 1):nloci])
				h2.mtx[indx.row,  ] <- c(h2.mtx[j, 1:(i - 1)], 
				  h1.mtx[j, i], h2.mtx[j, (i + 1):nloci])
			}
			if(i == nloci) {
				h1.mtx[indx.row,  ] <- c(h1.mtx[j, 1:(i - 1)], 
				  h2.mtx[j, i])
				h2.mtx[indx.row,  ] <- c(h2.mtx[j, 1:(i - 1)], 
				  h1.mtx[j, i])
			}
		}
	}
	return(list(h1 = h1.mtx, h2 = h2.mtx))
}
