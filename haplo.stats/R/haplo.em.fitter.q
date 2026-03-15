#$Author: schaid $
#$Date: 2007/02/27 20:15:24 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.em.fitter.q,v 1.5 2007/02/27 20:15:24 schaid Exp $
#$Locker:  $
#$Log: haplo.em.fitter.q,v $
#Revision 1.5  2007/02/27 20:15:24  schaid
#control max.haps.limit with checkIntMax (see haplo.em and haplo_em_pin)
#
#Revision 1.4  2004/03/19 15:02:44  sinnwell
#keep PACKAGE in all .C calls, required for R, '...' for Splus
#
#Revision 1.3  2004/03/17 21:06:24  sinnwell
#separate calls for .C( for R and Splus
#
#Revision 1.2  2003/08/26 22:09:38  sinnwell
#added GPL License
#
#Revision 1.1  2003/08/26 21:02:24  schaid
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
haplo.em.fitter  <- function(
                                 n.loci,
                                 n.subject,
                                 weight,
                                 geno.vec,
                                 n.alleles,
                                 max.haps,
                                 max.iter,
                                 loci.insert.order,		      
                                 min.posterior,
                                 tol,
                                 insert.batch.size,
                                 random.start,            
                                 iseed1,          
                                 iseed2,
                                 iseed3,
                                 verbose){

converge <- 0
min.prior <- 0.0
n.unique <- 0
lnlike <- 0.0
n.u.hap <- 0
n.hap.pairs <- 0

on.exit( 
        .C("haplo_free_memory", PACKAGE="haplo.stats") 
        )

tmp1 <- .C("haplo_em_pin",
           n.loci=as.integer(n.loci),
           n.subject=as.integer(n.subject),
           weight=as.double(weight),
           geno.vec=as.integer(geno.vec),
           n.alleles = as.integer(n.alleles),
           max.haps = as.integer(max.haps),
           max.iter=as.integer(max.iter),
           loci.insert.order=as.integer(loci.insert.order),
           min.prior=as.double(min.prior),
           min.posterior=as.double(min.posterior),
           tol=as.double(tol),
           insert.batch.size=as.integer(insert.batch.size),
           converge=as.integer(converge),
           lnlike=as.double(lnlike),
           n.u.hap=as.integer(n.u.hap),
           n.hap.pairs=as.integer(n.hap.pairs),
           random.start=as.integer(random.start),            
           iseed1=as.integer(iseed1),          
           iseed2=as.integer(iseed2),
           iseed3=as.integer(iseed3),
           verbose=as.integer(verbose),
           PACKAGE="haplo.stats")
         

tmp2 <- .C("haplo_em_ret_info",
           n.u.hap=as.integer(tmp1$n.u.hap),
           n.loci=as.integer(tmp1$n.loci),
           n.pairs=as.integer(tmp1$n.hap.pairs),
           hap.prob=as.double(numeric(tmp1$n.u.hap)),
           u.hap=as.integer(numeric(tmp1$n.u.hap*tmp1$n.loci)),
           u.hap.code=as.integer(numeric(tmp1$n.u.hap)),
           indx.subj=as.integer(numeric(tmp1$n.hap.pairs)),
           post=as.double(numeric(tmp1$n.hap.pairs)),
           hap1code=as.integer(numeric(tmp1$n.hap.pairs)),
           hap2code=as.integer(numeric(tmp1$n.hap.pairs)),
           PACKAGE="haplo.stats")
       
obj <- list(tmp1=tmp1, tmp2=tmp2)

return(obj)

}

