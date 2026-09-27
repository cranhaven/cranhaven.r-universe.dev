#$Author: sinnwell $
#$Date: 2008/04/10 14:32:18 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.score.slide.q,v 1.9 2008/04/10 14:32:18 sinnwell Exp $
#$Locker:  $
#$Log: haplo.score.slide.q,v $
#Revision 1.9  2008/04/10 14:32:18  sinnwell
#add eps.svd
#
#Revision 1.8  2007/03/08 14:38:51  sinnwell
#*** empty log message ***
#
#Revision 1.7  2007/01/25 20:31:42  sinnwell
#add haplo.effect and min.count, new features in haplo.score
#
#Revision 1.6  2006/10/25 15:09:15  sinnwell
#rm Matrix library call, only done in Ginv
#
#Revision 1.5  2006/01/27 16:26:08  sinnwell
#dependency of Ginv on Matrix
#
#Revision 1.4  2005/03/30 17:40:33  sinnwell
#shorten df column names
#
#Revision 1.3  2005/03/03 20:51:08  sinnwell
#change defaults for skip.haplo
#
#Revision 1.2  2003/12/08 19:43:53  sinnwell
# changed F,T to FALSE,TRUE
#
#Revision 1.1  2003/08/22 21:05:45  sinnwell
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

haplo.score.slide <- function(y, geno, trait.type="gaussian", n.slide=2,
                              offset = NA, x.adj = NA, min.count=5,
                              skip.haplo=min.count/(2*nrow(geno)),
                              locus.label=NA, miss.val=c(0,NA),
                              haplo.effect="additive", eps.svd=1e-5,
                              simulate=FALSE, sim.control=score.sim.control(),
                              em.control=haplo.em.control())
  # Developed by Schaid, DJ; Sinnwell, JP 2003
  # Mayo Clinic Rochester, Div of Biostatistics
{

  
# check that n.slide <= n.loci
  n.loci <- ncol(geno)/2

  if(n.slide == 1 | n.slide > n.loci) {
    warning("n.slide out of range, using default of 2 instead")
    n.slide <- 2
  }

  start.locus <- 1:(n.loci-(n.slide-1))
  if(missing(locus.label)) locus.label <- 1:n.loci
  n.site <- length(start.locus)
  score.global.p <- rep(NA,n.site)
  score.global.p.sim <- rep(NA,n.site)
  score.max.p.sim <- rep(NA,n.site)
  n.val.haplo <- rep(NA,n.site)
  n.val.global <- rep(NA,n.site)
  
  for (i in start.locus){

    # subset geno and locus.label to analyze
    col.start <- (i-1)*2 + 1
    col.end  <- (col.start - 1) + 2*n.slide

    geno.slide <-  geno[, (col.start:col.end)]

    temp <- haplo.score(y=y, geno=geno.slide, trait.type=trait.type,
                   offset = offset, x.adj = x.adj, haplo.effect=haplo.effect,
                   min.count=min.count, skip.haplo=skip.haplo,
                   miss.val=miss.val, eps.svd=eps.svd, simulate=simulate, sim.control=sim.control,
                   em.control = em.control)
    # keep global, global.sim and max.sim p-values
    score.global.p[i] <- temp$score.global.p
    score.global.p.sim[i] <- temp$score.global.p.sim
    score.max.p.sim[i] <- temp$score.max.p.sim
    # keep total simulations done
    n.val.haplo[i] <- temp$n.val.haplo
    n.val.global[i] <- temp$n.val.global
  }
  
  score.slide <- list(df=data.frame(start.loc=start.locus, score.global.p,
                      global.p.sim=score.global.p.sim, max.p.sim=score.max.p.sim),
                      n.loci=n.loci, haplo.effect=haplo.effect,
                      simulate=simulate, n.slide=n.slide, locus.label=locus.label,
                      n.val.haplo=n.val.haplo, n.val.global=n.val.global)

  class(score.slide) <- "haplo.score.slide"

  return(score.slide)

}


