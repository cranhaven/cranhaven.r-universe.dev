#$Author: sinnwell $
#$Date: 2009/03/09 16:10:19 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/plot.seqhap.q,v 1.6 2009/03/09 16:10:19 sinnwell Exp $
#$Locker:  $
#$Log: plot.seqhap.q,v $
#Revision 1.6  2009/03/09 16:10:19  sinnwell
#for R use pchisq(, , lower=FALSE) to get precision on small p-vals
#
#Revision 1.5  2008/05/16 18:34:44  sinnwell
#warnings for when perm pvals 0, set to 1/(n.sim+1)
#
#Revision 1.4  2008/05/09 20:59:58  sinnwell
#add minp parameter
#
#Revision 1.3  2008/05/08 17:38:26  sinnwell
#allow ylim, set minimum p as 1e-10, issue warning if pval not valid
#
#Revision 1.2  2007/05/25 18:17:15  sinnwell
#rm par options cex.axis and las/srt, reserve for users
#
#Revision 1.1  2007/05/23 17:41:36  sinnwell
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

plot.seqhap <- function(x, pval="hap", single=TRUE, minp=.Machine$double.eps, ...) {
  ## plot method of seqhap object
  ##  1. x-axis: give locus labels in order, in "pos" chrom positions
  ##  2. y-axis: plot the -log10 of the p-val chosen
  ##  3. the pval parameter selects which ("hap","hap.sim","sum","sum.sim") to plot
  ##  4. Use minp as the smallest "allowable" p-value.
  ##     Any p-value smaller will be set to log10(minp)
  
  if(!inherits(x, "seqhap"))
	    stop("Not a seqhap object")
  
  ## if ylim is given in ..., pull it off and use it
  dots <- as.list(substitute(list(...)))[-1]
  ylim.indx <- match('ylim', names(dots))
  ylim.user <- if(!is.na(ylim.indx)) eval(dots[ylim.indx]$ylim) else NULL

  single.p <- 1-pchisq(x$chi.stat,1)

  if(is.na(match(pval, c("hap", "sum", "hap.sim", "sum.sim")))) {
    warning("Invalid value for pval, set to default. \n")
    pval <- "hap"
  }
  
  switch(pval,
         "hap"={ seqp <- pchisq(x$hap.stat, x$hap.df, lower.tail=FALSE)
                ylabel <- '-log10(hap.pval)' 
                },
         "sum"={ seqp <- pchisq(x$sum.stat,x$sum.df, lower.tail=FALSE) 
                 ylabel <- '-log10(sum.pval)'
               },
         "hap.sim"={seqp <- ifelse(x$hap.p.point==0, 1/(x$n.sim+1), x$hap.p.point)
                    if(any(seqp < 1/x$n.sim)) warning("One or more permutation p-value(s) set from 0 to 1/(n.sim+1)")
                    ylabel <- '-log10(hap.sim.pval)'
                 },
         "sum.sim"={seqp <- ifelse(x$sum.p.point==0, 1/(x$n.sim+1), x$sum.p.point)
                    if(any(seqp < 1/x$n.sim)) warning("One or more permutation p-value(s) set from 0 to 1/(n.sim+1)")
                    ylabel <- '-log10(sum.sim.pval)'
                 })

  if(any(single.p < minp)) {
    warning(paste("One or more single-marker p-values too small to plot, set to ", minp, "\n"))
    single.p <- ifelse(single.p < minp, minp, single.p)
  }
  if(any(seqp < minp)) {
    warning(paste("One or more multi-marker p-values too small to plot, set to ", minp, "\n"))
    seqp <- ifelse(seqp < minp, minp, seqp)
  }

  log.single.p <- if(single) -log10(single.p) else NULL
  log.seqp <- -log10(seqp)
  
  ylim <- if(is.null(ylim.user)) c(0, max(3, log.single.p, log.seqp)) else ylim.user
  
  plot(x$pos, log.single.p,ylim=ylim, type="n",xaxt="n",
       xlab='', ylab=ylabel) #, ...)

  for(i in 1:length(x$pos))
    {
      # plot a filled triangle for locus i at heigh of -log10p[i]
      points(x$pos[x$scanned.loci[i,1]], log.seqp[i], pch=17)
      if(sum(x$scanned.loci[i,]>0)>1)
        {
          scanned.loci.i <- sort(x$scanned.loci[i,])
          scanned.loci.i <- scanned.loci.i[scanned.loci.i>0]
          
          #plot a line for each set of combined loci at height of -log10p[i]
          lines(c(x$pos[min(scanned.loci.i)],x$pos[max(scanned.loci.i)]),rep(log.seqp[i],2))
          
          #plot an asterisk/circle on each locus combined at height of -log10p[i]
          points(x$pos[setdiff(scanned.loci.i, x$scanned.loci[i,1])],rep(log.seqp[i],length(scanned.loci.i)-1), ...)
         }
    }

  if(single) {       lines(x$pos,log.single.p,lty=2) }

  axis(1,at=x$pos, labels=x$locus.label, ...) # adj=0, las=2, cex.axis=.8

  invisible()

}

