#$Author: sinnwell $
#$Date: 2008/01/04 20:47:47 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/plot.haplo.score.slide.q,v 1.12 2008/01/04 20:47:47 sinnwell Exp $
#$Locker:  $
#$Log: plot.haplo.score.slide.q,v $
#Revision 1.12  2008/01/04 20:47:47  sinnwell
#add ... to lines, allowing col(or) and lty specification.  Make exception calls to axis in R and S, S allows srt and doesn't use col; R doesn't allow srt and uses color to undesirably color the axis
#
#Revision 1.11  2007/05/29 15:53:29  sinnwell
#take out las and cex.axis, left for user to pass in ...
#
#Revision 1.10  2007/05/22 20:32:39  sinnwell
#change cex to cex.axis, las does what srt did in Splus, 1=horizontal, 2-vertical
#
#Revision 1.9  2007/04/25 20:32:45  sinnwell
#*** empty log message ***
#
#Revision 1.8  2007/04/12 20:49:51  sinnwell
#re-set zero p-values for global to epsilon = 1e-10
#
#Revision 1.7  2007/04/03 21:10:06  sinnwell
#use epsilon as minimum p-value
#give warnings if pval<epsilon and what it is set to for that test
#
#Revision 1.6  2005/03/31 19:23:35  sinnwell
#global.p.sim and max.p.sim names remove 'score' from them
#
#Revision 1.5  2004/02/16 22:03:55  sinnwell
#fix handling of pval parameter, easier
#
#Revision 1.4  2004/01/27 21:25:57  sinnwell
#allow x-axis ticks to be spaced relevant to marker dist
#
#Revision 1.3  2003/11/21 16:23:34  sinnwell
#better fix to cex and srt control to user
#
#Revision 1.2  2003/11/20 23:06:47  sinnwell
#give control to cex and srt
#
#Revision 1.1  2003/08/22 21:07:23  sinnwell
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
plot.haplo.score.slide <- function(x, pval="global", dist.vec=1:x$n.loci, ...)

##################################  
# Schaid DJ, Sinnwell JP  5/2003 #
# Mayo Clinic Biostatistics      #
##################################
{
  # plot method of haplo.score.slide object
  #  1. x-axis: give locus labels in order
  #  2. y-axis: plot the -log10 of the p-val chosen
  #  3. the pval parameter selects which ("global","global.sim","max.sim") to plot

  if(!inherits(x, "haplo.score.slide"))
	    stop("Not a haplo.score.slide object")
 
  if(length(dist.vec)!=x$n.loci) {
    dist.vec <- 1:x$n.loci
    warning("Length of distance vector should be the same as number of loci")
  }
  
  # match pval to the list below
  # if no match, give default 'global'
  p.vec <- c("global","global.sim","max.sim")
  p.int <- pmatch(pval, p.vec)
  if(all(is.na(p.int))) p.int <- 1
  pval <- p.vec[p.int]

  if(p.int > 1 && !x$simulate)
    stop("\nRequested to plot simulated p-values, but no simulations performed!")

  #if p-values are close to zero ( < epsilon) then find some minimum to plot
  # minimum depends on which pval choice
  epsilon <- 1e-10
    
  switch(pval,
         global = {
           p <- x$df$score.global.p
           if(min(p[p>0]) < epsilon) epsilon <- min(p[p>0])
           if(sum((p > epsilon)) < length(p)) cat(paste("Some p-values equivalent to zero, plotted as ", epsilon, "\n"))
           lnp <- (-1)*log10(ifelse(p < epsilon, epsilon, p))
           ylabel="-log10(score.global.p)"
         },
         global.sim = {
           eq.zero <- which(x$df$global.p.sim == 0)
           lnp <- (-1)*log10(x$df$global.p.sim)
           if(length(eq.zero)) {
             cat(paste("Some p-values are equivalent to zero, set to 0.5 / nsim \n"))
             lnp[eq.zero] <- (-1)*log10(.5/x$n.val.global[eq.zero])
           }
           ylabel="-log10(global.p.sim)"
         },
         max.sim = {
           eq.zero <- which(x$df$max.p.sim == 0)
           lnp <- (-1)*log10(x$df$max.p.sim)
           if(length(eq.zero)) {
             cat(paste("Some p-values are equivalent to zero, set to 0.5 / nsim \n"))
             lnp[eq.zero] <- (-1)*log10(.5/x$n.val.haplo[eq.zero])
           }
           ylabel="-log10(max.p.sim)"
         })

  # prepare x.axis marks for the loci, based on dist.vec
  x.axis.vec <- dist.vec-min(dist.vec)
  x.axis.vec <- x.axis.vec*(x$n.loci-1)/max(x.axis.vec) + 1

  # for R, las controls label rotation on the x-axis labels
  # for Splus x-axis, convert las to srt (degrees) 1=horizontal (0deg) 2=perpendicular (90deg)
  # set below where necessary
  
  # plot outer region, then the axis with locus labels
  plot(x=c(1,x$n.loci),y=c(0,max(lnp)),type="n",ylab=ylabel,xlab="",xaxt="n", ...)
  axis(1,at=x.axis.vec, labels=x$locus.label)
  
  # plot a line for each set of n.slide loci at height of -log10p[i]
  for (i in x$df$start.loc) {
    end.loc <- i + x$n.slide - 1
    lines(c(x.axis.vec[i],x.axis.vec[end.loc]),c(lnp[i],lnp[i]), ...)
  }
  invisible()

}
