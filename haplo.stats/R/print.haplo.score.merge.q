#$Author: sinnwell $
#$Date: 2005/03/30 16:40:22 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/print.haplo.score.merge.q,v 1.10 2005/03/30 16:40:22 sinnwell Exp $
#$Locker:  $
#$Log: print.haplo.score.merge.q,v $
#Revision 1.10  2005/03/30 16:40:22  sinnwell
#remove banner.width from printBanner
#
#Revision 1.9  2004/04/07 14:08:59  sinnwell
#use nlines for quick print
#
#Revision 1.8  2004/02/26 23:07:36  sinnwell
#print.banner to printBanner
#
#Revision 1.7  2003/10/03 19:32:38  sinnwell
#fix for R release, handles match.call different
#
#Revision 1.6  2003/08/26 16:37:57  sinnwell
#change license
#
#Revision 1.5  2003/06/19 22:05:04  sinnwell
#re-assign row numbers
#
#Revision 1.4  2003/06/19 13:46:02  sinnwell
#add order.by and all.haps options.  default is order by score and all.haps=F
#
#Revision 1.3  2003/04/15 18:40:24  sinnwell
#fix merge.bin to x
#
#Revision 1.2  2003/03/06 22:08:40  sinnwell
#insert license, and change getting n.loci
#
#Revision 1.1  2003/01/17 16:29:47  sinnwell
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

print.haplo.score.merge <- function(x, order.by="score", all.haps=FALSE, digits=max(options()$digits-2, 5), nlines=NULL, ...)
{

   ### Print haplo.score.merge object to screen 
    if (!inherits(x, 'haplo.score.merge'))
      stop("Not an object of class haplo.score.merge!")

    m.call <- match.call()
    noOrder <- is.na(pmatch("order.by", names(m.call)))
    noAll.haps <- is.na(pmatch("all.haps", names(m.call)))
    noDigits <- is.na(pmatch("digits", names(m.call)))
    if (!noOrder)
      order.by <- m.call[[3]]
    if (!noAll.haps) {
      if(!noOrder) 
        all.haps <- m.call[[4]]
      else all.haps <- m.call[[3]]
    }

#    if(length(m.call[[3]])) order.by <- m.call[[3]]
#    if(length(m.call[[4]])) all.haps <- m.call[[4]]
    
    n.loci <- (1:length(names(x)))[names(x)=="Hap-Score"] - 1

    order.vec <- c("score","freq","haplotype")
    order.int <- pmatch(order.by, order.vec)
    if(all(is.na(order.int))) order.int <- 1
    order.by <- order.vec[order.int]

    #if all.haps not requested, then subset to only those w/ a score
    if(!all.haps)  x <- x[1:length(na.omit(x$"Hap-Score")),]

    # Combine haplotypes and results
    # round numeric columns to set length digits
    df.print <- data.frame(x[,1:n.loci],
                     round(x[,(n.loci+1):ncol(x)],digits))

    # select between 3 different orderings
    switch(order.by,
           score = {
             ord <- 1:nrow(x)
           },
           freq = {
             ord <- (1:nrow(x))[order(x$"Hap-Freq")]
           },
           haplotype = {
             ord <- as.numeric(attributes(haplo.hash(df.print[,1:n.loci])$hap.mtx)$row.names)
           })

    cat("\n\n")

    printBanner("Haplotype Scores, p-values, and Frequencies By Group", border = "-")
    df.print <- data.frame(df.print[ord,], row.names=NULL)
    if(is.null(nlines))
      print(df.print, digits=digits, ...)
    else print(df.print[1:nlines,], digits=digits, ...)
    invisible()
  }
