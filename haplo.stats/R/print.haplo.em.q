#$Author: sinnwell $
#$Date: 2009/04/08 18:40:58 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/print.haplo.em.q,v 1.9 2009/04/08 18:40:58 sinnwell Exp $
#$Locker:  $
#$Log: print.haplo.em.q,v $
#Revision 1.9  2009/04/08 18:40:58  sinnwell
#*** empty log message ***
#
#Revision 1.8  2009/04/08 17:52:05  sinnwell
#use R's pchisq with lower.tail=FALSE for more signif digits
#
#Revision 1.7  2008/12/12 22:18:43  sinnwell
#return invisible(df)
#
#Revision 1.6  2008/02/11 22:52:27  sinnwell
#edit the message for missing rows
#
#Revision 1.5  2007/11/07 21:03:39  sinnwell
#add digits
#
#Revision 1.4  2004/04/06 20:39:39  sinnwell
#use nlines to limit printouts in vignettes
#
#Revision 1.3  2004/02/26 23:04:28  sinnwell
#print.banner to printBanner
#
#Revision 1.2  2003/08/26 22:09:38  sinnwell
#added GPL License
#
#Revision 1.1  2003/08/26 20:59:40  schaid
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

print.haplo.em <- function(x, digits=max(options()$digits-2, 5), nlines=NULL, ...){

  printBanner("Haplotypes")
  df <- data.frame(x$haplotype,round(x$hap.prob,digits))
  names(df) <- c(x$locus.label, "hap.freq")
  if(is.null(nlines)) print(df)
  else print(df[1:nlines,])
  invisible()

  if(x$converge==0)
     warning("EM failed to converge")

  printBanner("Details")

  pval <- NA
  if(x$df.lr > 0) {
    pval = pchisq(x$lr, x$df.lr, lower.tail=FALSE)
  }
    
  cat("lnlike = ",round(x$lnlike,digits),"\n")
  cat("lr stat for no LD = ",round(x$lr,digits),", df = ",x$df.lr,", p-val = ",round(pval,digits),"\n")

  if(length(x$rows.rem > 0)) {
     cat("\nResults may be incomplete because one or more subjects was removed\n")
   }

  invisible(df)
}
