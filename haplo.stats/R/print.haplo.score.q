#$Author: sinnwell $
#
#$Date: 2008/04/08 19:21:24 $
#
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/print.haplo.score.q,v 1.17 2008/04/08 19:21:24 sinnwell Exp $
#
#$Id: print.haplo.score.q,v 1.17 2008/04/08 19:21:24 sinnwell Exp $
#
#$Locker:  $
#
#$Log: print.haplo.score.q,v $
#Revision 1.17  2008/04/08 19:21:24  sinnwell
#haplo.effect back in x$
#
#Revision 1.16  2008/04/08 16:45:08  sinnwell
#haplo.effect now in x$control
#
#Revision 1.15  2008/04/04 16:14:28  sinnwell
#return tbl with invisible
#
#Revision 1.14  2007/01/25 19:41:07  sinnwell
#haplo.effect changed to x$haplo.effect
#
#Revision 1.13  2007/01/23 21:46:34  sinnwell
#add banner to say which haplo.effect used, change spacing
#
#Revision 1.12  2007/01/04 16:02:45  sinnwell
#exceptions for 1-row tbl output
#
#Revision 1.11  2005/03/29 16:27:27  sinnwell
#*** empty log message ***
#
#Revision 1.10  2005/03/25 15:38:36  sinnwell
#un-specify banner width, controlled by options.
#
#Revision 1.9  2004/04/07 14:08:59  sinnwell
#use nlines for quick print
#
#Revision 1.8  2004/02/26 23:08:27  sinnwell
#print.banner to printBanner
#
#Revision 1.7  2003/12/08 20:20:32  sinnwell
# changed T,F to TRUE,FALSE
#
#Revision 1.6  2003/12/01 23:46:26  sinnwell
#take out return statment.  invisible will return the original object
#
#Revision 1.5  2003/08/26 16:37:57  sinnwell
#change license
#
#Revision 1.4  2003/08/22 19:46:13  sinnwell
#updated to handle updated simulation results, add new license
#
#Revision 1.3  2003/04/25 22:11:01  schaid
#updated tbl to allow haplotype to be a data frame, as created by new haplo.em.pin function
#change was to force haplotype to be a matrix, to be backwards compatible with prior print function
#
#Revision 1.2  2003/03/06 23:22:01  sinnwell
#add license text
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
# 
print.haplo.score <- function(x, digits=max(options()$digits-2, 5), nlines=NULL, ...)

# Sinnwell JP, Schaid DJ
# Mayo Clinic Biostatistics 8/2003
  
{
# print a haplo.score object, one that has the simulation handling
# and used Progressive INsertion (PIN)

   if (!inherits(x, 'haplo.score'))
     stop("Not an object of class haplo.score!")

   printBanner(paste("Haplotype Effect Model: ", x$haplo.effect), border="-")
   
 # print of global score stats:
   printBanner("Global Score Statistics", border= "-")
   cat("\n")
   cat(paste("global-stat = ",round(x$score.global,digits),", df = ", x$df,
             ", p-val = ",round(x$score.global.p,digits),sep=""))

   # print separate section for sim p.vals and the conditions
   # under which they were made

   cat("\n\n")
   
   if(x$simulate) {
     printBanner("Global Simulation p-value Results", border="-")
     cat("\n")
     cat("Global sim. p-val = ",round(x$score.global.p.sim, digits),"\n")
     cat("Max-Stat sim. p-val = ",round(x$score.max.p.sim, digits), "\n")
     cat("Number of Simulations, Global: ", x$n.val.global, ", Max-Stat:", x$n.val.haplo)
     cat("\n\n")
   }
   
  # create table for haplotype specific stats
   tbl <- cbind(as.matrix(x$haplotype),round(x$hap.prob,digits),
            round(x$score.haplo,digits), round(x$score.haplo.p, digits))

   # add on simulated p-values, rounded to digits
   if(x$simulate) tbl <- cbind(tbl,round(x$score.haplo.p.sim, digits))

   ord <- order(x$score.haplo)
   tbl <- tbl[ord, ,drop=FALSE]

   if(!x$simulate) dimnames(tbl) <- list(NULL,c(x$locus.label,"Hap-Freq",
                  "Hap-Score","p-val"))
   else dimnames(tbl) <- list(NULL,c(x$locus.label,"Hap-Freq",
                  "Hap-Score","p-val","sim p-val"))

   printBanner("Haplotype-specific Scores",  border= "-")
   cat("\n")
   
   if(is.null(nlines) )
     print(tbl,quote=FALSE)
   else print(tbl[1:nlines,,drop=FALSE], quote=FALSE, ...)

   cat("\n")
   invisible(tbl)

}
