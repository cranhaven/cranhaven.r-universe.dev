#$Author: sinnwell $
#$Date: 2005/03/29 19:21:53 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/print.haplo.group.q,v 1.8 2005/03/29 19:21:53 sinnwell Exp $
#$Id: print.haplo.group.q,v 1.8 2005/03/29 19:21:53 sinnwell Exp $
#$Locker:  $
#$Log: print.haplo.group.q,v $
#Revision 1.8  2005/03/29 19:21:53  sinnwell
#make printBanner abide to options()$width
#
#Revision 1.7  2004/04/06 21:30:06  sinnwell
#use nlines to limit output for demos
#
#Revision 1.6  2004/02/26 23:06:50  sinnwell
#print.banner to printBanner
#
#Revision 1.5  2003/08/26 16:37:57  sinnwell
#change license
#
#Revision 1.4  2003/03/06 22:04:56  sinnwell
#update license and to handle haplo.group list obj
#
#Revision 1.3  2003/01/29 15:50:17  sinnwell
#remove printing of rows.rem attr
#
#Revision 1.2  2003/01/17 16:58:38  sinnwell
#revision for haplo.score version 1.2
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

print.haplo.group <- function(x, digits=max(options()$digits-2, 5), nlines=NULL, ...) {

  ### Print haplo.group object with frequency table for group
  ### and estimated frequencies for haplotypes.
    if (!inherits(x, 'haplo.group'))
      stop("Not an object of class haplo.group!")
    n.loci <- x$n.loci
    n.group <- length(x$group.count)

    # Round expected frequencies to set length digits
    df.print<-data.frame((x$group.df)[,1:n.loci],
           round((x$group.df)[,(n.loci+1):(n.loci+n.group+1)],digits))
    cat("\n")
    printBanner("Counts per Grouping Variable Value", border = "-")
    print(x$group.count)
    cat("\n\n")
    printBanner("Haplotype Frequencies By Group ", border = "-")

    if(is.null(nlines)) print(df.print)
    else print(df.print[1:nlines,])

    invisible()
}

