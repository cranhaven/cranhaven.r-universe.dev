#$Author: schaid $
#
#$Date: 2003/10/15 17:17:35 $
#
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/locator.haplo.q,v 1.4 2003/10/15 17:17:35 schaid Exp $
#
#$Id: locator.haplo.q,v 1.4 2003/10/15 17:17:35 schaid Exp $
#
#$Locker:  $
#
#$Log: locator.haplo.q,v $
#Revision 1.4  2003/10/15 17:17:35  schaid
#fixed format of hapotypes for text
#
#Revision 1.3  2003/08/26 16:39:04  sinnwell
#change license statement
#
#Revision 1.2  2003/03/06 23:18:02  sinnwell
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

locator.haplo <- function(obj){

   h <- apply(format(obj$haplotype),1,paste, collapse=":")
   x <- obj$hap.prob
   y <- obj$score.haplo
   rng <- range(y)
   delta <- .05* (rng[2] - rng[1])
   mid.rng <- sum(rng)/2

   tmp <- locator()
   x.pt <- tmp$x
   y.pt <- tmp$y
   n.pt <- length(x.pt)
   x.coord <- NULL
   y.coord <- NULL
   hap.txt <- NULL

   for(i in 1:n.pt){
      z <- (x-x.pt[i])^2 + abs(y-y.pt[i])^2
      which <- z==min(z)
      h.txt <- h[which]
      x.tmp <- x[which]
      sgn <- if(y[which] < mid.rng) + 1 else -1
      y.tmp <- y[which] + sgn*delta
      text(x.tmp,y.tmp,h.txt)
      x.coord <- c(x.coord,x.tmp)
      y.coord <- c(y.coord,y.tmp)
      hap.txt <- c(hap.txt, h.txt)
    }

 invisible()
return(list(x.coord=x.coord,y.coord =y.coord, hap.txt=hap.txt))
 }


   
