#$Author: sinnwell $
#
#$Date: 2003/08/26 16:39:04 $
#
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.score.glm.q,v 1.3 2003/08/26 16:39:04 sinnwell Exp $
#
#$Id: haplo.score.glm.q,v 1.3 2003/08/26 16:39:04 sinnwell Exp $
#
#$Locker:  $
#
#$Log: haplo.score.glm.q,v $
#Revision 1.3  2003/08/26 16:39:04  sinnwell
#change license statement
#
#Revision 1.2  2003/03/06 23:15:04  sinnwell
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

haplo.score.glm <- function(y,mu,a,v,x.adj,nreps,x.post, post, x){

   u.mtx  <- (y-mu)*x.post / a
   u.score <- apply(u.mtx,2,sum)

   # Var matrix for x.adj covariates

   v.11 <- t(x.adj * v) %*% x.adj

   # Var matrix for covar(x.adj, x.post)
   v.21 <- t(x.post) %*% (x.adj * v)

   # Var matrix for haplo scores
   res <- ( (y - mu)/a ) ^2
   t1 <- rep( (v-res) ,nreps) * post
   v.22 <- t(x*t1) %*% x + t(u.mtx) %*% u.mtx
           
   # Var matrix for haplo scores, adjusted for x.adj

   v.score <- v.22 - v.21 %*% solve(v.11) %*% t(v.21) 
    
   return(list(u.score=u.score, v.score=v.score))

}

