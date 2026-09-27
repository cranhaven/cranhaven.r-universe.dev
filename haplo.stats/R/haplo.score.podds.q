#$Author: sinnwell $
#
#$Date: 2003/12/08 19:42:08 $
#
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.score.podds.q,v 1.4 2003/12/08 19:42:08 sinnwell Exp $
#
#$Id: haplo.score.podds.q,v 1.4 2003/12/08 19:42:08 sinnwell Exp $
#
#$Locker:  $
#
#$Log: haplo.score.podds.q,v $
#Revision 1.4  2003/12/08 19:42:08  sinnwell
# changed F,T to FALSE,TRUE
#
#Revision 1.3  2003/08/26 16:39:04  sinnwell
#change license statement
#
#Revision 1.2  2003/03/06 23:15:59  sinnwell
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

haplo.score.podds <- function(y, alpha, beta=NA, x.adj=NA, nreps, x.post,
                              post, x){

###################################################################
#
# If U=c(u.a, u.e, u.g), where 
#   u.a = score for alpha's
#   u.e = score for unambiguous (x.adj) covariates
#   u.g = score for ambiguous haplotypes
#
# Then the upper triangle of Var(U) can be partitioned as
#
#          | v.aa   v.ae   v.ag |   |           |
#   V(U) = |        v.ee   v.eg | = | v.11 v.12 |
#          |               v.gg |   |      v.gg |
#
# where v.12 is composed of v.aa, v.ae, v.ee
#       v.12 is composed of v.ag, v.eg
#
# and Var(u.g) = v.gg - v.12 * v.12(inv) * t(v.12)
#
# The following computes each of the submatrices as needed
# to determine u.g and Var(u.g)
#
##################################################################

adjusted <- TRUE
if(any(is.na(x.adj))) adjusted <- FALSE

if(adjusted) n.xadj <- ncol(x.adj)

n.x <- ncol(x)
K <- max(y)

# to make suscripting easier, append Inf to front of alpha,
# as place-holder for alpha[1] = Inf
alpha <- c(Inf, alpha)

if(adjusted){
   s   <- ifelse(y==1, 1, 1/(1 + exp(-(alpha[y  ] + x.adj %*% beta ))) )
   s.p <- ifelse(y==K, 0, 1/(1 + exp(-(alpha[y+1] + x.adj %*% beta ))) )
 }

if(!adjusted){
   s   <- ifelse(y==1, 1, 1/(1 + exp(-(alpha[y  ]  ))) )
   s.p <- ifelse(y==K, 0, 1/(1 + exp(-(alpha[y+1]  ))) )
 }


w1 <- (s*(1-s) - s.p*(1-s.p))/(s - s.p)

u.mtx <- w1 * x.post
u.score <- apply(u.mtx,2,sum)

#  compute information matrix for alpha-beta (v.ab) and alpha-alpha (v.aa)

tmp1 <- (s   + s.p^2 - 2*s*s.p)*s.p*(1-s.p)/(s-s.p)^2
tmp2 <- (s.p +   s^2 - 2*s*s.p)*s*(1-s)/(s-s.p)^2
tmp3 <- s.p*(1-s.p)*s*(1-s)/(s-s.p)^2

v.ag <- matrix(rep(0, (K-1)*n.x), ncol=n.x)
if(adjusted) v.ae <- matrix(rep(0, (K-1)*n.xadj), ncol=n.xadj)
v.aa <- matrix(rep(0,(K-1)^2),ncol=(K-1))

n.subj <- length(y)

for(j in 2:K){
   wt <- rep(0,n.subj)
   wt <- ifelse(y==(j-1), (tmp1 - tmp3), wt)
   wt <- ifelse(y==j, (tmp2 - tmp3), wt)

   v.ag[(j-1),] <- apply(wt * x.post, 2,sum)

   if(adjusted) v.ae[(j-1),] <-  apply(wt * x.adj, 2,sum)

   v.aa[(j-1),(j-1)] <- sum(tmp1[y==(j-1)]) + sum(tmp2[y==j])   
   if(j < K) v.aa[(j-1), j] <- -sum(tmp3[y==j])
 }


# fill in lower tri of v.aa to make it symmetric
v.aa <- v.aa + t( (col(v.aa) > row(v.aa))*v.aa )

# Louis' method for v.gg
w2 <- s*(1-s) + s.p*(1-s.p)
t1 <- rep( (w2 - w1^2), nreps) * post
v.gg <- t(x*t1) %*% x + t(u.mtx) %*% u.mtx

if(adjusted){
   v.ee <- t(w2*x.adj) %*% x.adj
   v.eg <- t(w2*x.adj) %*% x.post
   v.11 <- rbind( cbind(v.aa, v.ae), cbind(t(v.ae),v.ee) )
   v.12 <- rbind(v.ag,v.eg)
   v.score <- v.gg - t(v.12) %*% solve(v.11) %*% v.12
 }

if(!adjusted){
   v.score <- v.gg - t(v.ag) %*% solve(v.aa) %*% v.ag
 }


return(list(u.score=u.score, v.score=v.score))

}
