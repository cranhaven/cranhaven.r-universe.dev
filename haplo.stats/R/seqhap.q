#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/seqhap.q,v 1.7 2011/11/10 15:29:40 sinnwell Exp $
#$Locker:  $
#$Log: seqhap.q,v $
#Revision 1.7  2011/11/10 15:29:40  sinnwell
#major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
#
#Revision 1.6  2008/09/26 21:40:50  sinnwell
#add sim.control parameters
#
#Revision 1.5  2007/05/25 15:38:53  sinnwell
#change inlist to scanned.loci
#
#Revision 1.4  2007/05/23 18:02:33  sinnwell
#add n.sim to result
#
#Revision 1.3  2007/05/23 14:13:08  sinnwell
#add locus.label arg, and return pos in the return list
#
#Revision 1.2  2007/04/17 15:18:02  sinnwell
#change F to FALSE for R check
#
#Revision 1.1  2007/04/06 19:30:46  sinnwell
#Initial revision
#
#$Author: sinnwell $
#$Date: 2011/11/10 15:29:40 $

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

#Required inputs: geno, disease, pos
#geno: a (n-by-p) genotype matrix, where is n is the number of subjects,
#      and p is twice of the number of SNPs. 
#disease: a vector of length n. 
#pos: a vector of length (p/2).

############### Example #################
#mydata <- read.table("seqhap.dat")
#mydata.y <- mydata[,1]
#mydata.x <-  mydata[,-1]
#pos <- unlist(read.table("seqhap.pos"))
#tmp <- seqhap(geno=mydata.x,disease=mydata.y,pos) 
######################################


seqhap <- function(y, geno, pos, locus.label=NA, weight=NULL,
                   mh.threshold=3.84, r2.threshold=.95,
                   haplo.freq.min=0.005, miss.val=c(0,NA),
                   sim.control=score.sim.control(),
                   control=haplo.em.control()) {


  y=as.integer(factor(unlist(y)))-1
  weight=unlist(weight)
  pos=unlist(pos)

  if(dim(geno)[1]!=length(y)) {
    print('error')
    return(0)}
  if(!is.null(weight) && dim(geno)[1]!=length(y)){
    print('error')
    return(0)}

  nsnp <- dim(geno)[2]/2
  nsub <- dim(geno)[1]

  # get haplotype frequencies
  obj <- haplo.em(geno, locus.label=locus.label, miss.val=miss.val,control=control)
  y <- y[obj$subj.id]#expand the disease status
  subjid <- obj$subj.id 
  hapmatrix <- obj$haplotype
  nhap <- dim(hapmatrix)[1]
  hap1 <- obj$hap1code
  hap2 <- obj$hap2code
  if(!is.null(weight)) post <- weight[obj$subj.id]*obj$post
  else post <- obj$post

  seed <- sample(29999,3,replace=FALSE)
  npost <- length(obj$subj.id)

  
  #convert hap and inlist to vectors
  hap <-as.numeric(apply(hapmatrix,2,factor)) - 1
  inlist <- numeric(nsnp*nsnp)
  
  tmp <- .C("seqhapC",
            nsnp =as.integer(nsnp),
            nsub=as.integer(nsub),
            npost=as.integer(npost),
            nhap=as.integer(nhap),
            subjid=as.integer(subjid),
            hap=as.integer(hap),
            hap1=as.integer(hap1),
            hap2=as.integer(hap2),
            disease=as.integer(y),
            post=as.double(post),
            pos=as.double(pos),
            seed=as.integer(seed),
            n.sim=as.integer(sim.control$min.sim),
            min.sim=as.integer(sim.control$min.sim),
            max.sim=as.integer(sim.control$max.sim),
            p.threshold=as.double(sim.control$p.threshold),
            mh.threshold=as.double(mh.threshold),
            r2.threshold=as.double(r2.threshold),
            haplo.freq.min=as.double(haplo.freq.min),
            
            inlist=as.integer(inlist), 
            hap.df=as.integer(numeric(nsnp)),
            hap.chi=as.double(numeric(nsnp)),
            hap.p.point=as.double(numeric(nsnp)),
            hap.p.region=as.double(0),
            
            sum.df=as.integer(numeric(nsnp)),
            sum.chi=as.double(numeric(nsnp)),
            sum.p.point=as.double(numeric(nsnp)),
            sum.p.region=as.double(0),

            chi.chi=as.double(numeric(nsnp)),
            chi.p.point=as.double(numeric(nsnp)),
            chi.p.region=as.double(0),
            PACKAGE="haplo.stats"
           )

  ##convert inlist from vector to matrix
  inlist <- t(matrix(tmp$inlist,nsnp,nsnp))

  results=list(
    converge=obj$converge,
    pos=pos,
    n.sim=tmp$n.sim,
    locus.label=obj$locus.label,
    scanned.loci=inlist,
    chi.stat=tmp$chi.chi,
    chi.p.point=tmp$chi.p.point,
    chi.p.region=tmp$chi.p.region,
    hap.stat=tmp$hap.chi,
    hap.df=tmp$hap.df,
    hap.p.point=tmp$hap.p.point,
    hap.p.region=tmp$hap.p.region,
    sum.stat=tmp$sum.chi,
    sum.df=tmp$sum.df,
    sum.p.point=tmp$sum.p.point,
    sum.p.region=tmp$sum.p.region)
  
   class(results) <- "seqhap"
   return(results)
  
}



