#  $Author: sinnwell $
#  $Date: 2007/03/22 15:41:32 $ -->
#  $Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.group.q,v 1.12 2007/03/22 15:41:32 sinnwell Exp $ -->
#  $Locker:  $  
#   $Log: haplo.group.q,v $
#   Revision 1.12  2007/03/22 15:41:32  sinnwell
#   fix call to haplo.em
#
#   Revision 1.11  2007/03/13 15:56:53  sinnwell
#   add weight for haplo.em on all people
#
#   Revision 1.10  2003/12/08 20:10:59  sinnwell
#    changed T,F to TRUE,FALSE
#
#   Revision 1.9  2003/10/15 16:13:10  schaid
#   changed em.control to control, to be consistent with haplo.em
#
#   Revision 1.8  2003/08/28 14:22:50  sinnwell
#   re-fix for rows.rem
#
#   Revision 1.7  2003/08/27 21:02:29  sinnwell
#   update for release 1.2.0 with PIN, keep rows.rem
#
#   Revision 1.6  2003/08/26 16:39:04  sinnwell
#   change license statement
#
#   Revision 1.5  2003/05/20 14:49:53  sinnwell
#   add more em control parameters
#
#   Revision 1.3  2003/03/06 20:58:13  sinnwell
#   updates to make return obj a list and, and gnames
#
#   Revision 1.2  2003/01/17 16:56:39  sinnwell
#   revision for haplo.score version 1.2
#
# Revision 1.2  2002/09/16 14:46:42  sinnwell
# Fix RCS keywords
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

haplo.group <- function(group, geno, locus.label=NA,
                        miss.val=0, weight=NULL,
                        control=haplo.em.control())
    ## Developed by JP Sinnwell  & DJ Schaid ##
    ## Mayo HSR Biostatistics  ##
    ## 7/2002 ##
    ## update for allowing missing alleles 11/2002
    ## update for haplo.em with Progressive Insertion 8/2003
{
# Build a haplo.group object with a dataframe, number of loci, and counts by group
# The dataframe will contain all possible haplotypes and expected frequencies
# for the entire data set and the groups where the haplotype could exist

  group.name <- deparse(substitute(group))

 # Check dims of group and geno
  nr <- nrow(geno)
  if(length(group)!=nr) stop("Dims of group and geno are not compatible")

 # Set number of loci  
  n.loci <- ncol(geno)/2
  if(n.loci != (floor(ncol(geno)/2)) ) stop("Odd number of cols of geno")

 # Make groups either numeric or character  
  if (!is.numeric(group)) group <-  as.character(group)
 
 # Check for  missing group data 
  miss.group <- is.na(group) | group=="NA" 

 # Subset to non-missing group values:
  group <- group[!miss.group]
  geno <- geno[!miss.group,]

 # Create a haplo object (using haplo.em) using size-evaluation criteria
  haplo.all <- haplo.em(geno, locus.label, miss.val=miss.val, weight=weight,
                        control=control)

   # Check convergence of EM
  if(!haplo.all$converge) stop("EM for haplo failed to converge")

  ## new subset of removed genorows based on enumeration criteria of haplo.em

  rem.geno <- haplo.all$rows.rem
  if(length(rem.geno)>0) {
    geno <- geno[-rem.geno,]
    group <- group[-rem.geno]
  }

 # Make initial dataframe with total groups
  haplotype.all <-  haplo.all$haplotype
  prob.all <- haplo.all$hap.prob
  haplo.freq <- data.frame(haplotype.all, prob.all)

 # Subset haplotypes by group variable
 # Do not weight because sub-samples are not over-sampled.
  ugroup <-  sort(unique(group))
  gnames <- character(0)
  for (i.group in ugroup) {
     gnames <- c(gnames, paste(group.name, i.group, sep= "="))
     group.sub <- group==i.group
     geno.sub <- geno[group.sub,]
    # Create haplo object for i.group
    # rows to be removed are already removed from first run of haplo.em
     haplo.tmp <- haplo.em(geno.sub, locus.label, miss.val=miss.val,
                           control=control)

    # Check convergence of EM
     if(!haplo.tmp$converge) cat("Warning: EM failed to converge for group", i.group, "\n")
    # Create dataframe for the group with haplotypes and prob
     haplotype.sub <- haplo.tmp$haplotype
     prob.sub <- haplo.tmp$hap.prob

     df.sub <- data.frame(haplotype.sub,prob.sub)

    # Merge matching the haplotypes (cols 1:n.loci) and keep all rows
    # This will leave NA's where no match in sub group
    haplo.freq <- merge(haplo.freq, df.sub, by.x=1:n.loci, by.y=1:n.loci, all.x=TRUE, all.y=TRUE)

  }   # end for i.group
  
 # Create locus label if missing:
  if (all(is.na(locus.label))) {
     locus.label<- paste("loc-",1:n.loci,sep="")
  }
 # Set names for the dataframe
  names(haplo.freq) <-  c(locus.label, "Total", gnames)
 # Get group frequencies
  group.count <-  table(group)

  hapgroup <- list(group.df=haplo.freq, group.count=group.count, n.loci=n.loci)

  class(hapgroup) <- c("haplo.group","data.frame")
  
  return(hapgroup)
}
