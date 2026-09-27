#$Author: sinnwell $
#$Date: 2008/03/24 22:21:29 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.model.frame.q,v 1.10 2008/03/24 22:21:29 sinnwell Exp $
#$Locker:  $
#$Log: haplo.model.frame.q,v $
#Revision 1.10  2008/03/24 22:21:29  sinnwell
#rm allele.lev, miss.val parameters
#
#Revision 1.9  2007/10/22 20:52:40  sinnwell
#fix to allow effect of only rare haplo.  Also some adjustments for this case in the dominant and recessive cases of the switch()
#
#Revision 1.8  2004/10/22 19:19:28  sinnwell
#for recessive model, if sum(col[i]) is zero, subset accordingly
#x.mat and haplo.common.  Guard against 1-col left keep as data.frame
#
#Revision 1.7  2004/03/22 15:04:24  sinnwell
#under last change for R, fixed stringsAsFactors problem
#
#Revision 1.6  2004/03/15 22:50:27  sinnwell
#hapEM$haplotype is char for R, so convert to char, then integer
#
#Revision 1.5  2004/03/03 22:14:30  schaid
#added allele.lev to allow this to work in R for character alleles
#
#Revision 1.4  2003/12/08 20:14:29  sinnwell
# changed T,F to TRUE,FALSE
#
#Revision 1.3  2003/11/17 23:28:01  schaid
#made compatible with R
#
#Revision 1.2  2003/10/06 15:45:49  sinnwell
#change stop( ) line w/ '\n' char: R didn't understand
#
#Revision 1.1  2003/09/16 16:02:09  schaid
#Initial revision
#
haplo.model.frame <- function(m, locus.label=NA, control=haplo.glm.control() ){

  # Procedures to modify a glm model.frame by:
  #
  # 1. enumerating haplotypes,
  # 2. setup haplotype design matrix
  # 3. expand rows in the current model.frame (m), according to the
  #    number of pairs  of haplotypes consistent with a subject's marker data
  # 4. replace the model.matrix in  m (which corresponds to unphased
  #    genotpes) with a haplotype design matrix, and then return the
  #    modified model.frame

  # Notes:
  # If there is a problem with "No haplotype freqs > haplo.min.freq"
  # the function has been changed to return an error message which
  # is then passed to the haplo.glm function. This prevents simulations
  # from crashing.

  # Input:
  #
  # m                 glm model.frame
  #
  # control = list  with the following parameters:
  #
  # haplo.effect      the model matrix coding of haplotypes
  #
  # haplo.base        the baseline haplotype
  #
  # haplo.freq.min    the minimum haplotype frequency for inclusion
  #                   of the haplotype as a distinct model covariate
  #
  # sum.rare.min    the sum of the "rare" haplotype frequencies must
  #                   be larger than this number in order for a "rare" 
  #                   haplotype term to be included in the model
  #
  # keep.rare.haplo   a logical for inclusion of the rare haplotype in
  #                   the model.
  
  haplo.effect    <- control$haplo.effect
  haplo.base      <- control$haplo.base
  haplo.freq.min  <- control$haplo.freq.min
  sum.rare.min    <- control$sum.rare.min
  keep.rare.haplo <- control$keep.rare.haplo


  # check type of haplo.effect, by allowing partial matching,
  # and then code to abreviations

  chk <- charmatch(haplo.effect, c("additive", "dominant", "recessive"))
  if(is.na(chk)) stop("Invalid haplo.effect")
  if(chk == 0)   stop("Ambiguous haplo.effect")
  haplo.effect <- c("add","dom","rec")[chk]


  # determine which factor in model frame is the geno matrix
  gindx <- mf.gindx(m)

  haplo.names <- names(m)[gindx]

  geno <- m[[gindx]]

  if(is.null(attributes(geno)$unique.alleles))
    stop("Genotype matrix does not contain unique.alleles attribute, use setupGeno")
  
  allele.lev <- attributes(geno)$unique.alleles

  
  # Setup  weights for use in EM
  wt <- model.extract(m, weights)
  if(!length(wt)){
    wt <- rep(1, nrow(m))
  } else if(any(wt < 0)){
    stop("negative weights not allowed")
  }

  # EM algorithm for haplotype frequencies

  hapEM <- haplo.em(geno, locus.label=locus.label, miss.val=NA,
                    weight=wt, control=control$em)


  if(!hapEM$converge){
    stop("haplo.em failed to converge in haplo.model.frame. Try different control parameters for haplo.em.control - see haplo.glm.control")
  }

  # If any subects were removed by EM, need to remove them from model.frame
  if(length(hapEM$rows.rem)) {
    m <- m[-hapEM$rows.rem, , drop=FALSE]
  }

  # data for haplotype indices

  g.dat <- data.frame(hapEM$indx.subj, hapEM$hap1code, hapEM$hap2code)
  attr(g.dat,"names") <-  c("indx.subj","hap1","hap2")

  indx.subj <- g.dat$indx.subj
  hap1code  <- g.dat$hap1
  hap2code  <- g.dat$hap2

  # haplotype frequencies
  haplo.freq <- hapEM$hap.prob
 
  ## Set up haplotype design matrix
 
  # create vector of unique haplotypes 
  uhap <- sort(unique(c(hapEM$hap1code, hapEM$hap2code)))

 # if no base haplotype defined, then use most frequent haplotype as base

  if(is.null(haplo.base)) haplo.base <- uhap[haplo.freq == max(haplo.freq)]
  if(length(haplo.base) > 1) haplo.base <- haplo.base[1]
  if(sum(uhap==haplo.base)==0){
    stop("Base haplotype not among possible haplotypes")
  }

  # check if any haplotypes remain after exclude base and rare haplotypes
  haplo.common <- uhap[(haplo.freq >  haplo.freq.min) & uhap!=haplo.base]

  if(!keep.rare.haplo && length(haplo.common)==0)
    stop("No haplotypes effects to model")

  # now set up design matrix for add effects, with all haplotypes except base,
  # and later collapse over rare haplotypes

  x.common <- outer(hap1code, haplo.common, "==") + 
              outer(hap2code, haplo.common, "==")

  haplo.rare   <- uhap[(haplo.freq <= haplo.freq.min) & uhap!=haplo.base]
  x.rare <- outer(hap1code, haplo.rare, "==") + 
            outer(hap2code, haplo.rare, "==")

  # find the haplotype frequencies for the rare haplotypes

  haplo.freq.rare <- haplo.freq[haplo.freq <= haplo.freq.min & uhap!=haplo.base]

  # now fix up design matrix for chosen haplo.effect
  # note: x.rare is added only if conditions for wanting it are
  # satisfied

  haplo.rare.term <- FALSE


  switch(haplo.effect,
         add = {
                 x.hap <- x.common
                 dimnames(x.hap) <- list(1:nrow(x.hap),haplo.common)
                 if(length(haplo.rare)>0 & sum(haplo.freq.rare) > sum.rare.min &
                    keep.rare.haplo == TRUE) 
                 {
                   x.hap <- cbind(x.hap, apply(x.rare, 1, sum))
                   dimnames(x.hap) <- list(1:nrow(x.hap),c(haplo.common,"rare"))
                   haplo.rare.term <- TRUE
                 }
               },
         dom = {
                 x.hap <- if(ncol(x.common)) 1*(x.common >= 1) else x.common
                 dimnames(x.hap) <- list(1:nrow(x.hap),haplo.common)
                 if(length(haplo.rare)>0 & sum(haplo.freq.rare) > sum.rare.min &
                    keep.rare.haplo == TRUE) 
                 {
                    x.hap <- cbind(x.hap, 1*(apply(x.rare, 1, sum) >= 1) )
                    dimnames(x.hap) <- list(1:nrow(x.hap),c(haplo.common,"rare"))
                    haplo.rare.term <- TRUE
                 }
               },
         rec = {
                  x.hap <- if(ncol(x.common)) 1*(x.common == 2) else x.common
                  dimnames(x.hap) <- list(1:nrow(x.hap),haplo.common)
                  if(length(haplo.rare)>0 & sum(haplo.freq.rare) > sum.rare.min &
                      keep.rare.haplo == TRUE)
                  {
                    x.hap <- cbind(x.hap, 1*(apply(x.rare, 1, sum) == 2))
                    dimnames(x.hap) <- list(1:nrow(x.hap),c(haplo.common,"rare"))
                    haplo.rare.term <- TRUE
                  }

                  # because coding rec can result in columns of 0's, we need to check and
                  # exclude cols of 0's

                  ok <- apply(x.hap,2,sum) > 0
                  colname.ok <- dimnames(x.hap)[[2]][ok]
                  
                  if(sum(ok)==0) {
                    stop("No homozygotes for rec haplo.effect")
                  } else if(sum(ok)>0) {
                  
                     # subset x.hap, its dimnames and haplo.common      <jps>
                     # must also protect against 1 column left, don't drop to vector
                    x.hap <- x.hap[,ok, drop=FALSE]
                    dimnames(x.hap)[[2]] <- colname.ok
                    if(length(haplo.common)) haplo.common <- haplo.common[ok[1:length(haplo.common)]]
                  }
                },
          stop("Method for haplo.effect not supported")
  )


  # add a '.' before haplotype column name, so that names will be
  # cleaner when concatenating is done with other names

  x.names <- dimnames(x.hap)[[2]]
  haplo.names <- paste(haplo.names, x.names, sep=".")
  
  dimnames(x.hap)[[2]] <- paste(".",dimnames(x.hap)[[2]],sep="")

  class(x.hap) <- "model.matrix"

  # Now expand model.frame (by repeating rows) to account for enumerated haplotypes
  m <- m[indx.subj,]

  # replace the model.matrix object with haplotype design matrix,
  # keeping the old name of the chosen variable (model.matrix)

  m[[gindx]] <- x.hap
  attr(m,"row.names") <- 1:nrow(m)

  # The following code is used to create the allele labels for haplotypes. For R, 
  # we need to use allele.lev (list of vectors, where each vector is the allele
  # labels for a locus). For S, we can either do the same, or rely on S's model.frame
  # to have taken care of this for us, when model.frame was called within haplo.glm

  if(is.null(allele.lev)){
      stop("Missing allele.lev = list of vectors for labels of alleles\nCheck par list for haplo.glm")
  }

  nloci <- ncol(hapEM$haplotype)
  haplo.unique <- NULL
  for(j in 1:nloci){
      haplo.unique <- cbind(haplo.unique, allele.lev[[j]][as.numeric(hapEM$haplotype[,j])] )
  }
  dimnames(haplo.unique) <- dimnames(hapEM$haplotype)
  
  return(list(m.frame = m,
              g.dat = g.dat,
              haplo.unique = haplo.unique, 
              haplo.base = haplo.base,
              haplo.freq = haplo.freq,
              haplo.common = haplo.common,
              haplo.rare = haplo.rare,
              haplo.rare.term = haplo.rare.term,
              haplo.names=haplo.names))
}
