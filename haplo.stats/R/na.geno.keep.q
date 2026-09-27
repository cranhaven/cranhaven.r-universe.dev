#$Author: sinnwell $
#$Date: 2008/03/24 21:24:53 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/na.geno.keep.q,v 1.4 2008/03/24 21:24:53 sinnwell Exp $
#$Locker:  $
#$Log: na.geno.keep.q,v $
#Revision 1.4  2008/03/24 21:24:53  sinnwell
#rm geno rows missing all.
#Put back on geno attributes, which removes requirement of allele.lev in haplo.glm
#add yxmiss and gmiss attributes to m for missing yx or geno rows
#
#Revision 1.3  2004/02/26 17:31:02  schaid
#changed F to FALSE
#
#Revision 1.2  2003/12/03 15:38:36  schaid
#fixed subsetting to response & covariates to not drop dim, to retain matrix class
#
#Revision 1.1  2003/09/16 16:03:08  schaid
#Initial revision
#
na.geno.keep <- function(m) {
  
  # determine which item in a model.frame is the genotype matrix
  gindx <- mf.gindx(m)

  # ignore genotype matrix when determining missing values for all
  # other variables (response and other covaraites)
  yxmiss <- apply(is.na(m[, -gindx, drop=FALSE]),1,any)
  
  # remove rows from genotype matrix that have all alleles missing
  gmiss <- apply(is.na(m[,gindx, drop=FALSE]), 1, all)
  
  # save attributes of geno, rows of m removed they would be dropped
  genoAttr <- attributes(m[,gindx])

  allmiss <- yxmiss | gmiss
  m <- m[!allmiss,]

  # make adjustments to attributes b/c of subset
  genoAttr$dim[1] <- genoAttr$dim[1] - sum(allmiss)

  # if an allele is removed, remove it from unique.alleles
  nloc <- ncol(m[,gindx])/2
  for(k in 1:nloc) {
    ualleles <- unique(c(m[,gindx][,(2*nloc-1)],m[,gindx][,2*nloc]))
    nalleles <- length(genoAttr$unique.alleles[[k]])
    if(length(ualleles) < length(genoAttr$unique.alleles[[k]])) {
      genoAttr$unique.alleles[[k]] <- genoAttr$unique.alleles[[k]][!is.na(match(1:nalleles, ualleles))]
    }
  }
  
  # put back on attributes
  for(att in names(genoAttr)) {
    attr(m[,gindx], att) <- genoAttr[[att]]
  }
  
  attr(m, "yxmiss") <- yxmiss
  attr(m, "gmiss") <- gmiss
  return(m)

}
