#$Author: sinnwell $
#$Date: 2011/02/21 19:08:17 $
#$Header: /projects/genetics/cvs/cvsroot/mgenet/R/geno1to2.q,v 1.1.1.1 2011/02/21 19:08:17 sinnwell Exp $
#$Locker:  $
#$Log: geno1to2.q,v $
#Revision 1.1.1.1  2011/02/21 19:08:17  sinnwell
#"initial, general functions for R genetics at Mayo BSI
#
#Revision 1.6  2007/05/14 15:33:24  sinnwell
#remove na.code
#
#Revision 1.5  2007/04/20 20:03:32  sinnwell
#replace unlistToMat with do.call("cbind"), use rep in assigning loc names
#
#Revision 1.3  2007/04/20 15:06:09  sinnwell
#rm na.code and miss.val, only allow 0,1,2 values, the rest are NA
# 
#Revision 1.2  2007/04/18 17:10:57  sinnwell
#add locus.label option
#
#Revision 1.1  2006/08/11 21:53:42  sinnwell
#Initial revision
#
## $Id: geno1to2.q,v 1.1.1.1 2011/02/21 19:08:17 sinnwell Exp $
  
## Jason Sinnwell, with contribution by Harold Ye
## Mayo Clinic, Division of Biostatistics
## 8/2006

## convert a geno matrix with allele counts to a matrix where each locus
## is represented by 2 columns.

geno1to2 <- function(geno, locus.label=NULL) {

  # define function to convert a vector or 0,1,2 (or NA) to alleles 1-1,1-2,2-2, respectively
  # to be used in apply statement below
  one2two <- function(macvec) {
    a1 <- ifelse(is.na(macvec), NA, ifelse(macvec==0,1,ifelse(macvec==2,2,1)))
    a2 <- ifelse(is.na(macvec), NA, ifelse(macvec==0,1,ifelse(macvec==2,2,2)))

    return(cbind(a1,a2))
  }
  
  # recode missing values to NA.
  ## Any value that is not 0, 1, 2, "0", "1", "2", is considered an NA
  values <- unique(as.vector(as.matrix(geno)))
  imatch <- match(values, c(0,1,2))
  cmatch <- match(values, c("0","1","2"))
  miss.vec <- values[ apply(is.na(cbind(imatch, cmatch)), 1, all) ]
  miss.notNA <- miss.vec[!is.na(miss.vec)]
  if(length(miss.notNA)) {
    for(val in miss.notNA) {
      geno[geno==val] <- NA
    }
  }
  
  # apply one2two
  two <- lapply(as.list(as.data.frame(geno)), one2two)
  twomat <- do.call("cbind", two)
  twomat <- as.data.frame(twomat)
  
  if(is.null(locus.label)) locus.label <- paste("m", 1:ncol(geno), sep="")
  
  names(twomat) <- paste(rep(locus.label,rep(2,length(locus.label))), c(1,2), sep=".")
  return(twomat)
  
}
