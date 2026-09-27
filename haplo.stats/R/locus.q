#$Author: sinnwell $
#
#$Date: 2011/02/21 20:09:27 $
#
#$Header: /projects/genetics/cvs/cvsroot/mgenet/R/locus.q,v 1.1.1.1 2011/02/21 20:09:27 sinnwell Exp $
#
#$Locker:  $
#
#$Log: locus.q,v $
#Revision 1.1.1.1  2011/02/21 20:09:27  sinnwell
#initial, genetics functions for Mayo BSI
#
#Revision 1.6  2003/12/08 19:49:00  sinnwell
#done
#changed T,F to TRUE,FALSE
#
#Revision 1.5  2003/09/19 15:40:09  sinnwell
#fix class and oldClass stuff for R/S compatibility
#
#Revision 1.4  2003/01/17 15:55:36  det01
#Removed setOldClass(c("locus","model.matrix")) code
#The code oldClass(geno) <- "locus" modified to oldClass(geno) <- "model.matrix"
#
#Revision 1.3  2002/12/13 17:47:35  det01
#Locus class inherits from model.matrix class
#
#
locus <- function(allele1,allele2,
                  chrom.label=NULL,
                  locus.alias=NULL,
                  x.linked=FALSE, sex=NULL, male.code="M", female.code="F", 
                  miss.val=NA) {

# Title: Create an object of locus class

  if (missing(allele1))
    stop("Error: allele1 is missing")
  
  if (missing(allele2)) {
    if (!is.matrix(allele1)) {
      stop("Error: allele1 not a matrix when allele2 is missing")
    }
    else {
      if (ncol(allele1)!=2) 
        stop("Error: allele1 must be matrix with 2 columns when allele2 is missing")
      allele2 <- allele1[,2]
      allele1 <- allele1[,1]
    }
  }

  if (length(allele1)!=length(allele2))
    stop("Error: allele vectors not of the same length") 

  # convert factor to character:
  if ((any(is.factor(allele1))) | (any(is.factor(allele2)))){
    allele1 <- as.character(allele1)
    allele2 <- as.character(allele2)
  }

  # fix miss.val so NA is kept as a missing code
  if (!any(is.na(miss.val)))
    miss.val <- c(miss.val,NA)
  
  n <- length(allele1)
  t <- factor(c(allele1,allele2),exclude=miss.val)
  a1 <- as.numeric(t[1:n])
  a2 <- as.numeric(t[(n+1):(2*n)])

  geno <- cbind(a1,a2)
  attr(geno,"chrom.label") <- chrom.label
  attr(geno,"locus.alias") <- locus.alias
  attr(geno,"x.linked") <- x.linked
  
  class(geno) <- "model.matrix"
  
  attr(geno,"allele.labels") <- levels(t)
  if (x.linked) {
    attr(geno,"male.code") <- male.code
    attr(geno,"female.code") <- female.code
    # Stop if x-linked related errors
    x.sexcheck(geno,sex,stop=TRUE)
  }
  
  return(geno)
}



