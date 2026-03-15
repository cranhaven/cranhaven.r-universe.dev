#$Author: sinnwell $
#
#$Date: 2006/10/25 19:31:34 $
#
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/x.sexcheck.q,v 1.6 2006/10/25 19:31:34 sinnwell Exp $
#
#$Locker:  $
#
#$Log: x.sexcheck.q,v $
#Revision 1.6  2006/10/25 19:31:34  sinnwell
#change T to TRUE and F to FALSE
#
#Revision 1.5  2003/01/17 15:58:53  det01
#Modified routine to check for class model.matrix
#
#Revision 1.4  2003/01/13 16:50:11  det01
#Modified check.geno data structure
#
#Revision 1.3  2002/12/13 17:49:44  det01
#*** empty log message ***
#
#
x.sexcheck <- function(x,sex,stop=FALSE) {
  #Title: Consistency checks for x.linked loci

  if (stop) {
    old.options <- options(warn=2)
    on.exit(options(old.options))
  }
    
  if (missing(x)) {
    warning("required argument x not found")
    return(TRUE)
  }

  if (missing(sex)) {
    warning("required argument sex not found")
    return(TRUE)
  }
  
  if (!("model.matrix" %in% class(x))) {
    warning("argument x must be of class locus")
    return(TRUE)
  }

  if (!attr(x,"x.linked"))
    return(FALSE)

  if (length(sex) != nrow(x)) {
    warning("length of sex not equal to number of individuals in x")
    return(TRUE)
  }

  if (any(is.na(sex))) {
    warning("missing a sex code for one or more individuals in x")
    return(TRUE)
  }

  male.code=attr(x,"male.code")
  female.code=attr(x,"female.code")
  if (any(!(sex==male.code | sex==female.code))) {
    warning("one or more values in sex do not match male.code or female.code attributes of x")
    return(TRUE)
  }

  male <- sex==male.code
  if(sum(male)>0) {
    miss <- is.na(x)
    check.geno <- (male & !miss)
    vec.check.geno <- apply(check.geno,1,sum)
    check.geno <- vec.check.geno>0
    if(any(x[check.geno,1]!=x[check.geno,2])) {
      warning("heterozygous male found for x.linked loci")
      return(TRUE)
    }
  }

  return(FALSE)
}

    
  
