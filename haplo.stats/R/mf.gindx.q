#$Author: sinnwell $
#$Date: 2003/12/08 20:05:21 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/mf.gindx.q,v 1.2 2003/12/08 20:05:21 sinnwell Exp $
#$Locker:  $
#$Log: mf.gindx.q,v $
#Revision 1.2  2003/12/08 20:05:21  sinnwell
#changed rep(F, to rep(0,nvars)
#
#Revision 1.1  2003/09/16 16:02:57  schaid
#Initial revision
#
 mf.gindx <- function(m){
    # determine which item in a model.frame is the genotype matrix
    nvars<- length(m)
    typevars <- rep(0,nvars)
    for(i in 1:nvars){
      typevars[i] <- data.class(m[[i]])
    }

   gindx <- (1:nvars)[typevars=="model.matrix" | typevars=="matrix"]

   if(length(gindx)==0) stop("No geno matrix in data frame")
   if(length(gindx) >1) stop("More than 1 geno matrix in data frame")
   return(gindx)
  }
