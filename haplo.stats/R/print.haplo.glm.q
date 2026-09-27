#$Author: sinnwell $
#$Date: 2011/11/10 15:29:40 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/print.haplo.glm.q,v 1.11 2011/11/10 15:29:40 sinnwell Exp $
#$Locker:  $
#$Log: print.haplo.glm.q,v $
#Revision 1.11  2011/11/10 15:29:40  sinnwell
#major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
#
#Revision 1.10  2008/04/04 16:10:18  sinnwell
#add show.missing and return coeffDF and hapDF by invisible
#
#Revision 1.9  2005/03/29 14:18:47  sinnwell
#fix call to print within widths, different for R/S+
#
#Revision 1.8  2004/10/22 22:08:27  sinnwell
#do not drop matrix to vector when subsetting to haplo.unique
#when only 1 haplotype
#
#Revision 1.7  2004/03/18 23:30:32  sinnwell
#keep matrix from converting to data.frame, and char vecs to factors
#
#Revision 1.6  2004/02/26 23:05:23  sinnwell
#print.banner to printBanner
#
#Revision 1.5  2004/02/06 16:34:17  sinnwell
#fix 1-sided pval to 2-sided
#
#Revision 1.4  2003/12/08 20:16:49  sinnwell
# changed T,F to TRUE,FALSE
#
#Revision 1.3  2003/11/17 23:28:19  schaid
#made compatible with R
#
#Revision 1.2  2003/10/15 21:13:30  schaid
#got rid of bug caused by use of 'fit' (should have been x)
#
#Revision 1.1  2003/09/16 16:03:15  schaid
#Initial revision
#
print.haplo.glm <- function(x, print.all.haplo=FALSE,
                            digits = max(options()$digits - 3, 5), ...){

  ## function to piece togeth haplotype data.frame
  haplo.df<- function(x){
    z <- x$haplo.common
    df <- as.matrix(x$haplo.unique[z,,drop=FALSE])
    y <- x$haplo.freq[z]

    if(x$haplo.rare.term){
      df <- rbind(df, rep("*",ncol(df)))
      y <- c(y, sum(x$haplo.freq[x$haplo.rare]))
    }

    # use dimnames to change row names do not convert from matrix to df
    dimnames(df)[[1]] <- x$haplo.names
    df <- rbind(df,x$haplo.unique[x$haplo.base,])
    dimnames(df)[[1]][nrow(df)] <- "haplo.base"
    y <- c(y,x$haplo.freq[x$haplo.base])
    data.frame(df,hap.freq=y)
  }


  cat("\nCall:  ", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  if(length(x$coef)) {
     cat("Coefficients:\n")
     print.default(format(x$coefficients, digits = digits), 
                   print.gap = 2, quote = FALSE)
   }
  else cat("No coefficients\n\n")

  cat("\nHaplotypes:\n")
  hap.df <- haplo.df(x)
  print(hap.df, digits=digits)
  
  wt <- x$prior.weights * x$haplo.post.info$post
  df.residual <- sum(wt) - length(x$coef) 
  df.null <- sum(wt) - 1

  cat("\nDegrees of Freedom: ", df.null, "Total (i.e. Null); ", 
      df.residual, "Residual\n")
   
  miss.tbl <- apply(1*x$missing, 2, sum)
  if(any(miss.tbl > 0)) {
    cat("\nSubjects removed by NAs in y or x, or all NA in geno\n") 
    print(miss.tbl)
  }

  cat("\n     Null Deviance: ", format(signif(x$null.deviance, digits)),
      "\n Residual Deviance: ", format(signif(x$deviance, digits)),
      "\n               AIC: ", format(signif(x$aic, digits)), "\n\n")

  
  invisible(x)
  ##invisible(list(coeffDF=coeff.df, hapDF=hap.df))

}

