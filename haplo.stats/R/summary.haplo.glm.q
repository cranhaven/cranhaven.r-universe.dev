#$Author: sinnwell $
#$Date: 2011/11/10 15:29:40 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/summary.haplo.glm.q,v 1.1 2011/11/10 15:29:40 sinnwell Exp $
#$Locker:  $
#$Log: 
#$

summary.haplo.glm <- function(object, show.all.haplo=FALSE, show.missing=FALSE, ...){

  ## Scaled Residuals, and dispersion (a.phi)
  residList <- residScaledGlmFit(object)

  ## Deviance residuals
  d.resids <- residuals.haplo.glm(object, type="deviance")
  ## use residuals.haplo.glm to get deviance residuals
  
  ## Build haplotype frequency data.frame
  
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

  ncoef <- length(object$coef)
  coef <- object$coef
  se <- sqrt(object$var.mat[cbind(1:ncoef, 1:ncoef) ])

  wt <- object$prior.weights * object$haplo.post.info$post
  df.residual <- sum(wt) - length(object$coef) 
  df.null <- sum(wt) - 1
  
  t.stat <- coef/se
  pval <- 2*(1-pt(abs(t.stat),  df.residual))


  coeff.df <- cbind(coef=coef, se=se, t.stat=t.stat, pval=pval)

  aliased <- is.na(coef(object))
  
  ## build modeled haplotypes data.frame
  hap.df <- haplo.df(object)
    
  ## build all.haplo data.frame
  if(show.all.haplo){
    haplo.type <- rep(NA,length(object$haplo.freq))
    haplo.type[object$haplo.common] <- "C"
    haplo.type[object$haplo.rare] <- "*"
    haplo.type[object$haplo.base] <- "B"
    full.haplotypes <- data.frame(object$haplo.unique,
        hap.freq = object$haplo.freq, hap.type=haplo.type)
    
  } else {
    full.haplotypes=NULL
  }

  if(show.missing) {
    miss.tbl <- apply(1*object$missing, 2, sum)
  } else {
    miss.tbl <- NULL
  }

  keep <- match(c("call", "terms", "family", "deviance", "null.deviance",
                  "aic", "iter"), names(object))

  ans <- c(object[keep], list(df.residual=df.residual, 
                      df.null=df.null, dispersion=residList$a.phi,
                      deviance.resid=d.resids, aliased=aliased,
                      coefficients=coeff.df, cov.scaled=object$var.mat,
                      haplotypes=hap.df, full.haplotypes=full.haplotypes,
                      missing=miss.tbl))

  class(ans) <- "summary.haplo.glm"
  
  return(ans)

}


print.summary.haplo.glm <- function(x,digits=max(getOption("digits")-3,3), ...)
{

  ## print standard summary.glm, then extra haplotype and missing information
  ## from haplo.glm
  tmp <- x
  class(tmp) <- "summary.glm"
  print(tmp, digits=digits, ...)
  
  cat("\nHaplotypes:\n")
  print(x$haplotypes, digits=digits)
  
  if(!is.null(x$full.haplotypes)) {
    cat("\nAll Haplotypes:\n")
    print(x$full.haplotypes, digits=digits)
    cat("\n")
    cat("  B = base   haplotype\n")
    cat("  C = common haplotype\n")
    cat("  * = rare   haplotype\n")   
  }
  
  if(!is.null(x$missing)) {
    cat("\nMissing Data:\n")
    cat("  Total subjects removed by NAs in y or x, \n")
    cat("  or all NAs in geno\n\n")
    print(x$missing)
  }  

  cat("\n")
  
  invisible(x)
  
}
