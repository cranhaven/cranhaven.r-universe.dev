######################   begin mph.summary ######################################
#
mph.summary <- function(mph.out,cell.stats=FALSE,model.info=FALSE,digits=4) {
  #
  #  This function is used in conjunction with the ML fitting
  #  function 'mph.fit', versions 3.0 and higher.
  #
  #  It computes and prints a collection of summary statistics of
  #  the fitted MPH model given in 'mph.out',
  #  which is the result of 'mph.fit'.  That is,
  #  mph.out <- mph.fit(y,...)
  #
  #  Author:  Joseph B. Lang,
  #           Dept of Statistics
  #              and Actuarial Science
  #           Univ of Iowa, Iowa City, IA 52242
  #           8/16/01
  #  Last Updated:  1/17/08, 4/30/09,  5/17/09
  #
  #  INPUT
  #     Required:
  #          mph.out = result of `mph.fit'
  #
  #     Optional:
  #          cell.stats = logical variable indicating whether cell
  #                       specific statistics are to be output
  #                       (default: cell.stats=F)
  #          model.info = logical variable indicating whether model
  #                       information is to be output
  #                       (default: model.info=F)
  #          digits     = integer giving output precision;
  #                       used in the round() function
  #
  #
  a <- mph.out
  if (a$h.mean==FALSE) {
    cat("\nMODEL GOODNESS OF FIT:   Test of   Ho: h(p)=0 vs. Ha: not Ho...")
  }  else {cat("\nMODEL GOODNESS OF FIT:   Test of   Ho: h(m)=0 vs. Ha: not Ho...")}
  cat("\n\n")
  cat("  Likelihood Ratio Stat (df=",a$df,"):  Gsq = ",
      round(a$Gsq,5))
  if (a$df > 0) cat(" (pval = ",signif(1-pchisq(a$Gsq,a$df),4),")")
  cat("\n")
  cat("  Pearson's Score Stat  (df=",a$df,"):  Xsq = ",
      round(a$Xsq,5))
  if (a$df > 0) cat(" (pval = ",signif(1-pchisq(a$Xsq,a$df),4),")")
  cat("\n")
  cat("  Generalized Wald Stat (df=",a$df,"):  Wsq = ",
      round(a$Wsq,5))
  if (a$df > 0) cat(" (pval = ",signif(1-pchisq(a$Wsq,a$df),4),")")
  cat("\n")
  sm <- 100*length(a$m[a$m < 5])/length(a$m)
  if ((sm > 75)&(a$df > 0)) {
    cat("\n  WARNING:", paste(sm,"%",sep=""),
        "of expected counts are less than 5. \n")
    cat("           Chi-square approximation may be questionable.\n")
  }
  cat("\n")
  r <- sort(round(a$adjresid,3))
  lr <- length(r); nbig <- sum(abs(r) > 2)
  cat("  Adj Resids: ");
  if (lr > 4) {cat(sep=" ",r[1],r[2],"...",r[lr-1],r[lr],", ")}
  else {cat(sep=" ",r,", ")}
  cat(sep=" ","Number |Adj Resid| > 2: ", nbig)
  cat("\n")


  cat("\nSAMPLING PLAN INFORMATION...\n")
  cat("Number of strata:  ",ncol(a$triple$Z),"\n")
  cat("Strata identifiers: "); cat(sep=", ",names(table(a$strata))); cat("\n")
  cat("Strata with fixed sample sizes: "); cat(sep=", ",a$fixed.strata);cat("\n")
  cat("Observed strata sample sizes:   "); cat(sep=", ",t(a$triple$Z)%*%a$y);
  cat("\n")


  if (a$L[1] != "NA") {
    cat("\nLINEAR PREDICTOR MODEL RESULTS...")
    cat("\n")
    dcovbeta <- diag(a$covbeta); dcovbeta[dcovbeta < 0] <- 0; sbeta <- as.matrix(sqrt(dcovbeta))
    z <- a$beta/sbeta
    pval <- 2*(1-pnorm(abs(z)))
    dimnames(sbeta)[2] <- "StdErr(BETA)"
    dimnames(z)[2] <- "Z-ratio"
    dimnames(pval)[2] <- "p-value"
    print(cbind(round(a$beta,digits),round(sbeta,digits),round(z,digits),signif(pval,5)))
    cat("\n")
    dcovL <- diag(a$covL); dcovL[dcovL < 0] <- 0; stdL <- as.matrix(sqrt(dcovL))
    dimnames(stdL)[2] <- "StdErr(L)"
    print(cbind(round(a$Lobs,digits),round(a$L,digits),round(stdL,digits),round(a$Lresid,digits)))
  }
  if (cell.stats==TRUE) {
    dcovm <- diag(a$covm); dcovm[dcovm < 0] <- 0;  stdm <- as.matrix(sqrt(dcovm))
    dcovp <- diag(a$covp); dcovp[dcovp < 0] <- 0;  stdp <- as.matrix(sqrt(dcovp))
    dimnames(stdm)[2] <- "StdErr.FV"
    dimnames(stdp)[2] <- "StdErr.PROB"
    a$strata <- as.matrix(a$strata)
    dimnames(a$strata)[2] <- list("strata")
    cat("\nCELL-SPECIFIC STATISTICS...")
    cat("\n")
    print(data.frame(a$strata,a$y,round(a$m,digits),round(stdm,digits),round(a$p,digits),round(stdp,digits),round(a$adjresid,digits)))
  }

  cat("\nCONVERGENCE INFORMATION...")
  cat("\n")
  cat(" ",a$warn.message,"\n")
  # cat("  iterations =",a$iter, ",  time elapsed =",a$time.elapsed)
  cat("\n")
  cat("  norm.diff  =",signif(a$norm.diff,6), "= dist between last and second last iterates.\n")
  cat("              ",a$warn.message.diff)
  cat("  norm.score =",signif(a$norm.score,6), "= norm of score at last iteration.\n")
  cat("              ",a$warn.message.score)


  if (model.info==TRUE) {
    cat("\nMODEL INFORMATION...")

    cat("\n")
    if (a$L[1] != "NA") {
      cat("Linear Predictor Model Link Function  L.fct (L.mean=", a$L.mean,"):\n")
      print(a$L.fct)
      cat("\n")
      cat("Link information as originally input, L.input.fct:\n")
      print(a$L.input.fct)
      cat("\n")
      cat("Derivative of Transpose Link Function derLt.fct: \n")
      print(a$derLt.fct)
      cat("\n")
      cat("Linear Predictor Model Design Matrix  X: \n")
      print(a$X)
      cat("\n")
      cat("U = Orthogonal Complement of X: \n")
      if (is.matrix(a$U)) print(a$U)
      else cat("\n  ",a$U,"\n")
    }
    cat("\n")
    cat("Constraint Function  h.fct (h.mean=", a$h.mean,"):\n")
    print(a$h.fct)
    cat("\n")
    cat("Constraint information as originally input, h.input.fct:\n")
    print(a$h.input.fct)
    cat("\n")

    cat("Derivative of Transpose Constraint Function derht.fct:\n")
    print(a$derht.fct)
    cat("\n")
    cat("Population (Strata) Matrix Z: \n")
    print(a$triple$Z)
    cat("\n")
    cat("Sampling Constraint Matrix ZF:\n")
    print(a$triple$ZF)
    cat("\n")
    cat("Fixed Sample Sizes n:\n")
    print(a$triple$n)
    cat("\n")

    cat("strata:\n")
    print(a$strata)
    cat("\n")
    cat("fixed.strata:\n")
    print(a$fixed.strata)
    cat("\n")
  }
  cat("\n")
}
######################   end mph.summary     ######################################

