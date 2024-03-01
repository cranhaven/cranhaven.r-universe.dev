print.summary.smoothexposccs <-
  function(x, digits = max(getOption('digits')-3, 3),  
           signif.stars = getOption("show.signif.stars"), ...) {
    if (!is.null(x$call)) {
      cat("Call:\n")
      dput(x$call)
      cat("\n")
    }
    if (!is.null(x$fail)) {
      cat(" Coxreg failed.", x$fail, "\n")
      return()
    }
    savedig <- options(digits = digits)
    on.exit(options(savedig))
    
    omit <- x$na.action
    #cat("  n=", x$n)
    if (!is.null(x$nevent)) cat(", number of events=", x$nevent, "\n")
    else cat("\n")
    if (length(omit))
      cat("   (", naprint(omit), ")\n", sep="")
    
    if (nrow(x$coef)==0) {   # Null model
      cat ("   Null model\n")
      return()
    }
    
    
    if(!is.null(x$coefficients)) {
      cat("\n")
      printCoefmat(x$coefficients, digits=digits,
                   signif.stars=signif.stars, ...)
    }
    if(!is.null(x$conf.int)) {
      cat("\n")
      print(x$conf.int)
    }
    cat("\n")
    
    if (!is.null(x$concordance)) {
      cat("Concordance=", format(round(x$concordance[1],3)),
          " (se =", format(round(x$concordance[2], 3)),")\n")
    }
    
    #cat("Smoothing parameter of the spline based age relative incidence = ", x$smoothingpara)
    
    cat("Spline based exposure relative incidence function:", "\n", "Smoothing parameter = ", x$smp, "\n", "Cross validation score = ", x$crossvalidation, "\n")
    
    
    
    invisible()
  }
