hyptest <-
  function(lmobj, p, xi = 0, type = "both"){
    estimate <- as.numeric(t(p)%*%lmobj$coef)
    stderr <- as.numeric(sqrt(t(p)%*%vcov(lmobj)%*%p))
    tstat <- (estimate - xi) / stderr
    degf <- summary(lmobj)$df[2]
    if (type == "upper") pvalue <- 1 - pt(tstat, degf)
    if (type == "lower") pvalue <- pt(tstat, degf)
    if (type == "both") pvalue <- 2 * (1 - pt(abs(tstat), degf))
    testresult <- cbind(estimate, stderr, tstat, degf, pvalue)
    return(testresult)
  }
