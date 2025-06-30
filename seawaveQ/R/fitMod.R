#' Internal function that fits the seawaveQ model.
#' 
#' fitMod is called from within \link{fitswavecav} but
#' can be invoked directly. It fits the seawaveQ model and returns the 
#' results.
#' @param cdatsub is the concentration data.
#' @param cavdat is the continuous (daily) ancillary data.
#' @param yrstart is the starting year of the analysis (treated as January
#' 1 of that year).  
#' @param yrend is the ending year of the analysis (treated as December 31
#' of that year).
#' @param tndbeg is the beginning (in whole or decimal years) of the 
#' trend period. 
#' @param tndend is the end (in whole or decimal years) of the trend 
#' period. 
#' @param tanm is a character identifier that names the trend 
#' analysis run.  It is used to label output files.
#' @param pnames is the parameter (water-quality constituents) to 
#' analyze (if using USGS parameters, omit the starting 'P', such as 
#' "00945" for sulfate).  
#' @param qwcols is a character vector with the beginning of the
#' column headers for remarks code (default is R), and beginning of 
#' column headers for concentration data (default is P for parameter).
#' @param mclass indicates the class of model to use.
#' A class 1 model is the the traditional SEAWAVE-Q model that has a
#' linear time trend. A class 2 model is a newer option for longer
#' trend periods that uses a set of restricted cubic splines on the 
#' time variable to provide a more flexible model. The default is 1.
#' (Harrell, 2010, 2018). 
#' @param numk is the number of knots in the restricted cubic spline model
#' (mclass = 2). The default is 4, and the recommended number is 3--7.
#' @param plotfile is by default FALSE. True will write pdf files of plots to 
#' the user's file system.
#' @param textfile is by default FALSE. True will write text output files
#' to the user's file system. These files are useful for detailed model 
#' comparisons, documenting session information, and for model archives.
#' @keywords models
#' @keywords regression
#' @keywords survival
#' @keywords ts
#' @keywords multivariate
#' @author Aldo V. Vecchia and Karen R. Ryberg
#' @return A PDF file (if plotfile is TRUE) containing plots (see 
#' \code{\link{seawaveQPlots}}), a text file showing the best model survival 
#' regression call and results, and a list. The first element of the list 
#' contains information about the data and the model(s) selected (see 
#' \code{\link{examplestpars}}). The second element of the list contains 
#' the summary of the survival regression call. The third element of the 
#' list is itself a list containing the observed concentrations (censored 
#' and uncensored) and the predicted concentrations used by 
#' \code{\link{seawaveQPlots}} or \code{\link{seawaveQPlots2}} to generate the 
#' plots.
#' @export
#' @examples
#' data(swData)
#' myRes <- fitMod(cdatsub = examplecdatsub, cavdat = examplecavdat, 
#' yrstart = 1995, yrend = 2003, tndbeg = 1995, tndend = 2003, tanm = "myfit3", 
#' pnames = c("04041"), qwcols = c("R", "P"), plotfile = FALSE,
#' textfile = FALSE)
#' @references
#' Allison, P.D., 1995, Survival analysis using the SAS system---A 
#' practical guide: Cary, N.C., SAS Institute, Inc., 304 p.
#' 
#' Harrell, F.E., Jr., 2010, Regression modeling strategies---With
#' applications to linear models, logistic regression, and survival
#' analysis: New York, Springer-Verlag, 568 p.
#' 
#' Harrell, F.E., Jr., 2018, rms---Regression modeling strategies: 
#' R package version 5.1-2, \url{https://CRAN.R-project.org/package=rms}.
fitMod <- function(cdatsub, cavdat, yrstart, yrend, tndbeg, tndend, tanm, 
                   pnames, qwcols, mclass = 1, numk = 4, plotfile = FALSE,
                   textfile = FALSE) {
  yr <- cdatsub[[1]]
  mo <- cdatsub[[2]]
  da <- cdatsub[[3]]
  dyr <- yr + (mo - 1) / 12 + (da - 0.5) / 366
  yrpr <- cavdat[[1]]
  mopr <- cavdat[[2]]
  dapr <- cavdat[[3]]
  dyrpr <- yrpr + (mopr - 1) / 12 + (dapr - 0.5) / 366
  ccol <- paste(qwcols[2], pnames, sep = "")
  clog <- log10(cdatsub[, ccol])
  cencol <- paste(qwcols[1], pnames, sep = "")
  centmp <- cdatsub[, cencol] == '<'
  
  #  set up matrix with continuous variables
  if (length(cdatsub[1,]) > 6) {
    cavmat <- as.matrix(cdatsub[, 7:length(cdatsub[1, ])])   
  } else {
    cavmat <- as.matrix(cdatsub)
  }
  # compute variables for decimal season, year, and trend
  tseas <- dyr - floor(dyr)
  tyr <- dyr
  tyrpr <- dyrpr
  tseaspr <- (dyrpr - floor(dyrpr))
  tmid <- (tndbeg + tndend) / 2
  tndlin <- tyr - tmid
  tndlin[tyr < tndbeg] <- tndbeg - tmid
  tndlin[tyr > tndend + 1] <- tndend - tmid 
  
  if (mclass == 2) {
    # create a linear tail-restricted cubic spline function,  
    # with a particular number of knots, 
    # using the observation dates in the trend period
    tndrcs <- rcs(tndlin, numk)
  
    # create the time series for continuous (daily) modeling of 
    # pesticide concentrations and create a linear tail-restricted 
    # cubic spline function, using the knots from the 
    # data used to build the model
    tndlinpr <- tyrpr - tmid
    tndlinpr[tyrpr < tndbeg] <- tndbeg - tmid
    tndlinpr[tyrpr > tndend + 1] <- tndend - tmid
    tndrcspr <- rcs(tndlinpr, attributes(tndrcs)$parms)
  } else {
    # create the time series for continuous (daily) modeling of 
    # pesticide concentrations 
    tndlinpr <- tyrpr - tmid
    tndlinpr[tyrpr < tndbeg] <- tndbeg - tmid
    tndlinpr[tyrpr > tndend + 1] <- tndend - tmid
  }
  
  # find cmaxt (decimal season of max concentration)
  tmpsm <- supsmu(tseas, clog)
  xsm <- tmpsm$x
  ysm <- tmpsm$y
  nsm <- length(ysm)
  cmaxt <- xsm[order(ysm)[nsm]]
  
  if (mclass == 2) {
    # nexvars is the number of explanatory variables (wave, trend, 
    # and continuous variables, if any)
    # stpars and aovout store the model output
    nexvars <- 2 + (length(cdatsub[1, ]) - 6) + (numk - 1) 
    stpars <- matrix(nrow = 2, ncol = (4 + 2 * nexvars + (numk - 1) + 1))
    aovout <- vector("list", 1)
    aicout <- vector("list", 2)
    bicout <- vector("list", 2)
  
    # parx and aovtmp are temporary objects to store results 
    # for 56 model possibilities
    parx <- matrix(nrow = 56, ncol = (dim(stpars)[[2]] - 1))
    aovtmp <- vector("list", 56)
    aictmp <- vector("list", 56)
    bictmp <- vector("list", 56)
  
    # ready to loop through 56 model choices 
    # (14 models x 4 halflives)
    wvmsg <- paste("Computing the best seasonal wave.")
    message(wvmsg)
    for (j in 1:14) {
      for (k in 1:4) {
        j2 <- (j - 1) * 4 + k
        awave <- compwaveconv(cmaxt, j, k)
        ipkt <- floor(360 * tseas)
        ipkt[ipkt == 0] <- 1
        wavest <- awave[ipkt]
        ipkt <- floor(360 * tseaspr)
        ipkt[ipkt == 0] <- 1
        wavestpr <- awave[ipkt]
        indcen <- !centmp
        intcpt <- rep(1, length(wavest))
        xmat <- cbind(intcpt, wavest, tndrcs)
        if (length(cdatsub[1, ]) > 6) {
          xmat <- cbind(xmat, cavmat)
        }
        nctmp <- length(xmat[1, ])
        clogtmp <- clog
      
        # requires survival package
        tmpouta <- survreg(Surv(time = clogtmp, time2 = indcen, 
                                type = "left") ~ xmat - 1, dist = "gaussian",
                           control = list(maxiter = 100))
        parx[j2, ] <- c(mclass, j2, tmpouta$scale, tmpouta$loglik[2], 
                        tmpouta$coef, summary(tmpouta)$table[1:nctmp, 2], 
                        summary(tmpouta)$table[grep("tndlin", 
                                                    row.names(summary(tmpouta)$table)), 4])
        aovtmp[[j2]] <- summary(tmpouta)
        aictmp[[j2]] <- extractAIC(tmpouta)[2]
        bictmp[[j2]] <- extractAIC(tmpouta, 
                                   k = log(length(tmpouta$linear.predictors)))[2]
      }
    }
  } else if (mclass == 1) {
    # nexvars is the number of explanatory variables (wave, trend, 
    # and continuous variables, if any)
    # stpars and aovout store the model output
    nexvars <- 2 + length(cdatsub[1,]) - 6
    stpars <- matrix(nrow = 2,ncol = 6 + 2 * (nexvars + 1))
    aovout <- vector("list", 1)
    aicout <- vector("list", 2)
    bicout <- vector("list", 2)
    
    # parx and aovtmp are temporary objects to store results 
    # for 56 model possibilities
    parx <- matrix(nrow = 56, ncol = 5 + 2 * (nexvars + 1))
    aovtmp <- vector("list", 56)
    aictmp <- vector("list", 56)
    bictmp <- vector("list", 56)
    
    # ready to loop through 56 model choices 
    # (14 models x 4 halflives)
    wvmsg <- paste("Computing the best seasonal wave.")
    message(wvmsg)
    for (j in 1:14) {
      for (k in 1:4) {
        j2 <- (j - 1) * 4 + k
        awave <- compwaveconv(cmaxt, j, k)
        ipkt <- floor(360 * tseas)
        ipkt[ipkt == 0] <- 1
        wavest <- awave[ipkt]
        ipkt <- floor(360 * tseaspr)
        ipkt[ipkt == 0] <- 1
        wavestpr <- awave[ipkt]
        indcen <- !centmp
        intcpt <- rep(1, length(wavest))
        xmat <- cbind(intcpt, wavest, tndlin)
        if (length(cdatsub[1, ]) > 6) { 
          xmat <- cbind(xmat, cavmat) 
        }
        nctmp <- length(xmat[1, ])
        clogtmp <- clog
        
        # requires survival package
        tmpouta <- survreg(Surv(time=clogtmp, time2=indcen, 
                                type="left") ~ xmat - 1, dist = "gaussian",
                            control = list(maxiter = 100))
        parx[j2,] <- c(mclass, j2, tmpouta$scale, tmpouta$loglik[2], 
                       tmpouta$coef, summary(tmpouta)$table[1:nctmp, 2], 
                       summary(tmpouta)$table["xmattndlin", 4])
        aovtmp[[j2]] <- summary(tmpouta)
        aictmp[[j2]] <- extractAIC(tmpouta)[2]
        bictmp[[j2]] <- extractAIC(tmpouta, 
                                   k = log(length(tmpouta$linear.predictors)))[2]
      }
    }
  } else {
    message("Model class is unknown")
  }
  # find largest likelihood (smallest negative likelihood)
  likx <- (-parx[, 4])
  # eliminate models with negative coefficient for the seasonal wave
  likx[parx[, 6] < 0] <- NA
  # This could be used to penalize models with two seasons of application
  likx[25:56] <- likx[25:56] + 0
  pckone <- order(likx)[1]
  stpars[1, ] <- c(parx[pckone, ], cmaxt)
  aovout[[1]] <- aovtmp[[pckone]]
  aicout[[1]] <- aictmp[[pckone]]
  bicout[[1]] <- bictmp[[pckone]]
  
  # generalized r-squared statistic based on the likelihood-ratio test
  # see Allison (1995, pp. 247-249)
  # Allison, Paul D. 1995. Survival Analysis Using the SAS System: 
  # A Practical Guide. Cary, NC: SAS Institute Inc.
  lrt <- -2 * aovout[[1]]$loglik[1] - (-2 * aovout[[1]]$loglik[2])
  r2 <- round(1 - exp(-lrt / aovout[[1]]$n), digits = 2)
  
  if (textfile == TRUE) {
    regCallFile <- paste(tanm, "_survregCall.txt", sep = "")
    resmsg <- paste("Final model survreg results saved to ", regCallFile, ".", 
                    sep = "")
    message(resmsg)
    si <- sessionInfo()
    sink(regCallFile, append = TRUE, type = "output")
    cat("\n\n", format(Sys.time(), "%A %d %b %Y %X %p %Z"), sep = "")
    cat("\n", si$R.version$version.string, 
        "\n", si$otherPkgs[[grep("seawaveQ", si$otherPkgs)]]$Package,
        " version ", si$otherPkgs[[grep("seawaveQ", si$otherPkgs)]]$Version,
        "\n", si$platform, 
        "\n\nFinal model survreg results for ", pnames, sep = "")
    print(aovout[[1]])
    cat("Generalized r-squared is: ", r2, "\n", sep = " ")
    cat("AIC (Akaike's An Information Criterion) is: ", round(aicout[[1]], 
                                                              digits = 2), 
        "\n", sep = " ")
    cat("BIC (Bayesian Information Criterion) is: ", round(bicout[[1]], 
                                                           digits = 2), 
        "\n", sep = " ")
    jmod <- floor((stpars[1, 2] - 1) / 4) + 1
    hlife <- stpars[1, 2] - (jmod - 1) * 4
    cat("Model class is ", mclass, "\nPulse input function is ", jmod,
        "\nHalf life is ", hlife, 
        "\nSeasonal value of the maximum concentration is ", 
        round(cmaxt, digits = 2), ".", "\n", sep = "")
    sink()
  }
  
  if (mclass == 2) {
    
    plotDat <- seawaveQPlots2(stpars, cmaxt, tseas, tseaspr, tndrcs, tndrcspr, 
                              cdatsub, cavdat, cavmat, clog, centmp, yrstart, 
                              yrend, tyr, tyrpr, pnames, tanm, mclass = 2, 
                              numk = numk, plotfile)
    # myRes <- list(stpars, aovout, plotDat, tndrcspr)
    myRes <- list(stpars, aovout, plotDat)
    myRes
  } else if (mclass == 1) {
    plotDat <- seawaveQPlots(stpars, cmaxt, tseas, tseaspr, tndlin,
                              tndlinpr, cdatsub, cavdat, cavmat, clog, centmp,
                              yrstart, yrend, tyr, tyrpr, pnames, tanm, mclass=1, 
                             plotfile)
    myRes <- list(stpars, aovout, plotDat)
    myRes
  } else {
    message("Model class unknown.")
  }
  myRes
}
