#' Function to prepare data and fit the seawaveQ model.
#' 
#' Fits the seawaveQ model (Vecchia and others, 2008) using a seasonal 
#' wave and continuous ancillary variables (streamflow anomalies and other 
#' continuous variables such as conductivity or sediment) to model water 
#' quality. The version in the 2.0.0 update to the R package has an
#' option to use restricted cubic splines as a more flexible definition
#' of the temporal trend.
#' @name fitswavecav
#' @title Fit seasonal wave and continuous ancillary data for trend 
#' analysis
#' @note The assumed data format is one with columns for water-quality
#' concentration values and a related column for qualification of 
#' those values, such as in the case of left-censored values less 
#' than a particular value. For example, a water-quality sample
#' was collected and the laboratory analysis indicated that the 
#' concentration was less than 0.01 micrograms per liter. The 
#' USGS parameter code for simazine is 04035 (U.S. Geological Survey, 
#' 2018b). When the data are retrieved through the National Water 
#' Information System: Web Interface 
#' (\url{https://waterdata.usgs.gov/nwis}; U.S. Geological Survey, 2018a), 
#' the concentration values are in a column labeled P04035 and the 
#' qualification information, or remark codes, are in a column labeled 
#' R04035. To use this function, the argument pnames would be the unique 
#' identifier for simazine values and qualifications, 04035, and the 
#' qwcols argument would be c("R", "P") to indicate that the 
#' qualification column starts with an R and the values column starts with 
#' a P. \cr
#' 
#' Other users may have data in different formats that can be 
#' modified to use with this function. For example, a user may have
#' concentration values and qualification codes in one column, such
#' as a column labeled simazine with the values 0.05, 0.10, <0.01, 
#' <0.01, and 0.90. In this case, the less thans and any other 
#' qualification codes should be placed in a separate column. The
#' column names for the qualification codes and the concentration values
#' should be the same with the exception of different beginning
#' letters to indicate which column is which. The columns could be
#' named Rsimazine and Psimazine. Then the argument pnames = "simazine" 
#' and the argument qwcols = c("R", "P"). \cr
#' 
#' Users should exercise caution when their water-quality data have 
#' multiple censoring limits and may want to recensor the data to a 
#' single censoring level. Censoring and recensoring issues are discussed
#' in the text and Appendix 1 of Ryberg and others (2010).
#' 
#' None of the variations of SEAWAVE is a simple model. The model complexity 
#' increases with flow anomalies and with the addition of restricted cubic 
#' splines. As the number of parameters in the model increases or the degree
#' of censoring increases, the sample size must also increase. See Ryberg 
#' and others (2020) for more details on sample size.
#' @param cdat is the concentration data
#' @param cavdat is the continuous (daily) ancillary data
#' @param tanm is a character identifier that names the trend 
#' analysis run.  It is used to label output files.
#' @param pnames are the parameters (water-quality constituents) to 
#' analyze (omit the starting character, for example for sulfate data 
#' indicated by P00945, enter "00945").  
#' @param yrstart is the starting year of the analysis (treated as January
#' 1 of that year). Zero means the start date will be determined by the 
#' start date of cavdat, the continuous ancillary data.
#' @param yrend is the ending year of the analysis (treated as December 31
#' of that year). Zero means the end date will be determined by the end 
#' date of cavdat, the continuous ancillary data.
#' @param tndbeg is the beginning (in whole or decimal years) of the 
#' trend period. Zero means the begin date will be the beginning of the
#' concentration data, cdat.
#' @param tndend is the end of the trend (treated as December 31
#' of that year). Zero means the end date will be the end of the 
#' concentration data, cdat.
#' @param iwcav is a character vector indicating which continuous
#' ancillary variables to include, if none are used for analysis,
#' use iwcav=c("none").
#' @param dcol is the column name for the dates, should be the same for 
#' both cdat and cavdat
#' @param qwcols is a character vector with the beginning of the
#' column headers for remarks code (default is R), and beginning of 
#' column headers for concentration data (default is P for parameter).
#' @param mclass indicates the class of model to use.
#' A class 1 model is the traditional SEAWAVE-Q model that has a
#' linear time trend. A class 2 model is a newer option for longer
#' trend periods that uses a set of restricted cubic splines on the 
#' time variable to provide a more flexible model. 
#' @param numk is the number of knots in the restricted cubic spline model.
#' The default is 4, and the recommended number is 3--7.
#' @param alpha is the significance level or alpha values for statistical
#' significance and confidence intervals.
#' @param bootRCS is a logical value indicating whether or not to perform
#' block bootstrapping for an attained significance level for the trend
#' with restricted cubic splines. No bootstrapping is performed for the linear
#' trend model.
#' @param nboot is the number of bootstrap replicates. A large number, 10,000,
#' is recommended, but this takes a long time. 
#' @param plotfile is by default FALSE. True will write pdf files of plots to 
#' the user's file system.
#' @param textfile is by default FALSE. True will write text output files
#' to the user's file system. These files are useful for detailed model 
#' comparisons, documenting session information, and for model archives.
#' @keywords models regression ts survival multivariate
#' @return A PDF file containing plots of the data and modeled 
#' concentration, a text file containing a summary of the survival 
#' regression call for each model selected, and a list. The first element
#' of the list is a data frame described under format. The second element
#' of the list is the summary of the survival regression call. The third 
#' element is the observed concentration data (censored and uncensored). 
#' The fourth element is the concentration data predicted by the model.  
#' The fifth element provides summary statistics for the predicted 
#' concentrations. The sixth element is a data frame that provides a summary of 
#' the trends with the columns pname (parameter name), mclass (a value of 1, 
#' indicating a linear trend model, a value of 2 for models using restricted
#' cubic splines), and the columns describing the trends. For linear models  
#' see \code{\link{pesticideTrendCalcs}}. For models with restricted cubic
#' splines, see the format section. See Ryberg and York (2020) for additional
#' details.
#' @format The data frame returned as the first element of the output list has 
#' one row for each chemical analyzed and the number of columns depends on the 
#' number of continuous ancillary variables used. The general format is as 
#' follows: \cr
#' \tabular{lll}{
#'  pname \tab character \tab parameter analyzed\cr
#'  mclass \tab numeric \tab a value of 1 or 2\cr
#'  jmod \tab numeric \tab the choice of pulse input function, an 
#'  integer 1--14 \cr
#'  hlife \tab numeric \tab the model half-life in months, an integer, 1 to 
#'  4 months \cr
#'  cmaxt \tab numeric \tab the decimal season of maximum concentration \cr
#'  scl \tab numeric \tab the scale factor from the 
#'  \code{survreg.object} \cr
#'  loglik \tab numeric \tab the log-likelihood for the model \cr
#'  cint \tab numeric \tab coefficient for model intercept \cr
#'  cwave \tab numeric \tab coefficient for the seasonal wave \cr
#'  ctnd[alpahnumeric] \tab numeric \tab coefficient(s) for the trend component(s) of model \cr
#'  c[alphanumeric] \tab numeric \tab 0 or more coefficients for the 
#'  continuous ancillary variables\cr
#'  seint \tab numeric \tab standard error for the intercept \cr
#'  sewave \tab numeric \tab standard error for the seasonal wave \cr
#'  setnd[alphanumeric] \tab numeric \tab standard error for the trend component(s) \cr
#'  se[alphanumeric] \tab numeric \tab  0 or more standard errors for the 
#'  continuous ancillary variables\cr
#'  pvaltnd[alphanumeric] \tab numeric \tab the \emph{p}-value for the trend component(s) \cr
#' }
#' The data frame returned as the sixth element of the output list has 
#' one row for each chemical analyzed. The general format for linear models is
#' described in \code{\link{pesticideTrendCalcs}}. The format for restricted
#' cubic spline models is as described as follows: \cr
#' \tabular{lll}{
#'   pname \tab character \tab parameter analyzed\cr
#'  mclass \tab numeric \tab A value of 1 or 2\cr
#'  baseConc \tab numeric \tab the concentration at the beginning of the trend
#'  period \cr
#'  endCon \tab numeric \tab the concentration at the end of the trend
#'  period \cr
#'  rcsctndPpor \tab numeric \tab the concentration trend in percent over the 
#'  trend period \cr
#'  rcsctndOrigPORPercentBase \tab numeric \tab the conc. trend in 
#'  original units over period of record (calc. based on percent per 
#'  year and base conc.) \cr
#'  pvalrcstnd \tab numeric \tab \emph{p}-value, attained significance level, based on 
#'  bootstrapping \cr
#'  ctndlklhd \tab numeric \tab trend likelihood \cr
#' }
#' @seealso The functions that \code{fitswavecav} calls internally: \cr
#' \code{\link{prepData}} and \code{\link{fitMod}}.
#' @export
#' @author Aldo V. Vecchia and Karen R. Ryberg
#' @examples
#' data(swData)
#' modMoRivOmaha <- combineData(qwdat = qwMoRivOmaha, cqwdat = cqwMoRivOmaha)
#' myfitLinearTrend <- fitswavecav(cdat = modMoRivOmaha, cavdat = cqwMoRivOmaha, 
#' tanm = "myfitLinearTrend", pnames = c("04035", "04037", "04041"), yrstart = 1995, 
#' yrend = 2003, tndbeg = 1995, tndend = 2003, iwcav = c("flowa30", "flowa1"), 
#' dcol = "dates", qwcols = c("R", "P"))
#' # trend model results
#' myfitLinearTrend[[1]]
#' # example regression call
#' myfitLinearTrend[[2]][[1]]
#' # first few lines of observed concentrations
#' head(myfitLinearTrend[[3]])
#' # first few lines of predicted concentrations
#' head(myfitLinearTrend[[4]])
#' # summary statistics for predicted concentrations
#' myfitLinearTrend[[5]]
#' # summary of trends
#' myfitLinearTrend[[6]]
#' myfitRCSTrend <- fitswavecav(cdat = modMoRivOmaha, cavdat = cqwMoRivOmaha, 
#' tanm = "myfitRCSTrend", pnames = c("04035", "04037", "04041"), yrstart = 1995, 
#' yrend = 2003, tndbeg = 1995, tndend = 2003, iwcav = c("flowa30", "flowa1"), 
#' dcol = "dates", qwcols = c("R", "P"), mclass = 2, numk = 4, bootRCS = FALSE,
#' plotfile = FALSE, textfile = FALSE)
#' @references
#' Ryberg, K.R. and York, B.C., 2020, seawaveQ---An R package providing a model 
#' and utilities for analyzing trends in chemical concentrations in streams with 
#' a seasonal wave (seawave) and adjustment for streamflow (Q) and other 
#' ancillary variables: U.S. Geological Survey Open-File Report 2020--1082, 25 
#' p., with 4 appendixes.
#' 
#' Ryberg, K.R., Vecchia, A.V., Martin, J.D., and Gilliom, R.J., 2010, 
#' Trends in pesticide concentrations in urban streams in the United 
#' States, 1992--2008: U.S. Geological Survey Scientific Investigations 
#' Report 2010--5139, 101 p., \url{https://pubs.usgs.gov/sir/2010/5139/}.
#'
#' U.S. Geological Survey, 2018a, National Water Information System: 
#' Web Interface, accessed July 7, 2018, at
#' \url{https://waterdata.usgs.gov/nwis}.
#' 
#' U.S. Geological Survey, 2018b, Parameter code definition: National 
#' Water Information System: Web Interface, accessed July 18, 2018,
#' at \url{https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes}.
#' 
#' Vecchia, A.V., Martin, J.D., and Gilliom, R.J., 2008, Modeling 
#' variability and trends in pesticide concentrations in streams: 
#' Journal of the American Water Resources Association, v. 44, no. 5, p. 
#' 1308--1324, \url{https://dx.doi.org/10.1111/j.1752-1688.2008.00225.x}.
fitswavecav <- function(cdat, cavdat, tanm = "trend1", pnames, yrstart = 0, 
                        yrend = 0, tndbeg = 0, tndend = 0, iwcav = c("none"), 
                        dcol = "dates", qwcols = c("R", "P"), mclass = 1, 
                        numk = 4, alpha = 0.10, bootRCS = FALSE, 
                        nboot = 1000, plotfile = FALSE, textfile = FALSE) {
  # perform data checks and check arguments
  dtmes <- c("yrstart, yrend, tndbeg, tndend should all be numeric, \n 
             greater than or equal to 0.")
  if (!is.numeric(c(yrstart, yrend, tndbeg, tndend))) 
    stop(dtmes)
  if (yrstart < 0 | yrend < 0 | tndbeg < 0 | tndend < 0) 
    stop(dtmes)
  if (yrstart > yrend) {
    yrstart <- 0
    yrend <- 0
  }
  if (tndbeg > tndend) {
    tndbeg <- 0
    tndend <- 0
  }
  if (yrstart != 0) {
    yrstart <- max(yrstart, year(min(cavdat[, dcol])))
  }
  if (yrstart == 0) {
    yrstart <- min(year(cavdat[, dcol]))
  }
  if (yrend != 0) {
    yrend <- min(yrend, year(max(cavdat[, dcol]))) + 1
  }
  if (yrend == 0) {
    yrend <- year(max(cavdat[, dcol])) + 1
  }
  if (tndbeg == 0) {
    tndbeg <- yrstart
  }
  if (tndend == 0) {
    tndend <- yrend
  }
  
  pltind <- plotfile
  txtind <- textfile
  
  if (is.logical(pltind)) {
    # good
  } else {
    stop("plotfile argument must be a logical")
  }
  
  if (is.logical(txtind)) {
    # good
  } else {
    stop("textfile argument must be a logical")
  }
  
  dtmsg <- paste("Trend begin year is ", tndbeg, "; trend end year is ", 
                 tndend, ".", sep = "")
  message(dtmsg)
							
  npars <- length(pnames)
  nparsmes <- c("There are no parameters to analyze. User must pass
				at least one parameter name to the function using the pnames 
				argument.")
  if (npars < 1) {
    stop(nparsmes)
  }
  
  # Check model class arguments and inform user of type of model.
  mclassmes <- c("Currently, the only valid mclass options are numeric values of 1 or 2.")
  
  if (mclass != 1 & mclass != 2) {
    stop(mclassmes)
  }
  
  if (mclass == 1 ) {
    message("Performing analysis using class 1 model with a linear trend.")
  }
  
  mclassmes2 <- c("Models of class 2 must have a numeric value for the number of knots.\nSuggested value is 3 to 6.\n.")
  if (mclass == 2 ) {
    message("Performing analysis using class 2 model with restricted cubic splines.")
    if (!is.numeric(numk)) {
      stop(mclassmes2)
    }
  }
	
  cdatBoot <- cdat
  cavdatBoot <- cavdat
  # prepare concentration data
  # prepare continous ancillary data
  # myfun <- function(x) deparse(substitute(x)) 
  prepmsg <- paste("Preparing the data.")
  message(prepmsg)
  myData <- prepData(cdat, cavdat, yrstart, yrend, dcol, pnames, 
                     iwcav, qwcols) 
  cdat <- myData[[1]]
  cavdat <- myData[[2]]
  pnamesf <- vector(length = 0)
  for (iipar in (1:npars)) {
    suppressWarnings(if (exists("stpars")) {
      rm(stpars)
    })
    suppressWarnings(if (exists("aovout")) {
      rm(aovout)
    })
    suppressWarnings(if (exists("obsDat")) {
      rm(obsDat)
    })
    suppressWarnings(if (exists("predDat")) {
      rm(predDat)
    })
    suppressWarnings(if (exists("predSummary")) {
      rm(predSummary)
    })
    matches <- unique(grep(paste(iwcav, collapse = "|"), 
                           names(cdat)))
    spcols <- c(1:4, grep(pnames[iipar], names(cdat)), matches)
    cdatiipar <- cdat[, spcols]
    cdatsub <- subset(cdatiipar, !is.na(cdatiipar[, paste(qwcols[2], 
                                                          pnames[iipar], sep = "")]))
    cencol <- paste(qwcols[1], pnames[iipar], sep = "")
    centmp <- cdatsub[, cencol] == "<"
	  
    # check to see if at least 10 noncensored values
    if (sum(!centmp) > 9) {
      if (mclass == 2) {
        fitmsg <- paste("Fitting model for ", pnames[iipar], 
                        ", using restriced cubic splines.", sep = "")
        message(fitmsg)
        myRes <- fitMod(cdatsub, cavdat, yrstart, yrend, 
                        tndbeg, tndend, tanm, pnames = pnames[iipar], 
                        qwcols, mclass = 2, plotfile = pltind, textfile = txtind)
      } else {
        fitmsg <- paste("Fitting model for ", pnames[iipar], 
                        ", using a linear trend model.", sep = "")
        message(fitmsg)
        myRes <- fitMod(cdatsub, cavdat, yrstart, yrend, 
                        tndbeg, tndend, tanm, pnames = pnames[iipar], 
                        qwcols, mclass = 1, numk = 4, plotfile = pltind, 
                        textfile = txtind)
      }
      stpars <- myRes[[1]]
      aovout <- myRes[[2]]
      obsDat <- myRes[[3]][[1]]
      predDat <- myRes[[3]][[2]]
      if (mclass == 2) {
        trendLine <- myRes[[3]][[3]]
      }
      mycol <- paste("P", pnames[iipar], sep = "")
      predSummary <- data.frame(analysis = tanm, pname = pnames[iipar], 
                                predMeanConc = round(mean(predDat[, mycol]), 
                                                     digits = 5), 
                                matrix(round(quantile(predDat[, mycol], 
                                                      probs = c(0.1, 0.25, 0.5, 0.75, 0.9)), 
					     digits = 5), nrow = 1, 
				       dimnames = list(NULL, c("predQ10", "predQ25", 
							       "predQ50", "predQ75",  
							       "predQ90"))), 
				stringsAsFactors = FALSE)
      if (!exists("aovoutall")) {
        aovoutall <- myRes[[2]]
      }
      else {
        aovoutall <- c(aovoutall, myRes[[2]])
      }
      pnamesf <- c(pnamesf, pnames[iipar])
    }
    else {
      notenoughmes <- paste("Less than 10 uncensored values for P", 
                            pnames[iipar], ".\n", "Analysis not performed.", 
                            sep = "")
      message(notenoughmes)
    }
    if (exists("stpars")) {
      stpars <- round(stpars, 5)
      row.names(stpars) <- NULL
      stparsout <- matrix(stpars[1, ], nrow = 1)
      if (iipar == 1) {
        stparsoutall <- stparsout
      }
      else if (iipar > 1 & exists("stparsoutall")) {
        stparsoutall <- rbind(stparsoutall, stparsout)
      }
      else {
        stparsoutall <- stparsout
      }
    }
    if (exists("obsDat")) {
      if (iipar == 1) {
        obsdatall <- obsDat
      }
      else if (iipar > 1 & exists("obsdatall")) {
        obsdatall <- merge(obsdatall, obsDat, all = TRUE)
      }
      else {
        obsdatall <- obsDat
      }
    }
    if (exists("predDat")) {
      if (iipar == 1) {
        preddatall <- predDat
      }
      else if (iipar > 1 & exists("preddatall")) {
        preddatall <- merge(preddatall, predDat, all = TRUE)
      }
      else {
        preddatall <- predDat
      }
    }
    if (exists("predSummary")) {
      if (iipar == 1) {
        predSumAll <- predSummary
      }
      else if (iipar > 1 & exists("predSumAll")) {
        predSumAll <- rbind(predSumAll, predSummary)
      }
      else {
        predSumAll <- predSummary
      }
    }
    if (exists("trendLine") & mclass == 2) {
      if (iipar == 1) {
        trendLineAll <- trendLine
      }
      else if (iipar > 1 & exists("trendLineAll")) {
        trendLineAll <- cbind(trendLineAll, trendLine)
      }
      else {
        message("Model is misspecified.")
        trendLineAll <- trendLine
      }
    }
  }
  if (exists("stparsoutall")) {
    mod1 <- floor((stparsoutall[, 2] - 1)/4) + 1
    hlife1 <- stparsoutall[, 2] - (mod1 - 1) * 4
    nxtmp <- length(stparsoutall[1, ])
    stparsoutall <- cbind(mclass, mod1, hlife1, 
                          stparsoutall[, nxtmp], 
                          matrix(stparsoutall[, -c(1, 2, nxtmp)], 
                                 nrow = dim(stparsoutall)[1]))
    stparsoutall <- data.frame(pnamesf, stparsoutall)
    if (iwcav[1] != "none") {
      names(stparsoutall) <- c("pname", "mclass", "jmod", 
                               "hlife", "cmaxt", "scl", "loglik", 
                               paste0("c", names(myRes[[2]][[1]]$coefficients)), 
                               paste0("se", names(myRes[[2]][[1]]$coefficients)), 
                               paste0("pval", 
                                     names(myRes[[2]][[1]]$coefficients)[grep("tndlin", 
                                                                              names(myRes[[2]][[1]]$coefficients))]))
    } else if (iwcav[1] == "none") {
      names(stparsoutall) <- c("pname", "mclass", "jmod", 
                               "hlife", "cmaxt", "scl", "loglik", 
                               paste0("c", names(myRes[[2]][[1]]$coefficients)), 
                               paste0("se", names(myRes[[2]][[1]]$coefficients)), 
                               paste0("pval", 
                                     names(myRes[[2]][[1]]$coefficients)[grep("tndlin", 
                                                                              names(myRes[[2]][[1]]$coefficients))]))
    }
    obsdatall$dectime <- round(obsdatall$dectime, digits = 3)
    preddatall$dectime <- round(preddatall$dectime, digits = 3)
    
    if (mclass == 1 ) { 
      yr <- tndbeg; mo <- 7; da <- 1
      dyr <- yr + (mo - 1) / 12 + (da - 0.5) / 366
      tmid <- (tndbeg + tndend) / 2
      yrpr <- c(tndbeg, tndbeg, tndend, tndend)
      mopr <- c(1, 7, 7, 12)
      dapr <- c(1, 1, 1, 31)
      dyrpr <- yrpr + (mopr - 1) / 12 + (dapr - 0.5) / 366
      tseas <- dyr - floor(dyr)
      tyr <- dyr
      tyrpr <- dyrpr
      tseaspr <- (dyrpr - floor(dyrpr))
      tmid <- (tndbeg + tndend) / 2
      tndlin <- tyr - tmid
      tndlin[tyr < tndbeg] <- tndbeg - tmid
      tndlin[tyr > tndend + 1] <- tndend - tmid
      tndlinpr <- tyrpr - tmid
      tndlinpr[tyrpr < tndbeg] <- tndbeg - tmid
      tndlinpr[tyrpr > tndend + 1] <- tndend - tmid
      trends <- data.frame(stparsoutall[, 1:2])
      mycolNams <- c("alpha", "baseConc", "ctndPpor", "cuciPpor", "clciPpor",
                   "ctndOrigPORPercentBase", "cuciOrigPORPercentBase", 
                   "clciOrigPORPercentBase", "ctndlklhd")
      trends[mycolNams] <- NA
      for (i in 1:dim(stparsoutall)[[1]]) {
        trends[i, 3:11] <- pesticideTrendCalcs(tndbeg, tndend, 
                                               stparsoutall$cxmattndlin[i], 
                                               stparsoutall$pvalxmattndlin[i], 
                                               alpha, stparsoutall$sexmattndlin[i], 
                                               stparsoutall$scl[i],
                                               exp((stparsoutall$cxmatintcpt[i] + 
                                                      stparsoutall$cxmattndlin[i] * 
                                                      tndlinpr[1]) * log(10)),
                                               mclass)
      }
      fitRes <- list(stparsoutall, aovoutall, obsdatall, preddatall, 
                     predSumAll, trends)
      fitRes
    } else if (mclass == 2) {
      # modified functions to do bootstrapping with less output and messages
      fitswavecavBoot <- function(cdat, cavdat, pnames, yrstart = 0,
                                  yrend = 0, tndbeg = 0, tndend = 0, iwcav = c("none"), 
                                  dcol = "dates", qwcols = c("R", "P"), mclass = 2, 
                                  numk = 4, nboot = 10000) {
        
        mclassmes <- c("Bootstrap option is for models of class 2 only.")
        if (mclass != 2) {
          stop(mclassmes)
        }
        pestMessage <- c("Bootstrap function is for one pesticide-site combination at a time.")
        if (length(pnames) > 1 ) {
          stop(pestMessage)
        }
        
        myData <- prepData(cdat, cavdat, yrstart, yrend, dcol, pnames, iwcav, qwcols) 
        cdat <- myData[[1]]
        cavdat <- myData[[2]]
        pnamesf <- vector(length = 0)
        matches <- unique(grep(paste(iwcav, collapse = "|"), names(cdat)))
        spcols <- c(1:4, grep(pnames, names(cdat)), matches)
        cdatiipar <- cdat[, spcols]
        cdatsub <- subset(cdatiipar, 
                          !is.na(cdatiipar[, paste(qwcols[2], pnames, sep = "")]))
        
        # create new dataset
        myyrs <- unique(cdatsub$yrc)
        newyrs <- sample(myyrs, size = length(myyrs), replace = TRUE)
        replacements <- cbind(myyrs, newyrs)
        for (i in 1:length(myyrs) ) {
          pck <- cdatsub$yrc == myyrs[i]
          if (i == 1) {
            newdat <- cdatsub[pck,]
            newdat$yrc <- newyrs[i]
          } else if (i > 1) {
            newdat2 <- cdatsub[pck,]
            newdat2$yrc <- newyrs[i]
            newdat <- rbind.data.frame(newdat, newdat2)
          }
        }
        o <- order(newdat$yrc, newdat$jdayc)
        cdatsub <- newdat
        
        cencol <- paste(qwcols[1], pnames, sep = "")
        centmp <- cdatsub[, cencol] == "<"
        
        myRes <- fitModBoot(cdatsub, cavdat, yrstart, yrend, tndbeg, tndend,  
                            pnames = pnames, qwcols, mclass = 2, numk = numk)
        stpars <- myRes[[1]]
        trendLine <- myRes[[3]][[1]]
        pnamesf <- c(pnamesf, pnames)
        if (exists("stpars")) {
          stpars <- round(stpars, 5)
          row.names(stpars) <- NULL
          stparsout <- matrix(stpars[1, ], nrow = 1)
          stparsoutall <- stparsout
        }
        if (exists("trendLine") & mclass == 2) {
          trendLineAll <- trendLine
        } 
        mod1 <- floor((stparsoutall[, 2] - 1)/4) + 1
        hlife1 <- stparsoutall[, 2] - (mod1 - 1) * 4
        nxtmp <- length(stparsoutall[1, ])
        stparsoutall <- cbind(mclass, mod1, hlife1, stparsoutall[, nxtmp], 
                              matrix(stparsoutall[, -c(1, 2, nxtmp)], 
                                     nrow = dim(stparsoutall)[1]))
        stparsoutall <- data.frame(pnamesf, stparsoutall)
        if (iwcav[1] != "none") {
          names(stparsoutall) <- c("pname", "mclass", "jmod", "hlife", "cmaxt", "scl", 
                                   "loglik", 
                                   paste0("c", names(myRes[[2]][[1]]$coefficients)), 
                                   paste0("se", names(myRes[[2]][[1]]$coefficients)), 
                                   paste0("pval", 
                                          names(myRes[[2]][[1]]$coefficients)[grep("tndlin", 
                                                                                   names(myRes[[2]][[1]]$coefficients))]))
        } else if (iwcav[1] == "none") {
          names(stparsoutall) <- c("pname", "mclass", "jmod", "hlife", "cmaxt", "scl", 
                                   "loglik", 
                                   paste0("c", names(myRes[[2]][[1]]$coefficients)), 
                                   paste0("se", names(myRes[[2]][[1]]$coefficients)), 
                                   paste0("pval", 
                                          names(myRes[[2]][[1]]$coefficients)[grep("tndlin", 
                                                                                   names(myRes[[2]][[1]]$coefficients))]))
        }
        
        trends <- data.frame(stparsoutall[, 1:2])
        mycolNams <- c("baseConc", "endConc", "rcsctndPpor", "rcsctndOrigPORPercentBase")
        trends[mycolNams] <- NA
        for (i in 1:dim(stparsoutall)[[1]]) {
          colHead <- paste0("P", stparsoutall[i, 1], "TL")
          baseConc <- trendLineAll[1, colHead]
          endConc <- trendLineAll[dim(trendLineAll)[[1]], colHead]
          rcsctndPpor <- 100 * (trendLineAll[dim(trendLineAll)[[1]], colHead] / trendLineAll[1, colHead] - 1)
          rcsctndOrigPORPercentBase <- baseConc * trendLineAll[dim(trendLineAll)[[1]], colHead] / trendLineAll[1, colHead] - baseConc
          trends[i, 3:6] <- c(round(baseConc, digits = 4), round(endConc, digits = 4), 
                              round(rcsctndPpor, digits = 4), 
                              round(rcsctndOrigPORPercentBase, digits = 4))
        }
        fitRes <- trends
        fitRes
      }
      
      fitModBoot <- function(cdatsub, cavdat, yrstart, yrend, tndbeg, tndend, 
                             pnames, qwcols, mclass = 1, numk = 4) {
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
        
        # set up matrix with continuous variables
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
        
        tndrcs <- rcs(tndlin, numk)
        
        tndlinpr <- tyrpr - tmid
        tndlinpr[tyrpr < tndbeg] <- tndbeg - tmid
        tndlinpr[tyrpr > tndend + 1] <- tndend - tmid
        tndrcspr <- rcs(tndlinpr, attributes(tndrcs)$parms)
        
        # find cmaxt (decimal season of max concentration)
        tmpsm <- supsmu(tseas, clog)
        xsm <- tmpsm$x
        ysm <- tmpsm$y
        nsm <- length(ysm)
        cmaxt <- xsm[order(ysm)[nsm]]
        
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
                                    type = "left") ~ xmat - 1, dist = "gaussian")
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
        
        jmod <- floor((stpars[1, 2] - 1) / 4) + 1
        hlife <- stpars[1, 2] - (jmod - 1) * 4
        
        plotDat <- seawaveQNoPlots(stpars, cmaxt, tseas, tseaspr, tndrcs, tndrcspr, 
                                   cdatsub, cavdat, cavmat, clog, centmp, yrstart, 
                                   yrend, tyr, tyrpr, pnames, mclass = 2, 
                                   numk = numk)
        myRes <- list(stpars, aovout, plotDat)
        myRes
      }
      
      seawaveQNoPlots <- function(stpars, cmaxt, tseas, tseaspr, tndrcs, tndrcspr, 
                                  cdatsub, cavdat, cavmat, clog, centmp, yrstart, 
                                  yrend, tyr, tyrpr, pnames, mclass = 2, numk) {
        pckone <- stpars[1, 2]
        mod1 <- floor((pckone - 1)/4) + 1
        hlife1 <- pckone - (mod1 - 1) * 4
        ipkt <- floor(360 * tseas)
        ipkt[ipkt == 0] <- 1
        
        # call function to compute seasonal wave
        wavexx <- compwaveconv(cmaxt, mod1, hlife1)
        wavest <- wavexx[ipkt]
        intcpt <- rep(1, length(wavest))
        ipktpr <- floor(360 * tseaspr)
        ipktpr[ipktpr == 0] <- 1
        wavestpr <- wavexx[ipktpr]
        intcptpr <- rep(1, length(wavestpr))
        
        xmat <- cbind(intcpt, wavest, tndrcs)
        if (length(cdatsub[1, ]) > 6) {
          xmat <- cbind(xmat, cavmat)
          cavmatpr <- as.matrix(cavdat[, 5:length(cavdat[1, ])])
        }
        xmatpr <- cbind(intcptpr, wavestpr, tndrcspr)
        if (length(cdatsub[1, ]) > 6) {
          xmatpr <- cbind(xmatpr, cavmatpr)
        }
        
        partmp <- stpars[1, 5:(5 + length(xmat[1, ]) - 1)]
        
        # trend line
        rcscols <- grep("tndlinpr", dimnames(xmatpr)[[2]])
        partmprcscols <- partmp[rcscols]
        fitadjx12 <- as.matrix(rowSums(mapply("*", as.data.frame(xmatpr[, c(1, rcscols)]), 
                                              partmp[c(1, rcscols)])))
        
        trendLine <- data.frame(trendLine = 10 ^ (fitadjx12))
        dimnames(trendLine)[[2]][1] <- paste0("P", pnames, "TL")
        noPlotDat <- list(trendLine)
        noPlotDat
      }
      trends <- data.frame(stparsoutall[, 1:2])
      mycolNams <- c("baseConc", "endConc", "rcsctndPpor", 
                     "rcsctndOrigPORPercentBase", "pvalrcstnd", "ctndlklhd")
      trends[mycolNams] <- NA
      for (i in 1:dim(stparsoutall)[[1]]) {
        colHead <- paste0("P", stparsoutall[i, 1], "TL")
        baseConc <- trendLineAll[1, colHead]
        endConc <- trendLineAll[dim(trendLineAll)[[1]], colHead]
        rcsctndPpor <- 100 * (trendLineAll[dim(trendLineAll)[[1]], colHead] / trendLineAll[1, colHead] - 1)
        rcsctndOrigPORPercentBase <- baseConc * trendLineAll[dim(trendLineAll)[[1]], colHead] / trendLineAll[1, colHead] - baseConc
        trends[i, 3:6] <- c(round(baseConc, digits = 4), round(endConc, digits = 4), 
                            round(rcsctndPpor, digits = 4), 
                            round(rcsctndOrigPORPercentBase, digits = 4))
        
        answer <- trends[i, 5]
        
        if (bootRCS == TRUE) {
          bootTrends <- data.frame(rcsctndPpor = numeric(0), 
                                   rcsctndOrigPORPercentBase = numeric(0), 
                                   sample = numeric(0))
          for (j in 1:nboot) {
            test <- fitswavecavBoot(cdatBoot, cavdatBoot, 
                                    pnames = pnames[i], yrstart = yrstart, 
                                    yrend = yrend, tndbeg = tndbeg, tndend = tndend, 
                                    iwcav = iwcav, dcol = dcol, 
                                    qwcols = qwcols, mclass = 2, numk = 4,
                                    nboot = nboot)
            bootTrends[j, 1:3] <- c(test$rcsctndPpor, test$rcsctndOrigPORPercentBase, j)
          }
          cntMoreExtrTnds <- dim(subset(bootTrends, abs(rcsctndPpor) >= abs(answer)))[[1]]
          pvalrcstnd <- cntMoreExtrTnds/nboot
          ctndlklhd <- 1 - pvalrcstnd / 2
          trends[i, 7:8] <- c(pvalrcstnd, ctndlklhd)
        }
      }
      fitRes <- list(stparsoutall, aovoutall, obsdatall, preddatall, 
                     predSumAll, trends)

      fitRes
    } else {
      message("Problem consolidating results.")
    }
  } else {
    message("No constituent had 10 or more uncensored values.")
  }
}
