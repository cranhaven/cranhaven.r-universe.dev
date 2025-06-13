#' Influenza- and temperature-attributable mortality for a FluMoDL object
#'
#' This function uses an object of class \code{FluMoDL} to calculate mortality
#' attributed to influenza and/or temperature.
#'
#' @param m An object of class \code{FluMoDL}.
#'
#' @param par A character vector indicating which exposures to calculate the
#' attributable mortality for. Defaults to \code{c("H1","H3","B","temp","RSV")}, which
#' indicates all three influenza proxies, temperature and RSV (if it exists in the model).
#'
#' @param sel For which time period(s) to calculate attributable mortality. This can be
#' one of several choices. For \code{sel="week"} (the default) and \code{sel="season"} attributable
#' mortality is calculated for each week or each season respectively. One can also
#' provide to \code{sel} a list of index vectors (integer or logical) corresponding to
#' particular rows of \code{m$data}, or a matrix of logical index vectors, or a single
#' index vector. Note that the index vectors should point to \emph{consecutive rows}
#' of \code{m$data}.
#'
#' @param from Week (integer, in YYYYWW format) or season to start from, in case
#' \code{sel="week"} or \code{sel="season"} respectively.
#'
#' @param to Week (integer, in YYYYWW format) or season to end with, in case
#' \code{sel="week"} or \code{sel="season"} respectively.
#'
#' @param temprange In case temperature-attributable mortality is calculated (argument
#' \code{par} includes "temp"), this argument specifies the temperature range or interest.
#' This can be one of several choices.
#'
#' If \code{temprange="cold"} (the default) mortality
#' attributable to cold temperatures is calculated, i.e. temperatures below the MMP
#' (minimum mortality point). If \code{temprange="heat"} mortality attributable to hot
#' temperatures is calculated, i.e. those above the MMP. If \code{temperature="all"} the
#' entire range of temperatures is used, i.e. any temperature other than the MMP.
#'
#' Alternatively one can provide a numeric vector of length two, indicating a specific
#' temperature range; this can also be provided as a \emph{character} vector of length two,
#' where one of the elements can be the word "MMP", which will be replaced with the MMP
#' temperature.
#'
#' @param ci If \code{TRUE}, empirical (Monte Carlo) 95%% Confidence Intervals are returned
#' for all attributable mortality estimates.
#'
#' @param nsim Number of Monte Carlo simulations to run per attributable mortality estimate.
#' Defaults to 5000. Increase if higher precision is required (and you don't mind the wait).
#'
#' @param mcsamples If \code{TRUE}, return all Monte Carlo simulation samples in the output.
#' See below.
#'
#' @param progress If \code{TRUE}, a progress bar appears if Monte Carlo simulations are
#' run and if there are more than three time periods selected in argument \code{sel}. Set to
#' \code{FALSE} to suppress the progress bar.
#'
#' @param blup If \code{FALSE} (the default), the model coefficients stored in \code{m$model}
#'   are used for the calculation of attributable mortality. If \code{TRUE}, the coefficients
#'   \link[=blup.FluMoDL]{stored in the FluMoDL object} are used; if \code{blup=TRUE} but
#'   \code{blup(m)} is \code{NULL}, a warning is generated. Alternatively, \code{blup} can
#'   be another object of class \code{\link{summary.FluMoDL}}, whose coefficients are used for the
#'   calculation.
#'
#' @details All attributable mortalities are calculated using the "backward" perspective, meaning
#'   the mortality at any given day that is attributable to exposures up to 30 days previously
#'   (=the maximum lag).
#'
#'   Confidence intervals (when \code{ci=TRUE}) are obtained empirically through Monte Carlo
#'   simulations; this can take quite some time if lots of CIs need to be calculated (for example
#'   if \code{sel=TRUE}). For this reason, a progress bar is shown by default in this case
#'   (which can be suppressed by \code{progress=FALSE}).
#'
#'   Temperature-attributable mortalities are by default calculated for cold temperatures, i.e.
#'   temperatures lower than the minimum mortality point (MMP). Note, however, that the adjustment
#'   in the FluMoDL is made for the entire range of daily mean temperatures, not just for cold.
#'   Therefore mortality attributable to any range of temperatures can be calculated, e.g. for
#'   heat, extreme cold, extreme heat, etc. See argument \code{temprange} above for details.
#'
#'
#' @return If \code{mcsamples=FALSE} (the default), a data.frame is returned with columns named
#'   'FluH1', 'FluH3', 'FluB' and 'Temp' (and/or 'RSV'), depending on the argument \code{par},
#'   and also 'FluH1.lo', 'FluH1.hi', 'FluH3.lo', ..., if \code{ci=TRUE}. Each row in the output
#'   corresponds to a selection
#'   made in argument \code{sel}, for example if \code{sel="week"} (the default) rows correspond to
#'   each week available in the data. If all influenza types/subtypes are selected in \code{par}, a
#'   column named 'AllFlu' is also calculated automatically, with the mortality (and 95%% CI)
#'   attributable to all influenza types/subtypes.
#'
#'   If \code{mcsamples=TRUE}, a list is returned with elements 'result' and 'mcsamples'. The
#'   first contains the data.frame with point estimates of influenza- and/or temperature-attributable
#'   mortality, as before (no 95%% CI is provided, even if \code{ci=TRUE}). The 'mcsamples'
#'   element contains a list of the Monte Carlo simulation samples for each parameter in \code{par}.
#'
#'
#' @references \itemize{
#'  \item Lytras T, Pantavou K, Mouratidou E, Tsiodras S. Mortality attributable to seasonal influenza
#'   in Greece, 2013 to 2017: variation by type/subtype and age, and a possible harvesting effect.
#'   \href{https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2019.24.14.1800118}{Euro Surveill.}
#'   2019;24(14):pii=1800118 (\href{https://www.ncbi.nlm.nih.gov/pubmed/30968823}{PubMed})
#'
#' \item Gasparrini A, Leone M. Attributable risk from distributed lag models.
#' \href{https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-55}{BMC Med Res Methodol} 2014;14:55.
#' }
#'
#' @importFrom utils read.csv setTxtProgressBar txtProgressBar
#'
#' @examples
#' data(greece) # Use example surveillance data from Greece
#' m <- with(greece, fitFluMoDL(deaths = daily$deaths,
#'     temp = daily$temp, dates = daily$date,
#'     proxyH1 = weekly$ILI * weekly$ppH1,
#'     proxyH3 = weekly$ILI * weekly$ppH3,
#'     proxyB = weekly$ILI * weekly$ppB,
#'     yearweek = weekly$yearweek))
#'
#' \donttest{
#' # Calculate influenza-attributable estimates by season, until 2016-17:
#' attr1 <- attrMort(m, par=c("H1","H3","B"), sel="season", to=2016)
#' attr1
#'
#' # Calculate influenza-attributable estimates by week, only point
#' #    estimates, for the 2014-15 season:
#' attr2 <- attrMort(m, par=c("H1","H3","B"), sel="week",
#'     from=201440, to=201520, ci=FALSE)
#' attr2
#' }
#'
#' # Calculate mortality attributable to temperatures below 5 celsius, for
#' #    the period of January 2017:
#' attr3 <- attrMort(m, par="temp",
#'     sel=with(m$data, which(dates>="2017-1-1" & dates<="2017-1-31")),
#'     temprange=c(5,-20))
#'
#' \donttest{
#' # Calculate attributable mortalities for the entire 2017-18 season, and
#' #    return the Monte Carlo simulation samples in the output
#' attr4 <- attrMort(m, sel="season", from=2017, to=2017, mcsamples=TRUE)
#' }
#'
#' @export
attrMort <- function(m, par=c("H1","H3","B","temp","RSV"), sel="week", from=NULL, to=NULL,
        temprange="cold", ci=TRUE, nsim=5000, mcsamples=FALSE, progress=TRUE, blup=FALSE) {
  # Check arguments
  if (!inherits(m, "FluMoDL")) stop("Argument `m` should be of class 'FluMoDL'.")
  if (is.logical(blup) && length(blup)==1) {
    if (blup) {
      blup <- blup(m)
      if (is.null(blup)) warning("Argument 'blup' was TRUE but object 'm' does not contain BLUP estimates.")
    } else {
      blup <- NULL
    }
  } else if (inherits(blup, "summary.FluMoDL")) {
    # Do nothing. All OK.
  } else {
    stop("Argument 'blup' should be TRUE, FALSE or an object of class 'summary.FluMoDL.")
  }
  if ((is.null(m$pred$proxyRSV) || is.null(m$data$proxyRSV)) && sum(par=="RSV")>0) {
    par <- par[par!="RSV"]
  }
  par <- par[par %in% c("H1", "H3", "B", "temp", "RSV")]
  if (length(par)==0) stop("Argument `par` should be at least one of 'H1', 'H3', 'B', 'temp', 'RSV'.")

  # Determine selections for which to calculate attributable mortality
  if (length(sel)==1 && sel=="week") {
    weeks_in_dataset <- sort(unique(m$data$yearweek))
    if (!is.null(from)) weeks_in_dataset <- weeks_in_dataset[weeks_in_dataset>=from]
    if (!is.null(to)) weeks_in_dataset <- weeks_in_dataset[weeks_in_dataset<=to]
    if (length(weeks_in_dataset)==0) stop("No weeks left in dataset after filtering with arguments `from` and `to`.")
    selIndices <- lapply(weeks_in_dataset, function(x) which(m$data$yearweek==x))
    selNames <- weeks_in_dataset

  } else if (length(sel)==1 && sel=="season") {
    seasons <- with(m$data,
                    (yearweek%/%100 - as.integer(yearweek%%100<=20))*(yearweek%%100<=20 | yearweek%%100>=40))
    seasons[seasons==0] <- NA
    seasons_in_dataset <- sort(unique(seasons)[!is.na(unique(seasons))])
    if (!is.null(from)) seasons_in_dataset <- seasons_in_dataset[seasons_in_dataset>=from]
    if (!is.null(to)) seasons_in_dataset <- seasons_in_dataset[seasons_in_dataset<=to]
    if (length(seasons_in_dataset)==0) stop("No seasons left in dataset after filtering with arguments `from` and `to`.")
    selIndices <- lapply(seasons_in_dataset, function(x) which(seasons==x))
    selNames <- seasons_in_dataset

  } else if (is.matrix(sel)) {
    if (!is.logical(sel) || nrow(sel)!=nrow(m$data)) stop("If argument `sel` is a matrix, it should be a logical matrix with number of rows equal to the number of rows in `m$data`.")
    selIndices <- names(as.list(as.data.frame(sel)))
    selNames <- colnames(sel)

  } else if (class(sel)=="list") {
    selIndices <- sel
    selNames <- names(sel)

  } else if (is.numeric(sel) || is.logical(sel)) {
    selIndices <- list(sel)
    selNames <- NULL

  } else {
    stop("Inappropriate value in argument `sel`. It should be one of 'week' or 'season', or a logical matrix, or an index vector or list of index vectors.")
  }

  # selIndices should now contain a list of index vectors. Let's see now whether these are appropriate
  for (i in 1:length(selIndices)) {
    if (is.logical(selIndices[[i]])) {
      selIndices[[i]] <- which(selIndices[[i]])
    }
    if (is.numeric(selIndices[[i]])) {
      selIndices[[i]] <- sort(unique(as.integer(selIndices[[i]])))
      selIndices[[i]] <- selIndices[[i]][selIndices[[i]]<=nrow(m$data)]
      if (length(selIndices[[i]])==0) stop(sprintf("In element %s of `sel`, no valid indices found.", i))
      if (sum(diff(selIndices[[i]])!=1)>0) stop(sprintf("In element %s of `sel`, a non-contiguous range of indices was given.", i))
    } else {
      stop(sprintf("In element %s of `sel`, found neither a logical nor an integer index vector.", i))
    }
  }

  # Now everything should be correct, whoa!!
  # Time to loop through the selections and calculate the attributable number of cases.
  if (!ci || length(selIndices)<=3) progress <- FALSE
  if (progress) {
    i=0
    pb <- txtProgressBar(title = "FluMoDL",
                         label = sprintf("Calculating for %s selections...", length(selIndices)),
                         min=0, max=length(selIndices), initial=0, style=3)
  }
  res <- lapply(selIndices, function(s) {
    if (progress) { i <<- i+1; setTxtProgressBar(pb, i) }
    p <- list()
    mc <- list()
    if ("temp" %in% par) {
      # First determine the required temperature range
      if (length(temprange)==1 && temprange=="cold") {
        tRange <- c(floor(min(m$data$temp)), m$MMP)
      } else if (length(temprange)==1 && temprange=="heat") {
        tRange <- c(m$MMP, ceiling(max(m$data$temp)))
      } else if (length(temprange)==1 && temprange=="all") {
        tRange <- c(floor(min(m$data$temp)), ceiling(max(m$data$temp)))
      } else if (length(temprange)==2 && is.vector(temprange) && !is.list(temprange)) {
        if (!is.na(match("MMP", temprange))) temprange[temprange=="MMP"] <- m$MMP
        tRange <- as.numeric(temprange)
        if (sum(is.na(tRange))>0) stop("Invalid value provided for argument `temprange`.")
      } else {
        tRange <- c(floor(min(m$data$temp)), ceiling(max(m$data$temp)))
      }
      basis.temp <- m$basis$temp
      p$Temp <- attrdl(m$data$temp, basis.temp, m$data$deaths, m$model, cen=m$MMP, type="an",
                       sub=s, range=tRange)
      if (ci || mcsamples) mc$Temp <- attrdl(m$data$temp, basis.temp, m$data$deaths, m$model,
                                             cen=m$MMP, type="an", sub=s, tot=FALSE, sim=TRUE, nsim=nsim, range=tRange)
    }
    if ("RSV" %in% par) {
      basis.proxyRSV <- m$basis$proxyRSV
      if (!is.null(blup) && !is.null(blup$coef$proxyRSV)) {
        p$RSV <- attrdl(m$data$proxyRSV, basis.proxyRSV, m$data$deaths,
                        coef=blup$coef$proxyRSV, vcov=blup$vcov$proxyRSV,
                        cen=0, type="an", sub=s)
        if (ci || mcsamples)
          mc$RSV <- attrdl(m$data$proxyRSV, basis.proxyRSV, m$data$deaths,
                           coef=blup$coef$proxyRSV, vcov=blup$vcov$proxyRSV,
                           cen=0, type="an", sub=s, tot=FALSE, sim=TRUE, nsim=nsim)
      } else {
        p$RSV <- attrdl(m$data$proxyRSV, basis.proxyRSV, m$data$deaths, m$model,
                        cen=0, type="an", sub=s)
        if (ci || mcsamples)
          mc$RSV <- attrdl(m$data$proxyRSV, basis.proxyRSV, m$data$deaths, m$model,
                           cen=0, type="an", sub=s, tot=FALSE, sim=TRUE, nsim=nsim)
      }
    }
    if ("B" %in% par) {
      basis.proxyB <- m$basis$proxyB
      if (!is.null(blup) && !is.null(blup$coef$proxyB)) {
        p$FluB <- attrdl(m$data$proxyB, basis.proxyB, m$data$deaths,
                         coef=blup$coef$proxyB, vcov=blup$vcov$proxyB,
                         cen=0, type="an", sub=s)
        if (ci || mcsamples)
          mc$FluB <- attrdl(m$data$proxyB, basis.proxyB, m$data$deaths,
                            coef=blup$coef$proxyB, vcov=blup$vcov$proxyB,
                            cen=0, type="an", sub=s, tot=FALSE, sim=TRUE, nsim=nsim)
      } else {
        p$FluB <- attrdl(m$data$proxyB, basis.proxyB, m$data$deaths, m$model,
                         cen=0, type="an", sub=s)
        if (ci || mcsamples)
          mc$FluB <- attrdl(m$data$proxyB, basis.proxyB, m$data$deaths, m$model,
                            cen=0, type="an", sub=s, tot=FALSE, sim=TRUE, nsim=nsim)
      }
    }
    if ("H3" %in% par) {
      basis.proxyH3 <- m$basis$proxyH3
      if (!is.null(blup) && !is.null(blup$coef$proxyH3)) {
        p$FluH3 <- attrdl(m$data$proxyH3, basis.proxyH3, m$data$deaths,
                          coef=blup$coef$proxyH3, vcov=blup$vcov$proxyH3,
                          cen=0, type="an", sub=s)
        if (ci || mcsamples)
          mc$FluH3 <- attrdl(m$data$proxyH3, basis.proxyH3, m$data$deaths,
                             coef=blup$coef$proxyH3, vcov=blup$vcov$proxyH3,
                             cen=0, type="an", sub=s, tot=FALSE, sim=TRUE, nsim=nsim)
      } else {
        p$FluH3 <- attrdl(m$data$proxyH3, basis.proxyH3, m$data$deaths, m$model,
                          cen=0, type="an", sub=s)
        if (ci || mcsamples)
          mc$FluH3 <- attrdl(m$data$proxyH3, basis.proxyH3, m$data$deaths, m$model,
                             cen=0, type="an", sub=s, tot=FALSE, sim=TRUE, nsim=nsim)
      }
    }
    if ("H1" %in% par) {
      basis.proxyH1 <- m$basis$proxyH1
      if (!is.null(blup) && !is.null(blup$coef$proxyH1)) {
        p$FluH1 <- attrdl(m$data$proxyH1, basis.proxyH1, m$data$deaths,
                          coef=blup$coef$proxyH1, vcov=blup$vcov$proxyH1,
                          cen=0, type="an", sub=s)
        if (ci || mcsamples)
          mc$FluH1 <- attrdl(m$data$proxyH1, basis.proxyH1, m$data$deaths,
                             coef=blup$coef$proxyH1, vcov=blup$vcov$proxyH1,
                             cen=0, type="an", sub=s, tot=FALSE, sim=TRUE, nsim=nsim)
      } else {
        p$FluH1 <- attrdl(m$data$proxyH1, basis.proxyH1, m$data$deaths, m$model,
                          cen=0, type="an", sub=s)
        if (ci || mcsamples)
          mc$FluH1 <- attrdl(m$data$proxyH1, basis.proxyH1, m$data$deaths, m$model,
                             cen=0, type="an", sub=s, tot=FALSE, sim=TRUE, nsim=nsim)
      }
    }
    if (sum(is.na(match(c("FluH1","FluH3","FluB"), names(p))))==0) {
      p$AllFlu <- with(p, FluH1+FluH3+FluB)
      if (ci || mcsamples) mc$AllFlu <- with(mc, FluH1+FluH3+FluB)
    }
    if (mcsamples) {
      res <- list(
        result = round(unlist(p[rev(names(p))])),
        mcsamples = mc[rev(names(mc))]
      )
    } else if (ci) {
      res <- round(c(sapply(rev(names(p)), function(n) {
        c(p[[n]], quantile(mc[[n]], c(0.025, 0.975), na.rm=TRUE))
      })))
      names(res) <- paste0(rep(rev(names(p)),each=3), c("",".lo",".hi"))
    } else {
      res <- round(unlist(p[rev(names(p))]))
    }
    res
  })
  if (exists("pb")) close(pb)
  if (mcsamples) {
    res <- list(
      result = as.data.frame.matrix(do.call("rbind", lapply(res, function(x) x$result))),
      mcsamples = lapply(res, function(x) x$mcsamples)
    )
    rownames(res$result) <- selNames
    names(res$mcsamples) <- selNames
  } else {
    res <- as.data.frame.matrix(do.call("rbind", res))
    rownames(res) <- selNames
  }
  res
}

