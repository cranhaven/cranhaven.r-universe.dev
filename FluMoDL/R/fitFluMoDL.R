#' Fit a FluMoDL object
#'
#' This function fits a FluMoDL object. This is a distributed lag nonlinear model (DLNM), of
#' quasipoisson family and with log link, which estimates the association between mortality
#' (as outcome) and daily mean temperatures and type-specific influenza incidence proxies
#' (as exposures), adjusted for covariates.
#'
#' Objects of class 'FluMoDL' contain the model, the associated data, estimates of the predicted
#' associations and other information.
#' These objects can be further used as input for function \code{\link{attrMort}}, to calculate
#' influenza-attributable and temperature-attributable mortalities for any period in the data
#' (and any temperature range). Methods \code{print()}, \code{coef()} and \code{vcov()} have
#' been defined for objects of class 'FluMoDL' (see below),
#' and also \code{\link[=summary.FluMoDL]{summary()}}.
#'
#' @param deaths A vector of \emph{daily} deaths, of equal length to argument \code{`dates`}
#' @param temp A vector of \emph{daily} mean temperatures, of equal length to argument \code{`dates`}
#' @param dates A vector of dates (of class \code{Date})
#' @param proxyH1 A vector of \emph{weekly} influenza A(H1N1)pdm09 incidence proxies, of equal
#'   length to argument \code{`yearweek`}
#' @param proxyH3 A vector of \emph{weekly} influenza A(H3N2) incidence proxies, of equal
#'   length to argument \code{`yearweek`}
#' @param proxyB A vector of \emph{weekly} influenza B incidence proxies, of equal
#'   length to argument \code{`yearweek`}
#' @param yearweek An integer vector of weeks, in \emph{yyyyww} format
#' @param proxyRSV An \emph{optional} vector of \emph{weekly} RSV incidence proxies, of equal
#'   length to argument \code{`yearweek`}. (This is an experimental feature, and this argument
#'   might be removed in the future.)
#' @param smooth \code{TRUE} (the default) if smoothing is to be applied to the influenza
#'   incidence proxies when converting them to a daily series.
#' @param periodic Should a periodic B-spline term be included in the model? 
#'   Defaults to \code{TRUE}.
#'
#' @details FluMoDL uses a DLNM with the \emph{daily} number of deaths as the outcome. Covariates
#'   include the following:
#'   \itemize{
#'     \item A \code{\link[dlnm:crossbasis]{cross-basis matrix}} for temperature. The exposure-response
#'     relationship is modelled with a quadratic B-spline with internal knots placed at the
#'     10th, 75th and 90th percentile of the temperatures distribution. The lag-response
#'     relationship is modelled with a natural cubic spline with three internal knots
#'     equidistant in the log scale.
#'
#'     \item Three \code{\link[dlnm:crossbasis]{cross-basis matrices}} for influenza incidence proxies for
#'     each type/subtype: A(H1N1)pdm09, A(H3N2) and B. These normally are equal to a
#'     sentinel Influenza-Like Illness (ILI) rate, times the laboratory swab samples Percentage
#'     Positive (%%) for each type. The exposure-response relationship is specified as linear,
#'     implying an approximately constant case fatality ratio for each influenza type. The
#'     lag-response relationship is specified as above (for temperature).
#'
#'     \item A periodic B-spline term to model seasonality, with three equidistant internal
#'     knots according to day of the year. Can optionally be suppressed by setting argument
#'     \code{periodic} to \code{FALSE}.
#'
#'     \item A linear trend, and a factor variable for day of the week.
#'
#'     \item \emph{Optionally}, a \code{\link[dlnm:crossbasis]{cross-basis matrix}} for an RSV
#'     incidence proxy, with specification identical to those for influenza. If given,
#'     it will be included in the model and output, and it will be possible to calculate
#'     mortality attributable to RSV with \code{\link{attrMort}}. This is an experimental feature;
#'     it might be removed in the future.
#'   }
#'
#' @return An object of class 'FluMoDL'. This is a list containing the following elements:
#'   \describe{
#'     \item{$data}{A \code{data.frame} with the data used to fit the model. Rows correspond to
#'     days in argument \code{dates}. The columns are named: \code{yearweek}, \code{dates},
#'     \code{deaths}, \code{temp}, (for temperature), \code{proxyH1}, \code{proxyH3}, \code{proxyB},
#'     \code{t} (linear trend, with values \code{1:nrow(m$data)}), \code{doy} (day of year,
#'     use to calculate the periodic B-spline term to model seasonality) and \code{dow} (day of
#'     the week). Also column \code{proxyRSV} if the relevant argument is provided.}
#'
#'     \item{$model}{The fitted model; an object of class \code{glm} and of 'quasipoisson' family
#'     with log link.}
#'
#'     \item{$basis}{A list with names 'temp', 'proxyH1', 'proxyH3' and 'proxyB' (and proxyRSV,
#'     if provided in the function arguments), containing the
#'     cross-basis matrices that are used as exposures in the model. See \code{\link[dlnm]{crossbasis}}.}
#'
#'     \item{$MMP}{The Minimum Mortality Point, i.e. the temperature where mortality is lowest.}
#'
#'     \item{$pred}{A list with names 'temp', 'proxyH1', 'proxyH3' and 'proxyB' (and 'proxyRSV'
#'     if provided in the function arguments), containing
#'     predictions (in the form of \code{\link[dlnm]{crosspred}} objects) for each exposure.
#'     These can be plotted in both the exposure-response and lag-response dimensions, see
#'     \code{\link[dlnm]{crosspred}}, \code{\link[dlnm]{plot.crosspred}} and the examples below.}
#'
#'     \item{$blup}{This element is NULL when creating the object, but can receive a
#'     \code{\link{summary.FluMoDL}} object that contains Best Linear Unbiased Predictor
#'     (BLUP) coefficients, to be used when estimating attributable mortality. Can be
#'     retrieved or set with the \code{\link{blup.FluMoDL}} method}
#'   }
#'
#' Objects of class 'FluMoDL' have methods \code{print()}, \code{coef()} and \code{vcov()}.
#' \code{coef()} returns a list of numeric vectors, with names 'proxyH1', 'proxyH3'
#' and 'proxyB' (and 'proxyRSV' if provided in the function arguments), containing the model
#' coefficients for these cross-basis terms. Similarly \code{vcov()} returns a list
#' of variance-covariance matrices for the same terms.
#'
#' @references \itemize{
#'  \item Lytras T, Pantavou K, Mouratidou E, Tsiodras S. Mortality attributable to seasonal influenza
#'   in Greece, 2013 to 2017: variation by type/subtype and age, and a possible harvesting effect.
#'   \href{https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2019.24.14.1800118}{Euro Surveill.}
#'   2019;24(14):pii=1800118 (\href{https://www.ncbi.nlm.nih.gov/pubmed/30968823}{PubMed})
#'
#'  \item Gasparrini A, Armstrong B, Kenward MG. Distributed lag non-linear models.
#'   \href{https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.3940}{Stat Med} 2010;29(21):2224–34.
#'
#'  \item Gasparrini A, et al. Mortality risk attributable to high and low ambient temperature:
#'   a multicountry observational study.
#'   \href{https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(14)62114-0/fulltext}{Lancet}
#'   2015 Jul 25;386(9991):369–75.
#' }
#'
#' @import stats
#'
#' @examples
#' data(greece) # Use example surveillance data from Greece
#' m <- with(greece, fitFluMoDL(deaths = daily$deaths,
#'     temp = daily$temp, dates = daily$date,
#'     proxyH1 = weekly$ILI * weekly$ppH1,
#'     proxyH3 = weekly$ILI * weekly$ppH3,
#'     proxyB = weekly$ILI * weekly$ppB,
#'     yearweek = weekly$yearweek))
#' m
#'
#' # Plot the association between A(H1N1)pdm09 activity and mortality
#' #   and the overall temperature-mortality association:
#' plot(m$pred$proxyH1, "overall")
#' plot(m$pred$temp, "overall")
#'
#' # Add the Minimum Mortality Point to the plot:
#' abline(v=m$MMP)
#'
#' # Check the lag-response dimension for the A(H1N1)pdm09 - mortality
#' #   association, for all proxy values, and for an indicative value of 30.
#' plot(m$pred$proxyH1) # Produces a 3D plot, see ?plot.crosspred
#' plot(m$pred$proxyH1, var=30)
#'
#' # Have a look at the data associated with this FluMoDL:
#' str(m$data)
#' tail(m$data)
#'
#' @export
fitFluMoDL <- function(deaths, temp, dates, proxyH1, proxyH3, proxyB, yearweek,
                           proxyRSV=NULL, smooth=TRUE, periodic=TRUE) {

  # Checking all parameters for consistency
  if (class(dates)!="Date") stop("Argument `dates` must be a vector of class 'Date'.")
  if (length(yearweek)!=length(proxyH1) ||
      length(yearweek)!=length(proxyH3) ||
      length(yearweek)!=length(proxyB)) {
    stop("Arguments `proxyH1`, `proxyH3`, `proxyB` and `yearweek` must have the same vector length.")
  }
  if (!is.null(proxyRSV) && length(yearweek)!=length(proxyRSV)) {
    stop("Arguments `proxyRSV` and `yearweek` must have the same vector length.")
  }
  if (length(deaths)!=length(dates) || length(temp)!=length(dates)) {
    stop("Arguments `deaths`, `temp` and `dates` must have the same vector length.")
  }

  # Check for missing values
  miss <- unlist(sapply(c("deaths","temp","dates","proxyH1","proxyH3","proxyB","yearweek","proxyRSV"),
                        function(x) if (!is.null(get(x)) && sum(is.na(get(x)))>0) return(x) else return(character(0))))
  if (length(miss)>0)
    stop(sprintf("No missing values are allowed for argument%s `%s`.",
                 ifelse(length(miss)>1, "s", ""),
                 paste(miss, collapse="`, `")))


  # Check for missing values
  isZero <- unlist(sapply(c("temp","proxyH1","proxyH3","proxyB","proxyRSV"),
                        function(x) if (!is.null(get(x)) && sum(get(x)!=0)==0) return(x) else return(character(0))))
  if (length(isZero)>0)
    stop(sprintf("Argument%s `%s` include%s only zeroes.",
                 ifelse(length(isZero)>1, "s", ""),
                 paste(isZero, collapse="`, `"),
                 ifelse(length(isZero)>1, "", "s")))

  # Check for perfectly collinear proxies
  if (isTRUE(all.equal(proxyH1, proxyH3))) stop("Arguments `proxyH1` and `proxyH3` are identical.")
  if (isTRUE(all.equal(proxyH1, proxyB))) stop("Arguments `proxyH1` and `proxyB` are identical.")
  if (isTRUE(all.equal(proxyH3, proxyB))) stop("Arguments `proxyH3` and `proxyB` are identical.")

  # Check date range
  range_dates <- range(dates)
  date_range <- c(max(c(min(dates), isoweekStart(min(yearweek)))),
                  min(c(max(dates), isoweekStart(max(yearweek))+6)))
  date_range[2] <- date_range[2] - as.integer(format(date_range[2], "%w"))
  if (diff(date_range)<=0) stop("The provided `dates` and `yearweek` do not overlap at all.")
  if (diff(date_range)<60) stop("Cannot fit model with less than 60 days of data.")

  # Setting up everything in a data.frame
  dat <- subset(data.frame(dates, deaths, temp), dates>=date_range[1] & dates<=date_range[2])
  dat$yearweek <- isoweek(dat$dates)
  dat <- dat[,c("yearweek", "dates", "deaths", "temp")]  # Rearrange in a more logical order
  dat$proxyH1 <- proxyH1[match(dat$yearweek, yearweek)]
  dat$proxyH3 <- proxyH3[match(dat$yearweek, yearweek)]
  dat$proxyB <- proxyB[match(dat$yearweek, yearweek)]
  if (!is.null(proxyRSV)) dat$proxyRSV <- proxyRSV[match(dat$yearweek, yearweek)]

  # Check if there's any discontinuity in the dates and yearweeks
  if (sum(diff(dat$dates)!=1)>0)
    stop("Discontinuities found in the `dates` argument.")
  if (sum(is.na(dat$yearweek))>0)
    stop("Discontinuities found in the `yearweek` argument.")

  # Smooth influenza incidence proxies is smooth=TRUE
  if (smooth) {
    dat$proxyH1 <- smooth.spline(dat$proxyH1, df=floor(nrow(dat)/7))$y
    dat$proxyH3 <- smooth.spline(dat$proxyH3, df=floor(nrow(dat)/7))$y
    dat$proxyB <- smooth.spline(dat$proxyB, df=floor(nrow(dat)/7))$y
    dat$proxyH1[dat$proxyH1<0] <- 0
    dat$proxyH3[dat$proxyH3<0] <- 0
    dat$proxyB[dat$proxyB<0] <- 0
    if (!is.null(proxyRSV)) {
      dat$proxyRSV <- smooth.spline(dat$proxyRSV, df=floor(nrow(dat)/7))$y
      dat$proxyRSV[dat$proxyRSV<0] <- 0
    }
  }

  lg <- 30  # 30 days maximum lag (fixed)

  # Create cross-basis matrices
  basis.temp <- crossbasis(dat$temp, lag=lg,
                           argvar=list(fun="bs", degree=2, knots=quantile(dat$temp, c(0.1,0.75,0.9))),
                           arglag=list(fun="ns", knots=logknots(lg,3)))
  basis.proxyH1 <- crossbasis(dat$proxyH1, lag=lg,
                              argvar=list(fun="poly", degree=1),
                              arglag=list(fun="ns", knots=logknots(lg,3)))
  basis.proxyH3 <- crossbasis(dat$proxyH3, lag=lg,
                              argvar=list(fun="poly", degree=1),
                              arglag=list(fun="ns", knots=logknots(lg,3)))
  basis.proxyB <- crossbasis(dat$proxyB, lag=lg,
                             argvar=list(fun="poly", degree=1),
                             arglag=list(fun="ns", knots=logknots(lg,3)))
  if (!is.null(proxyRSV)) {
    basis.proxyRSV <- crossbasis(dat$proxyRSV, lag=lg,
                                 argvar=list(fun="poly", degree=1),
                                 arglag=list(fun="ns", knots=logknots(lg,3)))
  }

  # Fit DLNM
  dat$t <- 1:nrow(dat)   # Linear trend
  dat$doy <- as.integer(format(dat$dates, "%j"))   # Day of the year
  dat$dow <- as.factor(format(dat$dates,"%u"))   # Day of the week
  if (periodic) {
    if (is.null(proxyRSV)) {
      model <- glm(deaths ~ basis.temp + basis.proxyH1 + basis.proxyH3 + basis.proxyB +
                     dow + t + pbs(doy, knots=c(91,182,274)), data=dat, family="quasipoisson")
    } else {
      model <- glm(deaths ~ basis.temp + basis.proxyH1 + basis.proxyH3 + basis.proxyB +
                     basis.proxyRSV + dow + t + pbs(doy, knots=c(91,182,274)), data=dat, family="quasipoisson")
    }
  } else {
    if (is.null(proxyRSV)) {
      model <- glm(deaths ~ basis.temp + basis.proxyH1 + basis.proxyH3 + basis.proxyB +
                     dow + t, data=dat, family="quasipoisson")
    } else {
      model <- glm(deaths ~ basis.temp + basis.proxyH1 + basis.proxyH3 + basis.proxyB +
                     basis.proxyRSV + dow + t, data=dat, family="quasipoisson")
    }
  }

  # Make predictions
  predTemp <- crosspred(basis.temp, model,
                        at = seq(ceiling(min(dat$temp)), floor(max(dat$temp)), 1),
                        bylag=0.2, cen=round(median(dat$temp)), cumul=TRUE)
  # Calculate Minimum Mortality Point
  MMP <- as.integer(names(which(predTemp$allfit==min(predTemp$allfit))))
  # Refit prediction for temperature, centered at the MMP
  predTemp <- crosspred(basis.temp, model,
                        at = seq(ceiling(min(dat$temp)), floor(max(dat$temp)), 1),
                        bylag=0.2, cen=MMP, cumul=TRUE)
  # Predictions (linear) for influenza proxies
  pL <- pretty(c(0,ceiling(max(dat[,c("proxyH1","proxyH3","proxyB")]))), 30)
  predProxyH3 <- crosspred(basis.proxyH3, model, at=pL, bylag=0.2, cen=0, cumul=TRUE)
  predProxyH1 <- crosspred(basis.proxyH1, model, at=pL, bylag=0.2, cen=0, cumul=TRUE)
  predProxyB <- crosspred(basis.proxyB, model, at=pL, bylag=0.2, cen=0, cumul=TRUE)
  if (!is.null(proxyRSV)) {
    predProxyRSV <- crosspred(basis.proxyRSV, model, at=pL, bylag=0.2, cen=0, cumul=TRUE)
  }


  # Pack it all together
  res <- list(
    data = dat, model = model,
    basis = list(temp = basis.temp, proxyH1 = basis.proxyH1,
                 proxyH3 = basis.proxyH3, proxyB = basis.proxyB),
    MMP = MMP,
    pred = list(temp = predTemp, proxyH1 = predProxyH1,
                proxyH3 = predProxyH3, proxyB = predProxyB),
    blup = NULL
  )
  if (!is.null(proxyRSV)) {
    res$basis$proxyRSV <- basis.proxyRSV
    res$pred$proxyRSV <- predProxyRSV
  }
  class(res) <- c("FluMoDL")
  return(res)
}



#' @export
print.FluMoDL <- function(x, ...) {
  cat("\n** FluMoDL model object **\n\n")
  cat(sprintf("From %s to %s (%s rows)\n",
              min(x$data$dates, na.rm=TRUE), max(x$data$dates, na.rm=TRUE), nrow(x$data)))
  cat(sprintf("%s total deaths in the data\n", sum(x$data$deaths)))
  cat(sprintf("Object %s a periodic term.\n", 
              ifelse(hasPeriodic(x), "includes", "does NOT include")))
  cat(sprintf("Minimum mortality point (temperature): %s\n", x$MMP))
  mid <- ceiling(length(x$pred$proxyH1$allfit)/2)
  cat(sprintf("Relative Risk for an indicative influenza incidence proxy of %s: (95%% CI)\n",
              names(x$pred$proxyH1$allfit)[mid]))
  cat(sprintf("Influenza A(H1N1)pdm09 = %.3f (%.3f - %.3f)\n",
              exp(x$pred$proxyH1$allfit[mid]),
              x$pred$proxyH1$allRRlow[mid], x$pred$proxyH1$allRRhigh[mid]))
  cat(sprintf("Influenza A(H3N2)      = %.3f (%.3f - %.3f)\n",
              exp(x$pred$proxyH3$allfit[mid]),
              x$pred$proxyH3$allRRlow[mid], x$pred$proxyH3$allRRhigh[mid]))
  cat(sprintf("Influenza B            = %.3f (%.3f - %.3f)\n",
              exp(x$pred$proxyB$allfit[mid]),
              x$pred$proxyB$allRRlow[mid], x$pred$proxyB$allRRhigh[mid]))
  if (!is.null(x$pred$proxyRSV)) {
    cat("Model contains a cross-basis term for RSV (Respiratory Syncytial Virus)\n")
    cat(sprintf("RR for an indicative RSV incidence proxy of %s = %.3f (%.3f - %.3f)\n",
                names(x$pred$proxyRSV$allfit)[mid],
                exp(x$pred$proxyRSV$allfit[mid]),
                x$pred$proxyRSV$allRRlow[mid],
                x$pred$proxyRSV$allRRhigh[mid]))
  }
}



#' @export
coef.FluMoDL <- function(object, ...) {
  if (!is.null(object$pred)) {
    return(lapply(object$pred[names(object$pred)[grep("proxy", names(object$pred))]], coef))
  } else if (!is.null(object$model)) {
    n <- c("proxyH1", "proxyH3", "proxyB", "proxyRSV")
    n <- n[sapply(n, function(x) length(grep(x, names(coef(object$model))))>0)]
    res <- lapply(n, function(nn) {
      nnn <- grep(nn, names(coef(object$model)))
      coef(object$model)[nnn]
    })
    names(res) <- n
    return(res)
  } else {
    stop("No elements `pred` or `model` found in this object; object is malformed.")
  }
}



#' @export
vcov.FluMoDL <- function(object, ...) {
  if (!is.null(object$pred)) {
    return(lapply(object$pred[names(object$pred)[grep("proxy", names(object$pred))]], vcov))
  } else if (!is.null(object$model)) {
    n <- c("proxyH1", "proxyH3", "proxyB", "proxyRSV")
    n <- n[sapply(n, function(x) length(grep(x, names(coef(object$model))))>0)]
    res <- lapply(n, function(nn) {
      nnn <- grep(nn, names(coef(object$model)))
      vcov(object$model)[nnn,nnn]
    })
    names(res) <- n
    return(res)
  } else {
    stop("No elements `pred` or `model` found in this object; object is malformed.")
  }
}



# Override [[ and [ operators to avoid messing up FluMoDL objects

#' @export
`[[<-.FluMoDL` <- function(x, value) {
  return(x)
}

#' @export
`[<-.FluMoDL` <- function(x, value) {
  return(x)
}

