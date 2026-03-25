
# Kaufman's Adaptive Moving Average (3) Strategy with Eniso's RAI as signal damper
# -----------------------------------------------------------------------------

# STRATEGY
strategy.kama3 <- function(prices, weights=NULL, indicators=NULL, parameters=list(), printSteps=F) {

  # DEFAULT Parameters
  .stratFUN.defaultParams <- list(lambda = 0.01, vola.periods = 10, rf = 0, threshold = 0, period="weeks")

  # DECLARE Parameters
  parameters <- .stratFUN.declareParams(defaultParams = .stratFUN.defaultParams, parameters = parameters)

  # VALIDATION Input Parameters!
  vola.periods <- parameters[["vola.periods"]]
  strat.thre <- parameters[["threshold"]]
  rf <- parameters[["rf"]]
  lambda <- parameters[["lambda"]]
  period <- parameters[["period"]]

  # GET RAI
  if( is.null(indicators) || !"rai" %in% tolower(names(indicators)) ) stop("Please provide RAI indicator!")
  rai <- indicators[["rai"]][,1]
  if (! is.xts(rai)) stop("Please provide RAI as xts!")

  if(printSteps==T) print("Parameters set.")

  # PERIODICAL prices
  if (period != "none") {
    prices <- .toPeriod(data=prices[paste0(min(index(rai)), "::", max(index(rai)))], period=period)
    rai <- .toPeriod(data=rai, period=period)
  }

  prices <- prices[index(rai)]
  logReturns <- .PricesToLogReturns(prices)

  # SET weights xts if NULL
  if(is.null(weights)) weights <- xts()

  if(printSteps==T) print("Periods calculated.")

  # cut Data
  rai <- rai[index(prices)]
  # PREVENT RAI from being too close to 0
#   rai[rai<0.01 & rai>=0] <- 0.01
#   rai[rai>(-0.01) & rai<=0] <- -0.01

  # FUNCTION to linear rescale values between bounds below and above border
  rescaleFUN <- function(values, lbound, ubound, border){
    above <- below <- values
    above[values<border] <- border
    below[values>border] <- border
    # abore border
    currentRange <- max(above) - border
    if (currentRange == 0) {
      above.new <- 0
    } else {
      above.new <- ((above - border) / currentRange ) * ubound
    }

    # below border
    currentRange <- border - min(below)
    if (currentRange == 0) {
      below.new <- 0
    } else {
      below.new <- ((border - below) / currentRange ) * lbound
    }
    return(above.new+below.new)
  }

  # FUNCTION to calculate a rolling window volatility
  # with a certain lag (window size)
  # data: xts
  rollingVola <- function(data, lag) {
    len <- length(data)
    if (lag > len) stop("Lag is larger than length!")
    vola <- data
    if (!is.null(nrow(data))) {
      vola[,] <- NA
      len <- nrow(data)
      if (lag > len) stop("Lag is larger than length!")
      for (i in (lag+1):len) { #i<-2
        vola[i,] <- sd(data[(i-lag):i,])
      }
    } else {
      vola[] <- NA
      for (i in (lag+1):len) { #i<-1
        vola[i] <- sd(data[(i-lag):i])
      }
    }
    return(vola)
  }

  # FUNCTION ewmaFUN to calculate EWMA for an x-vector
  ewmaFUN <- function(x, lambda) {
    m.t <- as.numeric(x[1])
    m.ts <- vapply(as.vector(x), function(x_t) return((m.t <<- lambda * x_t + (1 - lambda) * m.t)), 0)
    return(m.ts)
  }

  ## RETURNS
  returns <- logReturns
  # Rolling Vola
  returns.vola <- rollingVola(returns, lag=vola.periods)[(vola.periods+1):nrow(prices),]
  # Rolling mean
  returns.mean <- Reduce(cbind, lapply(returns, rollmean, k=vola.periods, align="right"))
  # Rolling sharpe
  returns.sharpe <- (returns.mean - log(1+rf)) / returns.vola

  ## RAI
  # Rolling Vola
  rai.vola <- rollingVola(rai, lag=vola.periods)[(vola.periods+1):nrow(prices),]
  # Rolling mean
  rai.mean <- Reduce(cbind, lapply(rai, rollmean, k=vola.periods, align="right"))
  # Rolling sharpe
  rai.sharpe <- rai.mean / rai.vola

  # OBTAIN strategy values
  strat.vals <- Reduce(cbind, lapply(returns.sharpe, function(return.sharpe) {
    factor <- rep(1, nrow(return.sharpe))
    # Exception: both values are negative --> strat vals also negative
    factor[return.sharpe<0 & rai.sharpe<0] <- -1
    return(factor * return.sharpe * rai.sharpe)
    }))

  # RESCALE strat vals to fit in (-1,1)
  strat.vals <- rescaleFUN(strat.vals, lbound=-1, ubound=1, border=0)

  if(printSteps==T) print("Strategy values set.")

  # REDUCE prices to same period as strategy values are available
  prices.reduced <- prices[index(strat.vals),]

  # EXTRACT signals
  signals <- -1 * (strat.vals <= -strat.thre)
  signals <- signals + (strat.vals > strat.thre)

  if(printSteps==T) print("Signal matrix calculated.")

  # SHIFT signals for next trading day period -> shift dates + 1
  signals <- lag(signals, k=1, na.pad=F)

  if(printSteps==T) print("Signal matrix shifted by 1 time period.")
  
  indicators <- list(kama3=strat.vals, RAI=rai)
  names(indicators) <- c(paste0("KAMA3(", lambda, ",", vola.periods, ",", rf, ")"), "RAI")

  # OUTPUT
  return( list(filters=list(), signals=signals, prices=prices, logReturns=logReturns, weights=weights, indicators=list(rai=rai), parameters=parameters) )
}


# plot.kama3 <- function(object, from=NULL, until=NULL, which=NULL, main=NULL) {
#   # GET VALUES
#   prices <- getPrices(object, from=from, until=until, which=which)
#   strat.vals <- getStratVals(object)[["KAMA3.vals"]][index(prices), colnames(prices)]
#   performance <- performance(object, of="assets", from=start(prices), until=end(prices), which=which)
# 
#   # DECLARE Parameters
#   parameters <- getParameters(object)
#   vola.periods <- parameters[["vola.periods"]]
#   strat.thre <- parameters[["threshold"]]
# 
#   # RAI
#   rai <- getIndicators(object)[["rai"]][index(prices),1]
# 
#   # PLOT main
#   if (is.null(main)) {
#     plot.main <- colnames(prices)
#   } else {
#     if (!is.character(main)) stop("Please provide plot headings as character!")
#     if (length(main) == 1) plot.main <- rep(main, ncol(prices))
#   }
#   if (length(plot.main) != ncol(prices))
#     stop("Please provide as many headings as graphics!")
# 
#   par.mar <- par()$mar # keep standard margins
#   margins <- c(7, 4.1, 4.1, 3)
# 
#   for (i in 1:ncol(prices)) { #i<-1
#     layout(matrix(1:6, ncol=2, byrow=T), widths=c(0.8, 0.2), heights=c(0.5, 0.2, 0.3))
#     #layout.show(1)
# 
#     # PLOT1: Plot prices
#     par(mar=c(0, margins[2:4]))
#     plot(prices[,i], main=plot.main[i], minor.ticks=F, axes=F)
#     axis(2, las=2)
# 
#     # PLOT2: LEGEND Prices
#     par(mar=rep(0,4))
#     plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
#     legend("left", legend=colnames(prices)[i], lty=c(1), cex=0.8, bty="n")
# 
#     # PLOT3: Plot KAMA3 and RAI
#     par(mar=c(0, margins[2], 0, margins[4]))
#     ylim <- c(min(cbind(strat.vals[,i]-strat.thre,rai[index(strat.vals)]), rm.na=T), max(cbind(strat.vals[,i]+strat.thre,rai[index(strat.vals)]), rm.na=T))
#     plot(prices[,i], ylim=ylim, type="n", main="", minor.ticks=F, axes=F) # no data
#     abline(h=0, col="gray")
#     # RAI
#     lines(rai, col="black")
#     axis(4, at=pretty(range(strat.vals[,i])), las=2) # right axis
#     # KAMA3
#     lines(strat.vals[,i], col="red")
#     if (strat.thre > 0) {
#       abline(h=strat.thre, lty=2, col="blue")
#       abline(h=-strat.thre, lty=2, col="blue")
#     }
# 
#     # PLOT4: LEGEND KAMA3 i + RAI
#     par(mar=rep(0,4))
#     plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
#     if (strat.thre > 0) {
#       legend("left", legend=c(paste0("KAMA3(", vola.periods, ")"), paste0("threshold(", strat.thre, ")"), "RAI"), col=c("red", "blue", "black"), lty=c(1,2,1), cex=0.8, bty="n")
#     } else {
#       legend("left", legend=c(paste0("KAMA3(", vola.periods, ")"), "RAI"), col=c("red", "black"), lty=c(1,1), cex=0.8, bty="n")
#     }
# 
#     # PLOT5: PERFORMANCE
#     par(mar=c(7, margins[2], 0, margins[4]))
#     # pseudo for same time domain
#     plot(prices[,i], ylim=range(performance[,i]), type="n", main="", axes=F, minor.ticks=F)
#     axis(1, at=.index(prices[,i])[axTicksByTime(prices)], labels=names(axTicksByTime(prices)), las=2)
#     axis(2, las=2) # right axis
#     # PERFORMANCE
#     lines(performance[,i], col="darkgray")
# 
#     # PLOT6: LEGEND performance
#     par(mar=c(margins[1],0,0,0))
#     plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
#     # LEGEND
#     legend("left", legend="Performance", col=c("darkgray"), lty=c(1), cex=0.8, bty="n")
#   } # for prices
# 
#   layout(1) #reset layout
#   par(mar=par.mar) #reset margins
# }


