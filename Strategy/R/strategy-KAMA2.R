
# Kaufman's Adaptive Moving Average (2) Strategy with EMA of Vola
# -----------------------------------------------------------------------------

# STRATEGY
strategy.kama2 <- function(prices, weights=NULL, indicators=NULL, parameters=list(), printSteps=F) {

  # DEFAULT Parameters
  .stratFUN.defaultParams <- list(return.lambda = 0.1, vola.period = 20, vola.lambda = 0.1, threshold = 0, period="none")

  # DECLARE Parameters
  parameters <- .stratFUN.declareParams(defaultParams = .stratFUN.defaultParams, parameters = parameters)

  # VALIDATION Input Parameters!
  return.lambda <- parameters[["return.lambda"]]
  vola.lambda <- parameters[["vola.lambda"]]
  vola.period <- parameters[["vola.period"]] # needed to calculate vola first over the time frame and then apply EWMA
  strat.thre <- parameters[["threshold"]]
  period <- parameters[["period"]]

  if(printSteps==T) print("Parameters set.")

  if (return.lambda > 1 || return.lambda < 0 || vola.lambda > 1 || vola.lambda < 0) stop("Lambdas are restricted to 0 <= lambda <= 1!")
  if (vola.period > nrow(prices)) stop("Volatility period must not be greater than length of assets!")

  # PERIODICAL prices
  if (period != "none")
    prices <- .toPeriod(data=prices, period=period)

  logReturns <- .PricesToLogReturns(prices)

  # SET weights xts if NULL
  if(is.null(weights)) weights <- xts()

  # FUNCTION ewmaFUN to calculate EWMA for an x-vector
  ewmaFUN <- function(x, lambda) {
    m.t <- as.numeric(x[1])
    m.ts <- vapply(as.vector(x), function(x_t) return((m.t <<- lambda * x_t + (1 - lambda) * m.t)), 0)
    return(m.ts)
  }

  # FUNCTION rolling SUM
  # in order to calculate a rolling window sum
  # with a certain lag (window size)
  # from an input vector "data"
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

  # EWMA volatility
  returns <- exp(logReturns)-1
  ewma.vola <- rollVola <- rollingVola(returns, lag=vola.period)[(vola.period+1):dim(prices)[1],]
  ewma.vola[] <- NA #empty values
  ewma.vola[] <- Reduce(cbind, lapply(rollVola, ewmaFUN, lambda=vola.lambda))
  # EWMA returns
  ewma.returns <- ewma.vola*NA #empty values
  ewma.returns[] <- as.matrix(Reduce(cbind, lapply(returns, ewmaFUN, lambda=return.lambda)))[(vola.period+1):dim(prices)[1],]

  # Strategy values
  strat.vals <- ewma.returns/ewma.vola

  if(printSteps==T) print("Strategy values set.")

  # Reduce matrices to same period as strategy values are available
  prices.reduced <- prices[(dim(prices)[1]-dim(strat.vals)[1]+1):dim(prices)[1],]

  # EXTRACT signals
  signals <- -1 * (strat.vals <= -strat.thre)
  signals <- signals + (strat.vals >= strat.thre)

  if(printSteps==T) print("Signal matrix calculated.")

  # SHIFT signals for next trading day period -> shift dates + 1
  signals <- lag(signals, k=1, na.pad=F)

  if(printSteps==T) print("Signal matrix shifted by 1 time period.")
  
  indicators <- list(KAMA2=strat.vals)
  names(indicators) <- paste0("KAMA2(",return.lambda, ",", vola.lambda, ",", vola.period ,")")

  # OUTPUT
  return( list(filters=list(), signals=signals, prices=prices, logReturns=logReturns, weights=weights, indicators=indicators, parameters=parameters) )
}

# plot.kama2 <- function(object, from=NULL, until=NULL, which=NULL, main=NULL) {
#   # GET VALUES
#   prices <- getPrices(object, from=from, until=until, which=which)
#   strat.vals <- getStratVals(object)[["KAMA2.vals"]][index(prices), colnames(prices)]
#   performance <- performance(object, of="assets", from=start(prices), until=end(prices), which=which)
# 
#   # DECLARE Parameters
#   parameters <- getParameters(object)
#   strat.thre <- parameters[["threshold"]]
#   return.lambda <- parameters[["return.lambda"]]
#   vola.lambda <- parameters[["vola.lambda"]]
#   vola.period <- parameters[["vola.period"]]
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
#   # PLOT Output
#   for (i in 1:ncol(prices)) { #i<-1
#     layout(matrix(1:6, ncol=2, byrow=T), widths=c(0.8, 0.2), heights=c(0.5, 0.2, 0.3))
#     #layout.show(4)
# 
#     # PLOT1: Plot Price Values
#     par(mar=c(0, margins[2:4]))
#     plot(prices[,i], main=plot.main[i], minor.ticks=F, axes=F)
#     axis(2, las=2)
# 
#     # PLOT2: LEGEND prices
#     par(mar=rep(0,4))
#     plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
#     legend("left", legend=colnames(prices)[i], lty=c(1), cex=0.8, bty="n")
# 
#     # PLOT3: Plot KAMA2
#     par(mar=c(0, margins[2], 0, margins[4]))
#     plot(prices[,i], ylim=range(strat.vals[,i]), type="n", main="", minor.ticks=F, axes=F) # no data
#     abline(h=0, col="gray") # in order to better see zero-line
#     lines(strat.vals[i], col="red")
#     axis(4, at=pretty(range(strat.vals[,i])), las=2) # right axis
#     # Plot values
#     lines(strat.vals[,i], col="red")
#     if (strat.thre > 0) {
#       abline(h=strat.thre, lty=2, col="blue")
#       abline(h=-strat.thre, lty=2, col="blue")
#     }
# 
#     # PLOT4: LEGEND KAMA2
#     par(mar=rep(0,4))
#     plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
#     if (strat.thre > 0) {
#       legend("left", legend=c(paste0("KAMA2(", return.lambda, ",", vola.period, ",", vola.lambda, ")"), paste0("threshold(", strat.thre, ")")), col=c("red", "blue"), lty=c(1,2), cex=0.8, bty="n")
#     } else {
#       legend("left", legend=c(paste0("KAMA2(", return.lambda, ",", vola.period, ",", vola.lambda, ")")), col=c("red"), lty=c(1), cex=0.8, bty="n")
#     }
# 
#     # PLOT5: PERFORMANCE
#     par(mar=c(margins[1:2], 0, margins[4]))
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
# 
#   } # for prices
# 
#   layout(1) #reset layout
#   par(mar=par.mar) #reset margins
# }
# 
# 
