
# Kaufman's Adaptive Moving Average Strategy
# -----------------------------------------------------------------------------

# STRATEGY
strategy.crossingkama <- function(prices, weights=NULL, indicators=NULL, parameters=list(), printSteps=F) {

  # DEFAULT Parameters
  .stratFUN.defaultParams <- list(er.period = 10, ema.slowest = 30, ema.fastest = 2, ma.k = 100, threshold = 0, period = "none")

  # DECLARE Parameters
  parameters <- .stratFUN.declareParams(defaultParams = .stratFUN.defaultParams, parameters = parameters)

  # SET Parameters
  er.period <- parameters[["er.period"]]
  ema.slowest <- parameters[["ema.slowest"]]
  ema.fastest <- parameters[["ema.fastest"]]
  k <- parameters[["ma.k"]]
  strat.thre <- parameters[["threshold"]]
  period <- parameters[["period"]]

  if(printSteps==T) print("Parameters set.")

  # SET weights xts if NULL
  if(is.null(weights)) weights <- xts()

  # PERIODIC prices
  if (period != "none")
    prices <- .toPeriod(data=prices, period=period)
  logReturns <- .PricesToLogReturns(prices)

  # FUNCTION rolling SUM
  # in order to calculate a rolling window sum
  # with a certain lag (window size)
  # from an input vector "data"
  rollingSum <- function(data, lag=1) {
    sum <- lag * rollmean(x = data, k = lag, align = "right")
    return(sum)
  }

  # EFFICIENT RATIO
  change <- abs(diff(prices, lag=er.period)[(er.period+1):dim(prices)[1]])
  vola <- rollingSum(data=abs(diff(prices, lag=1)[-1,]), lag=(er.period-1))
  if (length(which(vola==0))>0) # exception vola=0
    vola[[which(vola==0)]] <- 1
  efficient.ratio <- change/vola

  # SMOOTHING CONSTANT
  smoothing.const <- (efficient.ratio * (2/(ema.fastest + 1) - 2/(ema.slowest + 1)) + 2/(ema.slowest + 1) )^2

  # KAMA VALUES
  kama.vals <- prices[index(prices) %in% index(smoothing.const)]*NA # initialize
  kama.vals[1,] <- apply(prices[1:(er.period+1),], 2, mean) # initial KAMA value is simple mean

  for (i in 2:nrow(smoothing.const)) { #i<-2
    kama.vals[i,] <- as.matrix(kama.vals[i-1,]) + as.matrix(smoothing.const[i,]) * ( as.matrix(prices[i+er.period,]) - as.matrix(kama.vals[i-1,]))
  }

  # MA VALUES
  ma.vals <- Reduce(cbind, lapply(prices, rollmean, k=k, align="right"))

  # EXTRACT signals
  diff <- kama.vals - ma.vals
  signals <- -1 * (diff <= -strat.thre)
  signals <- signals + (diff >=  strat.thre)

  # SHIFT signals for next trading day period -> shift dates + 1
  signals <- lag(signals, k=1, na.pad=F)

  if(printSteps==T) print("Signal matrix shifted by 1 time period.")
  
  filters <- list(MA=ma.vals, KAMA=kama.vals)
  names(filters) <- c(paste0("MA(", k, ")"), paste0("KAMA(", er.period, ",", ema.slowest, ",", ema.fastest, ")"))

  # OUTPUT
  return( list(filters=filters, signals=signals, prices=prices, logReturns=logReturns, weights=weights, indicators=indicators, parameters=parameters) )
}

# plot.crossingkama <- function(object, from=NULL, until=NULL, which=NULL, main=NULL) {
#   # GET VALUES
#   prices <- getPrices(object, from=from, until=until, which=which)
#   ma.vals <- getStratVals(object)[["MA.vals"]][index(prices), colnames(prices)]
#   kama.vals <- getStratVals(object)[["KAMA.vals"]][index(prices), colnames(prices)]
#   performance <- performance(object, of="assets", from=start(prices), until=end(prices), which=which)
# 
#   # DECLARE Parameters
#   parameters <- getParameters(object)
#   er.period <- parameters[["er.period"]]
#   ema.slowest <- parameters[["ema.slowest"]]
#   ema.fastest <- parameters[["ema.fastest"]]
#   k <- parameters[["ma.k"]]
#   strat.thre <- parameters[["threshold"]]
#   period <- parameters[["period"]]
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
#     layout(matrix(1:4, ncol=2, byrow=T), widths=c(0.8, 0.2), heights=c(0.65, 0.35))
#     #layout.show(2)
# 
#     # PLOT1: Plot Price Values
#     par(mar=c(0, margins[2:4]))
#     plot(prices[,i], main=plot.main[i], minor.ticks=F, axes=F)
#     axis(2, las=2)
#     # PLOT KAMA vals
#     lines(kama.vals[,i], col="red")
#     # PLOT MA vals
#     lines(ma.vals[,i], col="orange")
#     if (strat.thre > 0) {
#       lines(ma.vals[,i] + strat.thre, col="blue", lty=2)
#       lines(ma.vals[,i] - strat.thre, col="blue", lty=2)
#     }
# 
#     # PLOT2: LEGEND prices
#     par(mar=c(0,0,0,0))
#     plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
#     if (strat.thre > 0) {
#       legend("left", legend=c(colnames(prices[,i])
#                                 , paste0("KAMA(", er.period, ",", ema.fastest, ",", ema.slowest, ")")
#                                 , paste0("MA(", k, ")")
#                                 , paste0("threshold(",strat.thre,")"))
#              , col=c("black", "red", "orange", "blue"), lty=c(1,1,1,2), cex=0.8, bty="n");
#     } else {
#       legend("left", legend=c(colnames(prices[,i])
#                                 , paste0("KAMA(", er.period, ",", ema.fastest, ",", ema.slowest, ")")
#                                 , paste0("MA(", k, ")"))
#              , col=c("black", "red", "orange"), lty=c(1,1,1), cex=0.8, bty="n");
#     }
# 
#     # PLOT3: PERFORMANCE
#     par(mar=c(margins[1:2], 0, margins[4]))
#     # pseudo for same time domain
#     plot(prices[,i], ylim=range(performance[,i]), type="n", main="", axes=F)
#     axis(1, at=.index(prices[,i])[axTicksByTime(prices)], labels=names(axTicksByTime(prices)), las=2)
#     axis(4, at=pretty(range(performance[,i])), las=2) # right axis
#     # PERFORMANCE
#     lines(performance[,i], col="darkgray")
# 
#     # PLOT4: LEGEND performance
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
