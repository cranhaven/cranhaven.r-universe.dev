# Moving Average Strategy
# -----------------------------------------------------------------------------

# STRATEGY
strategy.ewma <- function(prices, weights=NULL, indicators=NULL, parameters=list(), printSteps=F) {

  # DEFAULT Parameters
  .stratFUN.defaultParams <- list(lambda = 0.1, threshold = 0, period="none")

  # DECLARE Parameters
  parameters <- .stratFUN.declareParams(defaultParams = .stratFUN.defaultParams, parameters = parameters)

  # VALIDATION Input Parameters!
  lambda <- parameters[["lambda"]]
  strat.thre <- parameters[["threshold"]]
  period <- parameters[["period"]]

  if (is.null(lambda) || !is.numeric(lambda) || length(lambda) !=1
      || lambda < 0 || lambda > 1)
    stop("Lambda has to be a single numeric value between 0 and 1!")

  if(printSteps==T) print("Parameters set.")

  # PERIODICAL prices
  if (period != "none")
    prices <- .toPeriod(data=prices, period=period)

  logReturns <- .PricesToLogReturns(prices)

  # SET weights xts if NULL
  if(is.null(weights)) weights <- xts()

  # FUNCTION ewmaFUN to calculate EWMA for an x-vector and given lambda
  ewmaFUN <- function(x, lambda) {
    m.t <- as.numeric(x[1])
    m.ts <- vapply(as.vector(x), function(x_t) return((m.t <<- lambda * x_t + (1 - lambda) * m.t)), 0)
    return(m.ts)
  }

  # STRATEGY VALUES
  # strat.vals <- Reduce(cbind, lapply(prices, fTrading::emaTA, lambda=lambda, startup=0))
  strat.vals <- prices*NA #initialize
  strat.vals[,] <- Reduce(cbind, lapply(prices, ewmaFUN, lambda=lambda))

  if(printSteps==T) print("Strategy values set.")

  # Reduce matrices to same period as strategy values are available
  prices.reduced <- prices[(nrow(prices)-nrow(strat.vals)+1):nrow(prices),]

  # EXTRACT signals
  signals <- -1 * (prices.reduced <= (strat.vals - strat.thre))
  signals <- signals + (prices.reduced >= (strat.vals + strat.thre))

  if(printSteps==T) print("Signal matrix calculated.")

  # SHIFT signals for next trading period -> shift dates + 1
  signals <- lag(signals, k=1, na.pad=F)

  if(printSteps==T) print("Signal matrix shifted by 1 time period.")
  
  filters <- list(ewma=strat.vals)
  names(filters) <- paste0("EWMA(", lambda, ")")

  # OUTPUT
  return( list(filters=filters, signals=signals, prices=prices, logReturns=logReturns, weights=weights, indicators=indicators, parameters=parameters) )
}

# plot.ewma <- function(object, from=NULL, until=NULL, which=NULL, main=NULL) {
# 
#   # GET VALUES
#   prices <- getPrices(object, from=from, until=until, which=which)
#   strat.vals <- getStratVals(object)[["EWMA.vals"]][index(prices), colnames(prices)]
#   performance <- performance(object, of="assets", from=start(prices), until=end(prices), which=which)
# 
#   # DECLARE Parameters
#   parameters <- getParameters(object)
#   lambda <- parameters[["lambda"]]
#   strat.thre <- parameters[["threshold"]]
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
#     # LAYOUT
#     layout(matrix(1:4, ncol=2, byrow=T), widths=c(0.8, 0.2), heights=c(0.65, 0.35))
#     #layout.show(1)
# 
#     # PLOT1: Plot Price Values
#     par(mar=c(0, margins[2:4]))
#     plot(prices[,i], main=plot.main[i], minor.ticks=F, axes=F)
#     axis(2, las=2)
#     lines(strat.vals[,i], col="red")
#     if (strat.thre > 0) {
#       lines(strat.vals[,i] + strat.thre, lty=2, col="blue")
#       lines(strat.vals[,i] - strat.thre, lty=2, col="blue")
#     }
# 
#     # PLOT2: LEGEND prices
#     par(mar=rep(0,4))
#     plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
#     # LEGEND
#     legend("left", legend=c(colnames(prices)[i], paste0("EWMA(",lambda ,")")), lty=c(1,1), col=c("black","red"), cex=0.8, bty="n")
# 
#     # PLOT3: PERFORMANCE
#     par.mar.top0 <- c(margins[1:2], 0, margins[4])
#     par(mar=par.mar.top0)
#     # pseudo for same time domain
#     plot(prices[,i], ylim=range(performance[,i]), type="n", main="", axes=F)
#     axis(1, at=.index(prices[,i])[axTicksByTime(prices)], labels=names(axTicksByTime(prices)), las=2)
#     axis(4, at=pretty(range(performance[,i])), las=2) # right axis
#     # PERFORMANCE
#     lines(performance[,i], col="darkgray")
# 
#     # PLOT4: LEGEND performance
#     par(mar=c(par.mar.top0[1],0,0,0))
#     plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
#     # LEGEND
#     legend("left", legend="Performance", col=c("darkgray"), lty=c(1), cex=0.8, bty="n")
# 
#   } # for prices
# 
#   layout(1) #reset layout
#   par(mar=par.mar) #reset margins
# }
