
# Faber's Ivy Portfolio Approach
# -----------------------------------------------------------------------------

strategy.faber <- function(prices, weights=NULL, indicators=NULL, parameters=list(), printSteps=F) {

  # DEFAULT Parameters
  .stratFUN.defaultParams <- list(k = 10)

  # DECLARE Parameters
  parameters <- .stratFUN.declareParams(defaultParams = .stratFUN.defaultParams, parameters = parameters)

  # SET Parameters
  k <- parameters[["k"]]
  parameters[["period"]] <- period <- "months" # fixed period in Faber's rule

  if(printSteps==T) print("Parameters set.")

  # PERIODIC
  prices.periodic <- .toPeriod(prices, period=period)
  logReturns <- .PricesToLogReturns(prices.periodic)

  # SET weights xts if NULL
  if(is.null(weights)) weights <- xts()

  if(printSteps==T) print("Periods calculated.")

  # STRATEGY VALUES
  strat.vals <- Reduce(cbind, lapply(prices, rollmean, k=21*k, align="right")) # approximately 21 average trading days a month

  if(printSteps==T) print("Strategy values set.")

  # REDUCE prices to same period as strategy values are available
  prices.reduced <- prices.periodic[index(strat.vals),]

  # EXTRACT signals
  signals <- 1*(prices.reduced >= strat.vals)
  signals <- na.locf(merge.xts(signals, prices[,1])[, 1:ncol(signals)])

  if(printSteps==T) print("Signal matrix calculated.")

  # SHIFT signals for next trading day period -> shift dates + 1
  signals <- lag(signals, k=1, na.pad=F)

  if(printSteps==T) print("Signal matrix shifted by 1 time period.")
  
  filters <- list(faber=strat.vals)
  names(filters) <- paste0("Faber(",k,")")

  # OUTPUT
  return( list(filters=filters, signals=signals, prices=prices, logReturns=logReturns, weights=weights, indicators=indicators, parameters=parameters) )
}

# plot.faber <- function(object, from=NULL, until=NULL, which=NULL, main=NULL) {
#   # GET VALUES
#   prices <- getPrices(object, from=from, until=until, which=which)
#   strat.vals <- getStratVals(object)[["Faber.vals"]][index(prices), colnames(prices)]
#   performance <- performance(object, of="assets", from=start(prices), until=end(prices), which=which)
# 
#   # DECLARE Parameters
#   parameters <- getParameters(object)
#   k <- parameters[["k"]]
#   period <- parameters[["period"]]
#   prices <- .toPeriod(prices, period=period)
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
#   for (i in 1:ncol(prices)) {
# 
#     layout(matrix(1:4, ncol=2, byrow=T), widths = c(0.8, 0.2), heights=c(0.65, 0.35))
#     #layout.show(4)
# 
#     # PLOT1: Plot Price Values
#     par(mar=c(0, margins[2:4]))
#     plot(prices[,i], main=plot.main[i], minor.ticks=F, las=2, axes=F)
#     axis(2, las=2)
#     # Add Faber MA values
#     lines(strat.vals[,i], col="red")
# 
#     # PLOT2: pseudo
#     par(mar=rep(0,4))
#     plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
#     # LEGEND
#     legend("left", legend=c(colnames(prices)[i], paste0("Faber(",k ,")")), lty=c(1,1), col=c("black","red"), cex=0.8, bty="n")
# 
#     # PLOT3: PERFORMANCE
#     par(mar=c(margins[1:2], 0, margins[4]))
#     # pseudo for same time domain
#     plot(prices[,i], ylim=range(performance[,i]), type="n", main="", axes=F)
#     axis(1, at=.index(prices[,i])[axTicksByTime(prices)], labels=names(axTicksByTime(prices)), las=2)
#     axis(4, at=pretty(range(performance[,i])), las=2) # right axis
#     # PERFROMANCE
#     lines(performance[,i], col="darkgray")
# 
#     # PLOT4: LEGEND PERFORMANCE
#     par(mar=c(margins[1],0,0,0))
#     plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
#     # LEGEND
#     legend("left", legend="Performance", col=c("darkgray"), lty=c(1), cex=0.8, bty="n")
#   } # for prices
# 
#   layout(1) #reset layout
#   par(mar=par.mar) #reset margins
# }
