# BUYHOLD Strategy
# -----------------------------------------------------------------------------

# STRATEGY
strategy.buyhold <- function(prices, weights=NULL, indicators=NULL, parameters=list(), printSteps=F) {

  # DEFAULT Parameters
  .stratFUN.defaultParams <- list(period="none")

  # DECLARE Parameters
  parameters <- .stratFUN.declareParams(defaultParams = .stratFUN.defaultParams, parameters = parameters, printWarnings = F)

  # SET Parameters
  period <- parameters[["period"]]

  if(printSteps==T) print("Parameters set.")

  # PERIODICAL prices
  if (period != "none")
    prices <- .toPeriod(data=prices, period=period)

  logReturns <- .PricesToLogReturns(prices)

  # SET weights xts if NULL
  if(is.null(weights)) weights <- xts()

  # EXTRACT signals
  signals <- prices*0 + 1 # set all on buy

  if(printSteps==T) print("Signal matrix calculated.")

  # NOT SHIFTING THIS SIGNALS as not necessary for buyhold
  # if(printSteps==T) print("Signal matrix shifted by 1 time period.")

  # OUTPUT
  return( list(filters=list(), signals=signals, prices=prices, logReturns=logReturns, weights=weights, indicators=indicators, parameters=parameters) )
}

# plot.buyhold <- function(object, from=NULL, until=NULL, which=NULL, main=NULL) {
#   # GET VALUES
#   prices <- getPrices(object, from=from, until=until, which=which)
#   performance <- performance(object, of="assets", from=start(prices), until=end(prices), which=which)
# 
#   # DECLARE Parameters
#   # NONE to be declared
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
#     layout(matrix(1:4, ncol=2, byrow=T), widths = c(0.8, 0.2), heights=c(0.65, 0.35))
#     #layout.show(1)
#     par(mar=c(0, margins[2:4]))
# 
#     # PLOT1: Plot Price Values
#     plot(prices[,i], main=plot.main[i], minor.ticks=F, axes=F)
#     axis(2, las=2)
# 
#     # PLOT2: pseudo
#     par(mar=rep(0,4))
#     plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
#     legend("left", legend=colnames(prices[,i]), lty=c(1), col=c("black"), cex=0.8, bty="n")
# 
#     # PLOT3: PERFORMANCE
#     par(mar=c(margins[1:2], 0, margins[4]))
#     # pseudo for same time domain
#     plot(prices[,i], ylim=range(performance[,i]), type="n", main="", axes=F)
#     axis(1, at=.index(prices[,i])[axTicksByTime(prices)], labels=names(axTicksByTime(prices)), las=2)
#     axis(4, at=pretty(range(performance[,i])), las=2) # right axis
#     # performance
#     lines(performance[,i], col="darkgray")
# 
#     # PLOT4: LEGEND PERFORMANCE
#     par(mar=c(margins[1],0,0,0))
#     plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
#     legend("left", legend="Performance", col=c("darkgray"), lty=c(1), cex=0.7, bty="n")
# 
#   } # for prices
# 
#   layout(1) #reset layout
#   par(mar=par.mar) #reset margins
# }
