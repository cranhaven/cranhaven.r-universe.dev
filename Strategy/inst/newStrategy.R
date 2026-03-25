# TITLE
# -----------------------------------------------------------------------------


# STRATEGY
stratFUN <- function(prices, weights=NULL, indicators=NULL, parameters=list(), printSteps=F) {
  
  # DEFAULT Parameters
  .stratFUN.defaultParams <- list(param = 1, param2 = 10, period="none")
  
  # DECLARE Parameters
  parameters <- .stratFUN.declareParams(defaultParams = .stratFUN.defaultParams, parameters = parameters)
  
  # VALIDATION Input Parameters!
  period <- parameters[["period"]]
  
  # PERIODICAL prices
  if (period != "none")
    prices <- .toPeriod(data=prices, period=period)
  
  # GET Log Returns
  logReturns <- .PricesToLogReturns(prices)
  
  # EXTRACT signals
  signals <- -1 # * (condition of sell-rule)
  signals <- signals # + (condition of buy-rule)
  
  if(printSteps==T) print("Signal matrix calculated.")
  
  # SHIFT signals for next trading period -> shift dates + 1
  signals_shifted <- signals[-nrow(signals),]
  index(signals_shifted) <- index(signals[-1,])
  signals <- signals_shifted
  
  if(printSteps==T) print("Signal matrix shifted by 1 time period.")
  
  # OUTPUT
  return( list(strat.vals=list(strat.vals=strat.vals), signals=signals, prices=prices, logReturns=logReturns, weights=weights, indicators=indicators, parameters=parameters) )
}


# DEFINE PLOT OUTPUT
plotFUN <- function(object, from=NULL, until=NULL, which=NULL, main=NULL) {
  
  # GET VALUES
  prices <- getPrices(object, from=from, until=until, which=which)
  strat.vals <- getStratVals(object)[["strat.vals"]][index(prices), colnames(prices)]
  performance <- performance(object, of="assets", from=start(prices), until=end(prices), which=which)
  
  # DECLARE Parameters
  parameters <- getParameters(object)
  # example: lambda <- parameters[["lambda"]]
  
  # PLOT main
  if (is.null(main)) {
    plot.main <- colnames(prices)
  } else {
    if (!is.character(main)) stop("Please provide plot headings as character!")
    if (length(main) == 1) plot.main <- rep(main, ncol(prices))
  }
  if (length(plot.main) != ncol(prices))
    stop("Please provide as many headings as graphics!")
  
  par.mar <- par()$mar # keep standard margins
  margins <- c(7, 4.1, 4.1, 3) 
  
  # PLOT Output
  for (i in 1:ncol(prices)) { #i<-1
    # DEFINE LAYOUT
    layout(matrix(1:4, ncol=2, byrow=T), widths=c(0.8, 0.2), heights=c(0.65, 0.35))
    #layout.show(1)
    
    # PLOT1: Plot Price Values
    # example
    # ------------------------------------------------------------------------------------------------------------------------
    # par(mar=c(0, margins[2:4]))
    # plot(prices[,i], main=plot.main[i], minor.ticks=F, axes=F)
    # axis(2, las=2)
    
    # PLOT2: LEGEND prices
    # example:
    # ------------------------------------------------------------------------------------------------------------------------
    # par(mar=rep(0,4))
    # plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
    # LEGEND
    # legend("left", legend=c(colnames(prices)[i], paste0("EWMA(",lambda ,")")), lty=c(1,1), col=c("black","red"), cex=0.8, bty="n")
    
    
  } # for prices
  
  layout(1) #reset layout
  par(mar=par.mar) #reset margins
}
