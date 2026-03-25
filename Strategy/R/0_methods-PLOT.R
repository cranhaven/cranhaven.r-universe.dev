#if (!isGeneric("plot"))
#  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
# setMethod(f = "plot",
#           signature(x="Strategy",y="missing"),
#           definition = function(x, y, from=NULL, until=NULL, which.assets=NULL, which.filters=NULL, which.indicators=NULL, main=NULL, show.signals=TRUE, include.costs=TRUE, ...) {


#' @export
#' @name plot
#' @aliases plot,Strategy,missing-method
#' @title Plot of a \code{Strategy}-object
#' @description Calls a generic \code{plot} function that can plot the data of any \code{Strategy}-object. If a \code{plotFUN}-function is given within the object, this user-defined function will be used. The generic function plots 3 parts:
#' \itemize{
#'    \item{Price area} Plots the asset price data and filters.
#'    \item{Indicator area} Plots the indicators and trading signals.
#'    \item{Performance area} Plots performance of the strategy.
#' }
#' @usage \method{plot}{Strategy}(x, y, from=NULL, until=NULL
#'        , which.assets=NULL, which.filters=NULL, which.indicators=NULL
#'        , main=NULL, show.signals=TRUE, include.costs=TRUE, ...)
#' @param x An object of class \code{Strategy}.
#' @param y Standard plot argument that is not relevant for Strategy objects!
#' @param from From date that chart is to be plotted.
#' @param until Until date that chart is to be plotted.
#' @param which.assets Which assets shall be plotted (each one will result in single plot)
#' @param which.filters Which filters shall be added to price plot. Default value \code{NULL} will return all filters from the strategy.
#' @param which.indicators Which indicators shall be added to indicator plot. Default value \code{NULL} will return all filters from the strategy. If \code{"none"}, no indicator is plotted and indicator area is not shown.
#' @param main The main title of the plot.
#' @param show.signals If \code{TRUE}, the trading signals are shown within the indicators area of the plot. Default value is \code{TRUE}.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is redundant if no costs are given.
#' @param ... Further arguments passed to custom plotFUN (if available) of the object (x).
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Plot first asset of MA(200)-Strategy
#' plot(myStrat.MA, from="2015-01-01", until="2015-12-31", which.assets=1)
#'
#' ##End(Not run)
plot.Strategy <- function(x, y, from=NULL, until=NULL, which.assets=NULL, which.filters=NULL, which.indicators=NULL, main=NULL, show.signals=TRUE, include.costs=TRUE, ...) {

            object <- x
            which <- which.assets
            fontsize <- 1 # generally not change

            if (!is.null(object@plotFUN)) {
              plot.args.def <- names(formals(object@plotFUN))
              plot.args.list <- list(object=object, which=which, from=from, until=until, which.filters=which.filters
                                   , which.indicators=which.indicators, main=main, show.signals=show.signals, '...'=list(...))
              plot.args <- plot.args.list[which(names(plot.args.list) %in% plot.args.def)]
              do.call(object@plotFUN, plot.args)

            } else { # if no plotFUN defined by strategy


              if (!is.logical(show.signals))
                stop("Argument show.signals must be a boolean!")

              par(mgp=c(2.5, 1, 0))

              # GET VALUES
              prices <- getPrices(object, from=from, until=until, which=which)
              filters <- lapply(getFilters(object, which=which.filters), function(x) x[paste0(start(prices),"::",end(prices)), colnames(prices)])

              indicators <- lapply(getIndicators(object, which=which.indicators), function(x) x[paste0(start(prices),"::",end(prices))])
              performance <- performance(object, of="assets", from=start(prices), until=end(prices), which=which, include.costs=include.costs)

              # lengths
              flen <- length(filters)
              ilen <- length(indicators)

              # define plot range for price window
              if (flen!=0) {
                prices.min <- pmin(apply(prices, 2, min), apply(matrix(Reduce(rbind, lapply(filters, function(filter) apply(filter, 2, min))), ncol=ncol(prices)), 2, min) )
                prices.max <- pmax(apply(prices, 2, max), apply(matrix(Reduce(rbind, lapply(filters, function(filter) apply(filter, 2, max))), ncol=ncol(prices)), 2, max) )
              } else {
                prices.min <- apply(prices, 2, min)
                prices.max <- apply(prices, 2, max)
              }

              if (show.signals==TRUE) {
                signals <- getSignals(object, which=which, from=start(prices), until=end(prices))
                trades <- sign(abs(getTrades(object, which=which, from=start(prices), until=end(prices))))
                trades[1,] <- 1 # to ensure plotting
                trades[nrow(trades),] <- 1 # to ensure plotting
              }

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

              # layout conditions
              layout.len <- 6
              heights <- c(0.45, 0.2, 0.35)
              if (show.signals==FALSE && ilen==0) {
                layout.len <- 4
                heights <- c(0.6, 0.4)
              }


              # PLOT1 Output
              for (i in 1:ncol(prices)) { #i<-1
                layout(matrix(1:layout.len, ncol=2, byrow=TRUE), widths=c(0.8, 0.2), heights=heights)
                #layout.show(2)

								prices_i <- as.zoo(prices[,i])

                # PLOT1: Plot Price Values & FILTERS
							  par(mar=c(0, margins[2:4]))
							  plot.zoo(prices_i, ylim=c(prices.min[i],prices.max[i]), main=plot.main[i]
										, xaxt="n", yaxt="n", type="n", ylab="", xlab="")
							  lines(prices_i, col="black")
							  axis(2, las=2)
							  # Draw filter vals
							  if (flen > 0) {
							    for (fNo in 1:flen) {
							      lines(as.zoo(filters[[fNo]][,i]), col=rainbow(flen)[fNo])
							    }
							  }
							  graphics::box()

                # PLOT2: LEGEND prices
                par(mar=c(0,0,0,0))
                plot(1:2, 1:2, type="n", axes=FALSE, ann=FALSE) #only for layout
                legend.names <- c(colnames(prices_i), names(filters))
                legend("left", legend=legend.names, col=c("black",rainbow(flen)), lty=rep(1,flen+1), cex=fontsize, bty="n");

                if (layout.len == 6) {

                  # PLOT3: indicators & signals
                  par(mar=c(0, margins[2], 0, margins[4]))
                  # pseudo for same time domain
                  plot.zoo(prices_i, ylim=c(-1,1), type="n", main="", xaxt="n", yaxt="n", xlab="", ylab="")

                  # signals
                  if (show.signals==TRUE) {
                    par(new=TRUE)
                    trades_i <- trades[trades[,i]!=0,i]
                    signals_i <- signals[index(trades_i),i]
                    signals_i <- na.fill(na.locf(merge.xts(signals[,i], prices_i)[,1]), 0) # resolve time domain issue
                    signals_range <- c(-1,1)*max(abs(signals_i), na.rm=TRUE) # 0 is middle
                    # plot init / pseudo range for rectangles
										plot.zoo(signals_i, ylim=signals_range, type="n", main="", xaxt="n", yaxt="n", ylab="", xlab="")
                    # plot colors
                    cols <- signals_i*NA
                    cols[signals_i>0] <- "lightblue"
                    cols[signals_i<0] <- "lightblue" # might be changed later to different signal color
										rect(xleft = index(signals_i), ybottom = rep(0,nrow(signals_i))
													, xright = c(index(signals_i[2:nrow(signals_i),]), index(prices_i[nrow(prices),]))
													, ytop = signals_i, col = cols, border = NA)
                  }
									## MIDDLE/ZERO LINE
              		lines(x=index(prices_i), y=rep(0,nrow(prices_i)), col="grey")
									
									# indicators
                  if (ilen > 0) {
                    for (indNo in 1:ilen) { #indNo<-1
                      par(new=TRUE)
                      ind <- merge.xts(indicators[[indNo]], prices_i)[,1] # resolve time domain issue
											ind <- ind/max(abs(ind), na.rm=TRUE) # make interval [-1, 1]
                      ind_range <- c(-1,1)
                      plot.zoo(na.locf(ind), ylim=ind_range, col=rainbow(ilen)[indNo]
														, type="l", main="", xaxt="n", yaxt="n", ylab="", xlab="")
                    }
                  }
                  graphics::box() # draw box line

                  # PLOT4: LEGEND indicators
                  par(mar=c(0,0,0,0))
                  plot(1:2, 1:2, type="n", axes=FALSE, ann=FALSE) #only for layout
                  if (show.signals==TRUE) {
                    legend("left", legend=c("Position", names(indicators)), col=c("lightblue", rainbow(ilen)), lty=rep(1,1+ilen), cex=fontsize, bty="n");
                  } else {
                    legend("left", legend=names(indicators), col=rainbow(ilen), lty=rep(1,ilen), cex=fontsize, bty="n");
                  }
                }

                # PLOT5: PERFORMANCE
                par(mar=c(margins[1:2], 0, margins[4]))
                # pseudo for same time domain
								performance_i = na.locf(merge.xts(prices_i, performance[,i])[,-1])
                plot.zoo(performance_i, type="l", main="", xaxt="n", yaxt="n", ylab="", xlab="", col="grey")
                axis(1, at=index(performance_i)[axTicksByTime(performance_i)], labels=names(axTicksByTime(performance_i)), las=2)
                axis(4, at=pretty(range(performance_i)), las=2) # right axis

                # PLOT6: LEGEND performance
                par(mar=c(margins[1],0,0,0))
                plot(1:2, 1:2, type="n", axes=FALSE, ann=FALSE) #only for layout
                # LEGEND
                legend("left", legend="Performance", col=c("darkgray"), lty=1, cex=fontsize, bty="n")

              } # for prices

              layout(1) #reset layout
              par(mar=par.mar) #reset margins
            }
          } # end if no plotFUN defined
#)


#' @export
#' @name plotPerformance
#' @title Plot Strategy Performance
#' @description Plots performance of an object of class \code{Strategy}.
#' @param object An object of class \code{Strategy}.
#' @param which Names or number of assets that should be included in performance. If a portfolio performance from only a subset of the assets is calculated, the weights are scaled accordingly.
#' @param of Performance to be extracted from assets separately or the portfolio performance.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which performance shall be plotted. If \code{NULL}, the start date of the performances is used.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which performance shall be plotted. If \code{NULL}, the end date of the performances is used.
#' @param use.backtest If \code{TRUE}, the signals from the backtesting output are considered for performance calculation. If \code{FALSE}, the signals from the normal strategy execution with the intial parameters are used.
#' @param plot.params If set to TRUE, the parameters used for the performance periods are plotted into the chart. Requires that use.backtest is set to \code{TRUE}.
#' @param plot.params.names New parameter names to be shown can be supplied. Requires that use.backtest is set to \code{TRUE} to take effect.
#' @param plot.params.first If \code{TRUE}, the parameter for the first period is plotted. Otherwise, the parameters are plot at the point on the x-axis, from which they are valid. Requires that use.backtest is set to \code{TRUE} to take effect.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is redundant if no costs are given.
#' @param ... Further arguments that can be passed to the underlying plot()-function.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Plot MA(200)-Strategy
#' plotPerformance(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' # Plot backtested MA(200)-Strategy
#' # plotPerformance(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=TRUE)
#'
#' ##End(Not run)
setGeneric(name = "plotPerformance",
           def = function(object, which=NULL, of="portfolio", from=NULL, until=NULL, use.backtest=FALSE, include.costs=TRUE, plot.params=TRUE, plot.params.names=NULL, plot.params.first=TRUE, ...) {
             standardGeneric("plotPerformance")
           }
)

#' @rdname plotPerformance
#' @aliases plotPerformance,Strategy-method
setMethod(f = "plotPerformance",
          signature = "Strategy",
          definition = function(object, which, of=c("portfolio", "assets"), from, until, use.backtest, include.costs, plot.params, plot.params.names, plot.params.first, ...) {
            # match argument "of" --> standard set in setGeneric function
            of <- match.arg(of)
            if (!is.logical(plot.params) || !is.logical(plot.params.first))
              stop("Please provide plot.params and plot.params.first as logical!")

            performance <- performance(object, of=of, which=which, from=from, until=until, use.backtest=use.backtest, include.costs=include.costs)

            # PLOT Parameters / check for heading
            args <- list(...)
            plot.main <- colnames(performance)
            if ("main" %in% names(args))
              plot.main <- args[["main"]]
            if (length(plot.main) != ncol(performance))
              stop("Please provide as many headings as graphics!")
            args.inUse <- which(names(args) %in% "main")
            if (length(args.inUse) > 0)
              args <- args[-args.inUse] # select only editable arguments for plot

            for (i in 1:ncol(performance)) {
              par(mar=c(7, 4.1, 4.1, 2.1), mgp=c(2.5, 1, 0))
              do.call(function(...) { # using do.call for passing  plot arguments from ellipsis
                plot(performance[,i], main=plot.main[i], minor.ticks=F, las=2, axes=FALSE, ...)
              }, args)
              axis(1, at=.index(performance[,i])[axTicksByTime(performance)], labels=names(axTicksByTime(performance)), las=2)
              axis(2, at=pretty(range(performance[,i])), las=2)

              if (use.backtest == T && of != "portfolio" && plot.params == TRUE) {
                paramData_all <- getParameters(object, use.backtest=TRUE)[[colnames(performance[,i])]] # get all
                paramData_all <- as.xts(apply(paramData_all, 2, function(x) if(is.numeric(x)) round(x, 3)))
                if (max(paramData_all) > 1000)
                  warning("Parameter values > 1000 but only first 3 numbers are shown!")

                # check for costum param names
                name.len <- 3 # standard 3 chars to display
                if (!is.null(plot.params.names)) {
                  if (length(plot.params.names) != ncol(paramData_all)) {
                    stop("Please provide as many plot.params.names as parameters are backtested! You can check by calling getParameters(stratObj, use.backtest=TRUE).")
                  } else {
                    colnames(paramData_all) <- plot.params.names
                    name.len <- max(nchar(plot.params.names))
                  }
                }
                paramData.lbl <- matrix(apply(paramData_all, 1, function(params) paste(paste(substring(names(params),1,name.len), substring(params,1,3), sep="="), collapse="\n")))
                paramData <- xts(paramData.lbl, order.by=index(paramData_all))
                if (plot.params.first == T) {
                  paramData.idx <- as.Date(index(paramData))
                  paramData.before <- which(as.Date(paramData.idx) < start(performance[,i]))
                  if (length(paramData.before) > 0) {
                    paramData.idx[last(paramData.before)] <- start(performance[,i])
                  }
                  index(paramData) <- paramData.idx
                }
                paramData <- paramData[paste0(start(performance[,i]),"::",end(performance[,i])),]
                abline(v=.index(paramData), col="blue")
                text(x=.index(paramData), y=1, labels=paramData, adj=0, col="blue", cex=0.7)
              } #if plot parameters

            } #for performance

          } #method function
)


#' @export
#' @name plotDrawdowns
#' @title Plot Strategy Drawdowns
#' @description Plots drawdowns of the performance of an object of class \code{Strategy}.
#' @param object An object of class \code{Strategy}.
#' @param which Names or number of assets that should be included in performance. If a portfolio performance from only a subset of the assets is calculated, the weights are scaled accordingly.
#' @param of Performance to be extracted from assets separately or the portfolio performance.
#' @param type If the \code{absolute} or \code{relative} drawdown of the performance shall be returned.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which drawdowns shall be plotted. If \code{NULL}, the start date of the performances is used.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which drawdowns shall be plotted. If \code{NULL}, the end date of the performances is used.
#' @param use.backtest If \code{TRUE}, the signals from the backtesting output are considered for drawdowns calculation. If \code{FALSE}, the signals from the normal strategy execution with the initial parameters are used.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is redundant if no costs are given.
#' @param returnValues If \code{TRUE}, the drawdown values are returned.
#' @param ... Further arguments that can be passed to the underlying plot()-function.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Plot MA(200)-Strategy drawdowns
#' plotDrawdowns(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' # Plot backtested MA(200)-Strategy drawdowns
#' # plotDrawdowns(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=TRUE)
#'
#' ##End(Not run)
setGeneric(name = "plotDrawdowns",
           def = function(object, from=NULL, until=NULL, which=NULL, of="portfolio", type="relative", include.costs=TRUE, use.backtest=FALSE, returnValues=FALSE, ...) {
             standardGeneric("plotDrawdowns")
           }
)


#' @rdname plotDrawdowns
#' @aliases plotDrawdowns,Strategy-method
setMethod(f = "plotDrawdowns",
          signature = "Strategy",
          definition = function(object, from, until, which, of=c("portfolio","assets"), type=c("relative","absolute"), include.costs, use.backtest, returnValues, ...) {
            of <- match.arg(of)
            type <- match.arg(type)

            if (!is.logical(returnValues))
              stop("Argument returnValues must be a boolean!")

            performance <- performance(object, of=of, which=which, from=from, until=until, include.costs=include.costs, use.backtest=use.backtest)

            drawdowns <- function(x, type) {
              x.dates <- index(x)
              x <- as.matrix(x)
              # get Drawdowns
              dd <- cummax(x) - x
              # get Max Drawdown Position
              to.pos <- last(which(dd==max(dd))) # to which position
              # get absolute MDD
              mdd <- dd[to.pos]
              # from which position drops
              from.pos <- last(which(x==cummax(x)[to.pos]))
              # get max value (mdd drop start)
              from <- x[from.pos]
              if (type=="relative") {
                dd <- dd/cummax(x) # in percent
                mdd <- mdd/from # in percent
              }
              return(list(dd=-dd, mdd=-mdd, from=from.pos, to=to.pos))
            }

            dd.list <- lapply(performance, FUN=function(x, type) drawdowns(x,type), type=type)
            dd <- as.xts(Reduce(cbind, lapply(dd.list, FUN=function(x) x[["dd"]])))
            from <- Reduce(cbind, lapply(dd.list, FUN=function(x) x[["from"]]))
            to <- Reduce(cbind, lapply(dd.list, FUN=function(x) x[["to"]]))
            mdd <- Reduce(cbind, lapply(dd.list, FUN=function(x) x[["mdd"]]))

            # PLOT Parameters / check for heading
            args <- list(...)
            plot.main <- paste0(toupper(substring(type,1,1)), tolower(substring(type,2)), " Drawdowns of ", colnames(performance))
            if ("main" %in% names(args))
              plot.main <- args[["main"]]
            if (length(plot.main) != ncol(performance))
              stop("Please provide as many headings as graphics!")
            args.inUse <- which(names(args) %in% "main")
            if (length(args.inUse) > 0)
              args <- args[-args.inUse] # select only editable arguments for plot

            par.mar <- par()$mar # keep standard margins
            margins <- c(7, 4.1, 4.1, 3)

            for (i in 1:ncol(dd)) { #i<-1
              par(mar=margins)
              # plot(dd[,i], ylim=c(min(dd[,i])*1.04, 0), type="n", main=plot.main[i], minor.ticks=F, las=2, axes=F, yaxs="i", xaxs="i", ...)
              do.call(function(...) { # using do.call for passing  plot arguments from ellipsis
                plot(dd[,i], type="n", main=plot.main[i], minor.ticks=F, las=2, axes=FALSE, ...)
              }, args)
              axis(2, las=2)
              axis(1, at=.index(dd[,i])[axTicksByTime(dd)], labels=names(axTicksByTime(dd)), las=2)
              arr.x <- c(index(dd[from[i],i]), index(dd[to[i],i]))
              arr.y <- c(0, mdd[i])
              arrows(x0=arr.x[1], y0=arr.y[1], x1=arr.x[2], y1=arr.y[2], col="red", lwd=2, length=0.15, angle=25, code=2)
              text(paste0("MDD ",round(mdd[i]*100,0),"%"), x=arr.x[2], y=arr.y[2], col="red", pos=2, cex=0.8)
              lines(dd[,i])
              #pol.y <- rbind(-dd[,i],xts(0,last(index(dd))))
              #polygon(x=c(.index(dd),last(.index(dd))), y=pol.y, col = "bisque3", border = NA)
              #lines(-dd[,i])
              #box()
            }

            if (returnValues==TRUE)
              return(dd)
        } #method function
)


#' @export
#' @name plotWeights
#' @title Plot Strategy Weights
#' @description Plots the weights of the portfolio of an object of class \code{Strategy}.
#' @param object An object of class \code{Strategy}.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which weights shall be plotted. If \code{NULL}, the start date of the weights is used.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which weights shall be plotted. If \code{NULL}, the end date of the performances is used.
#' @param ... Currently not active.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Plot MA(200)-Strategy weights
#' plotWeights(myStrat.MA)
#'
#' ##End(Not run)
setGeneric(name = "plotWeights",
           def = function(object, from=NULL, until=NULL, ...) {
             standardGeneric("plotWeights")
           }
)


#' @rdname plotWeights
#' @aliases plotWeights,Strategy-method
setMethod(f = "plotWeights",
          signature = "Strategy",
          definition = function(object, from, until, ...) {

  par.mar <- par()$mar

  # Ensure ellipse consistency
  args <- list(...)
  plot.main <- "Portfolio Weights"
  if ("main" %in% names(args))
    plot.main <- args[["main"]]
  args.inUse <- which(names(args) %in% "main")
  if (length(args.inUse) > 0)
    args <- args[-args.inUse] # select only editable arguments for plot


  # data
  weights <- abs(getWeights(object, from=from, until=until))
  signals <- getSignals(object, from=from, until=until)
  col <- rainbow(ncol(weights))

  mar <- c(7, 4.1, 4.1, 2.1)

  # plot
  layout(matrix(c(1,2),ncol=2),widths=c(0.8,0.2))
  par(mar=c(mar[1:3],0))
  # plot weights timeline
  do.call(function(...) { # using do.call for passing  plot arguments from ellipsis
    plot(weights[,1], ylim=c(0,max(rowSums(weights))), type="n", las=2, minor.ticks=F, axes=FALSE, yaxs="i", main=plot.main)
  }, args)
  axis(1, at=.index(weights)[axTicksByTime(weights)], labels=names(axTicksByTime(weights)), las=2)
  axis(2, las=2)
  par(new=TRUE)
  # plot weights as colored bars
  barplot(weights, col = col, space = 0, ylab = "", border = NA, axes=FALSE, axisnames=FALSE, yaxs="i", ylim=c(0,max(rowSums(weights))))
  graphics::box()
  # legend
  legendtext <- colnames(weights)
  if (is.null(legendtext)) {
    for (i in 1:dim[2]) {
      legendtext[i] = paste("Asset", i, sep = " ")
    }
  }
  par(mar=c(mar[1], 0, mar[3],0))
  plot(0,0,type="n",axes=FALSE,xlab="",ylab="",main="")
  legend("center", legend = legendtext, bty = "n", cex = 0.7, fill = col)

  # reset layout
  layout(1)
  par(mar=par.mar)
})
