#' @export
#' @name getPrices
#' @title Get price data from \code{Strategy}-object
#' @description Gets the price data of an object of class \code{Strategy} that was used within strategy calculation.
#' @param object An object of class \code{Strategy}.
#' @param which Names or column-number of assets that should be included. If \code{NULL}, all prices are returned.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which prices shall be returned. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which prices shall be returned. If \code{NULL}, no restriction is made.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get price data from MA(200)-Strategy
#' getPrices(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' ##End(Not run)
setGeneric(name = "getPrices",
           def = function(object, from=NULL, until=NULL, which=NULL) {
             standardGeneric("getPrices")
           }
)


#' @rdname getPrices
#' @aliases getPrices,Strategy-method
setMethod(f = "getPrices",
          signature = "Strategy",
          definition = function(object, from, until, which) {

            # get all prices
            prices <- object@prices

            # set dates
            if (is.null(from)) {
              from <- start(prices)
            } else {
              from <- as.Date(from)
              if (from > end(prices)) {
                stop("From date is greater than assets start!")
              }
            }
            if (is.null(until)) {
              until <- end(prices)
            } else {
              until <- as.Date(until)
            }

            # validate which values
            which.out <- validWhich(which, prices)

            # restrict prices to range
            prices <- prices[paste0(from,"::",until), which.out]

            return(prices)
          }
)



#' @export
#' @name getWeights
#' @title Get weights from \code{Strategy}-object
#' @description Gets the weights data of an object of class \code{Strategy} that was used within strategy calculation.
#' @param object An object of class \code{Strategy}.
#' @param which Names or column-number of assets that should be included. If \code{NULL}, all weights are returned.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which weights shall be returned If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which weights shall be returned. If \code{NULL}, no restriction is made.
#' @param use.backtest If set to \code{TRUE}, the weights of the backtest are returned. Requires \code{\link{backtest}} to be  executed first.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get weights data from MA(200)-Strategy
#' getWeights(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' ##End(Not run)
setGeneric(name = "getWeights",
           def = function(object, from=NULL, until=NULL, which=NULL, use.backtest=FALSE) {
             standardGeneric("getWeights")
           }
)


#' @rdname getWeights
#' @aliases getWeights,Strategy-method
setMethod(f = "getWeights",
          signature = "Strategy",
          definition = function(object, from, until, which, use.backtest) {

            # get all weights
            weights <- object@weights

            # set dates
            if (is.null(from)) {
              from <- start(weights)
            } else {
              from <- as.Date(from)
            }
            if (is.null(until)) {
              until <- end(weights)
            } else {
              until <- as.Date(until)
            }

            # validate which values
            which.out <- validWhich(which, weights)

            # restrict weights to range
            weights <- weights[paste0(from,"::",until), which.out]

            # reconstruct original weights
            weights <- weights * getSignals(object, from=from, until=until, which=which.out, use.backtest=use.backtest)[index(weights)]

            return(weights)
          }
)



#' @export
#' @name getTrades
#' @title Get trades according to the signals from the \code{Strategy}-object
#' @description Gets the trades of an object of class \code{Strategy} that were performed within strategy calculation.
#' @param object An object of class \code{Strategy}.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which trades shall be returned. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which trades shall be returned. If \code{NULL}, no restriction is made.
#' @param which Names or column-number of assets that should be included. If \code{NULL}, trades for all assets are returned.
#' @param of Trades to be calculated on basis of trading \code{signals} or \code{weights} of portfolio.
#' @param use.backtest If set to \code{TRUE}, the trades of the backtest are returned. Requires \code{\link{backtest}} to be  executed first.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get price data from MA(200)-Strategy
#' getTrades(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' ##End(Not run)
setGeneric(name = "getTrades",
           def = function(object, from=NULL, until=NULL, which=NULL, of="signals", use.backtest=FALSE) {
             standardGeneric("getTrades")
           }
)


#' @rdname getTrades
#' @aliases getTrades,Strategy-method
setMethod(f = "getTrades",
          signature = "Strategy",
          definition = function(object, from, until, which, of=c("signals","weights"), use.backtest) {

            of <- match.arg(of)

            # get all signals
            signals <- getSignals(object, from=from, until=until, which=which, use.backtest=use.backtest)
            if (of == "weights") signals <- signals * getWeights(object, from=from, until=until, which=which, use.backtest=use.backtest)[index(signals)]

            # calculate trades based on differences of trading signals
            sigdiff <- abs(diff(signals, na.pad=TRUE))
            sigdiff[1,] <- 0
            longshort <- abs(diff(sign(signals), na.pad=TRUE))
            longshort[1,] <- 0
            trades <- sigdiff*0
            # 1 trades made (no long/short change)
            coredata(trades)[which(sigdiff>0,arr.ind=T)] <- 1
            # 2 trades made (long/short change)
            coredata(trades)[which(longshort==2,arr.ind=T)] <- 2

            return(trades)
          }
)



#' @export
#' @name getIndicators
#' @title Get indicators from \code{Strategy}-object
#' @description Gets the indicators data of an object of class \code{Strategy} that was used within strategy calculation.
#' @param object An object of class \code{Strategy}.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which indicators shall be returned. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which indicators shall be returned. If \code{NULL}, no restriction is made.
#' @param which Names or list-number of indicators that should be included. If \code{NULL}, all indicators are returned.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' randreturns <- xts::xts(rnorm(nrow(assets)), order.by=
#' seq(from=Sys.Date()-nrow(assets)+1, to=Sys.Date(), by="d"))
#' indicators <- list(returns=randreturns) # example: random returns
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params, indicators=indicators)
#'
#' # Get indicator data from MA(200)-Strategy
#' getIndicators(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' ##End(Not run)
setGeneric(name = "getIndicators",
           def = function(object, from=NULL, until=NULL, which=NULL) {
             standardGeneric("getIndicators")
           }
)


#' @rdname getIndicators
#' @aliases getIndicators,Strategy-method
setMethod(f = "getIndicators",
          signature = "Strategy",
          definition = function(object, from, until, which) {

            if (is.character(which) && which == "none") return(list())

            # get all indicators
            indicators <- object@indicators

            # validate which values
            which.out <- validWhich(which=which, data=indicators)

            # restrict indicators to selected
            if (!is.null(which.out)) {
              indicators <- indicators[which.out]
            } else {
              indicators <- list()
            }

            return(indicators)
          }
)




#' @export
#' @name getStratFUN
#' @title Get strategy function from \code{Strategy}-object
#' @description Gets the strategy function of an object of class \code{Strategy} that was used for strategy calculation.
#' @param object An object of class \code{Strategy}.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get strategy function from MA(200)-Strategy
#' MA.FUN <- getStratFUN(myStrat.MA)
#'
#' ##End(Not run)
setGeneric(name = "getStratFUN",
           def = function(object) {
             standardGeneric("getStratFUN")
           }
)


#' @rdname getStratFUN
#' @aliases getStratFUN,Strategy-method
setMethod(f = "getStratFUN",
          signature = "Strategy",
          definition = function(object) {
            return(object@stratFUN)
          }
)




#' @export
#' @name getStratName
#' @aliases getStratName
#' @title Get strategy function name from \code{Strategy}-object
#' @description Gets the strategy function name of an object of class \code{Strategy} that was used for strategy calculation. This function is for aesthetic purposes only and does not have any numerical relevance.
#' @param object An object of class \code{Strategy}.
#' @param include.params If set to \code{TRUE}, the parameters used for strategy evaluation are included.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get strategy function name from MA(200)-Strategy
#' getStratName(myStrat.MA) # returns "MA"
#' getStratName(myStrat.MA, include.params=TRUE) # returns "MA(200)"
#'
#' ##End(Not run)
setGeneric(name = "getStratName",
           def = function(object, include.params=FALSE) {
             standardGeneric("getStratName")
           }
)


#' @rdname getStratName
#' @aliases getStratName,Strategy-method
setMethod(f = "getStratName",
          signature = "Strategy",
          definition = function(object, include.params) {
            if (!is.logical(include.params))
              stop("Include.params argument must be logical!")
            strat <- object@strat
            params <- NULL
            if (include.params == TRUE) {
              params.list <- getParameters(object)
              # dont print period or printSteps
              params.list <- params.list[!names(params.list) %in% c("period")]
              if (length(params.list) != 0) {
                params <- paste0( "(", paste(names(params.list), Reduce(cbind, params.list), sep="=", collapse=","), ")" )
              }
            }
            name <- paste0(strat, params)
            return(name)
          }
)



#' @export
#' @name getParameters
#' @title Get strategy function parameters from \code{Strategy}-object
#' @description Gets the strategy function parameters of an object of class \code{Strategy} that were used for strategy calculation.
#' @param object An object of class \code{Strategy}.
#' @param use.backtest If set to \code{TRUE}, the calibrated parameters of the backtest are returned. Requires \code{\link{backtest}} to be  executed first.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get parameters from MA(200)-Strategy
#' getParameters(myStrat.MA)
#'
#' ##End(Not run)
setGeneric(name = "getParameters",
           def = function(object, use.backtest=FALSE) {
             standardGeneric("getParameters")
           }
)



#' @rdname getParameters
#' @aliases getParameters,Strategy-method
setMethod(f = "getParameters",
          signature = "Strategy",
          definition = function(object, use.backtest) {
            if (!is.logical(use.backtest))
              stop("Please provide boolean expression for use.backtest.")
            if (use.backtest==T) {
              stratFUN.params <- object@backtest.parameters
              if (length(stratFUN.params) ==0 )
                stop("Please execute backtest-method first!")
            } else {
              stratFUN.params <- object@strat.params
            }

            return(stratFUN.params)
          }
)



#' @export
#' @name getSignals
#' @title Get trading signals from \code{Strategy}-object
#' @description Gets the trading signals of an object of class \code{Strategy} that were output from strategy calculation.
#' @param object An object of class \code{Strategy}.
#' @param which Names or column-number of assets that should be returned. If \code{NULL}, all signals are returned.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which signals shall be returned. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which signals shall be returned. If \code{NULL}, no restriction is made.
#' @param use.backtest If set to \code{TRUE}, the signals of the backtest are returned. Requires \code{\link{backtest}} to be  executed first.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get signals from MA(200)-Strategy
#' # all signals returned
#' getSignals(myStrat.MA)
#' # backtest signals for first two assets returned
#' # getSignals(myStrat.MA, which=c(1,2), use.backtest=TRUE)
#'
#' ##End(Not run)
setGeneric(name = "getSignals",
           def = function(object, from=NULL, until=NULL, which=NULL, use.backtest=FALSE) {
             standardGeneric("getSignals")
           }
)


#' @rdname getSignals
#' @aliases getSignals,Strategy-method
setMethod(f = "getSignals",
          signature = "Strategy",
          definition = function(object, from, until, which, use.backtest) {
            if (!is.logical(use.backtest))
              stop("The backtest-argument is a boolean expression!")
            if (use.backtest==T) {
              signals <- object@backtest.signals
              if (!is.numeric(signals) || is.null(signals))
                stop("Please execute backtest-method first!")
            } else {
              signals <- object@signals
            }
            which <- validWhich(which, signals)

            # set dates
            if (is.null(from)) {
              from <- start(signals)
            } else {
              from <- as.Date(from)
              if (from > end(signals)) {
                stop("From date is greater than assets start!")
              }
            }
            if (is.null(until)) {
              until <- end(signals)
            } else {
              until <- as.Date(until)
            }

            # Date range
            signals <- na.omit(signals[paste0(from,"::",until),which])

            return(signals)
          }
)



#' @export
#' @name getFilters
#' @title Get strategy values from \code{Strategy}-object
#' @description Gets the strategy values of an object of class \code{Strategy} that were output from strategy calculation.
#' @param object An object of class \code{Strategy}.
#' @param which Which filters shall be returned. Either list number or names to be passed.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get strategy values from MA(200)-Strategy
#' getFilters(myStrat.MA) # all strategy values returned
#'
#' ##End(Not run)
setGeneric(name = "getFilters",
           def = function(object, which=NULL) {
             standardGeneric("getFilters")
           }
)


#' @rdname getFilters
#' @aliases getFilters,Strategy-method
setMethod(f = "getFilters",
          signature = "Strategy",
          definition = function(object, which) {
            filters <- object@filters

            if (is.character(which) && which=="none") return(list())

            # validate which values
            which.out <- validWhich(which=which, data=filters)

            # restrict indicators to selected
            if (!is.null(which.out)) {
              filters <- filters[which.out]
            } else {
              filters <- list()
            }


            return(filters)
          }
)



#' @export
#' @name getBacktestSetup
#' @title Get backtest parameter values from \code{Strategy}-object
#' @description Gets the backtest parameter values of an object of class \code{Strategy} that were used for backtesting the strategy. This includes the information about the parameters,
#' @param object An object of class \code{Strategy}.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get backtest setup from MA(200)-Strategy
#' getBacktestSetup(myStrat.MA)
#'
#' ##End(Not run)
setGeneric(name = "getBacktestSetup",
           def = function(object) {
             standardGeneric("getBacktestSetup")
           }
)


#' @rdname getBacktestSetup
#' @aliases getBacktestSetup,Strategy-method
setMethod(f = "getBacktestSetup",
          signature = "Strategy",
          definition = function(object) {
            return(object@backtest.setup)
          }
)



#' @export
#' @name getCosts
#' @title Get strategy function from \code{Strategy}-object
#' @description Returns the fixed and relative trading costs of an object of class \code{Strategy}..
#' @param object An object of class \code{Strategy}.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get strategy function from MA(200)-Strategy
#' MA.costs <- getCosts(myStrat.MA)
#' # return fix costs
#' MA.costs$fix
#' # return relative costs
#' MA.costs$relative
#'
#' ##End(Not run)
setGeneric(name = "getCosts",
           def = function(object) {
             standardGeneric("getCosts")
           }
)


#' @rdname getCosts
#' @aliases getCosts,Strategy-method
setMethod(f = "getCosts",
          signature = "Strategy",
          definition = function(object) {
            costs.fix <- object@costs.fix
            costs.rel <- object@costs.rel
            costs <- list(fix=costs.fix, relative=costs.rel)
            return(costs)
          }
)
