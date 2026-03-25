#' @export
#' @name performance
#' @title Get Strategy Performance
#' @description Gets the performance of an object of class \code{Strategy}.
#' @param object An object of class \code{Strategy}.
#' @param which Names or number of assets that should be included in performance. If a portfolio performance from only a subset of the assets is calculated, the weights are scaled accordingly.
#' @param of Performance to be extracted from assets separately or the portfolio performance.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which performance shall be returned If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which performance shall be returned. If \code{NULL}, no restriction is made.
#' @param type Which type of performance shall be returned. \code{performance} is the cumulative performance starting at \code{1}, \code{logReturns} to get logarithmic returns or \code{returns} for arithmetic returns.
#' @param use.backtest If \code{TRUE}, the signals from the backtesting output are considered for performance calculation. If \code{FALSE}, the signals from the initial strategy execution are used.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is obsolete if no costs are given.
#' @examples
#' ## Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get performance of MA(200)-Strategy
#' performance(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' # Get backtest performance of MA(200)-Strategy
#' # performance(myStrat.MA, from="2015-01-01", until="2015-12-31"
#' # , use.backtest=TRUE, type="logReturns")
#'
#' ## End(Not run)
setGeneric(name = "performance",
           def = function(object, of="portfolio",  type="performance", from=NULL, until=NULL, which=NULL, use.backtest=FALSE, include.costs=TRUE) {
             standardGeneric("performance")
           }
)

#' @rdname performance
setMethod(f = "performance",
          signature = "Strategy",
          definition = function(object, of=c("portfolio", "assets"), type=c("performance", "logReturns", "returns"), from, until, which, use.backtest, include.costs) {
            of <- match.arg(of)
            type <- match.arg(type)
            if (!is.logical(use.backtest))
              stop("The backtest-argument must be a boolean!")
            if (!is.logical(include.costs))
              stop("The include.costs-argument must be a boolean!")

            # GET data
            signals <- getSignals(object, from=from, until=until, which=which, use.backtest=use.backtest)
            prices <- getPrices(object, from=from, until=until, which=colnames(signals))


            # SET start date (performance=1)
            if (start(prices) < start(signals)) {
              from.start <- last(index(prices[index(prices) < start(signals)]))
            } else {
              from.start <- start(prices)
            }

            # PERIODICITY prices same as signals
            prices <- prices[index(signals)]

            # VALIDATE Date Range Input!
            # from
            if (!is.null(from)) {
              # if from is not a valid date, this will throw an error
              from <- as.Date(from)
              # do not start before price data
              if (from < start(prices))
                from <- start(prices)
            } else {
              from <- from.start
            }
            # until
            if (!is.null(until)) {
              # if until is not a valid date, this will throw an error
              until <- as.Date(until)
            } else {
              until <- end(signals)
            }
            # range validation
            if (from > until)
              stop("From date cannot be greater than until date!")

            logReturns <- .PricesToLogReturns(prices)

            # add signals first row = 0 to let performance start at 1
            # in case logReturns start equal to signals, this is redundant
            if (start(logReturns) < start(signals)) {
              signals_cash <- xts(t(rep(0, ncol(signals))), order.by=from)
              signals_cash2 <- NULL
              if (from < from.start && from.start < start(signals))
                signals_cash2 <- xts(t(rep(0, ncol(signals))), order.by=from.start)
              signals <- rbind(signals_cash, signals_cash2, signals)
              colnames(signals) <- colnames(prices)
            }

            # arith. returns
            ret <- (exp(logReturns)-1)

            # weight performances to portfolio level
            if (of == "portfolio") {
              # get weights (already with signal included)
              weights <- getWeights(object)[index(ret),colnames(signals)]
              # scale weights (in case not all assets selected)
              if (ncol(weights) != ncol(object@weights)) {
                normweights <- rowSums(abs(weights)) #init
                for (i in 1:ncol(weights)) weights[,i] <- weights[,i]/normweights
              }
              # compute cumulative performance without costs
              performance <- cumprod(1+xts(rowSums(ret * weights), order.by=index(weights)))
              if (use.backtest == TRUE) {
                colnames(performance) <- "Backtest Portfolio"
              } else {
                colnames(performance) <- "Portfolio"
              }
              # realized returns of portfolio before costs
              ret <- exp(.PricesToLogReturns(performance))-1
            } else {
              # realized returns of assets without weights before costs
              ret <- ret * signals
            }

            # Calculate strategy performance
            if (include.costs == TRUE) {
              # get costs
              costs <- getCosts(object)
              costs.fix <- costs$fix
              costs.rel <- costs$relative
              volume <- object@volume
              # get number of trades per period
              tradesof <- "signals"
              if (of=="portfolio") tradesof <- "weights"
              trades <- getTrades(object, from=from, until=until, which=which, of=tradesof, use.backtest=use.backtest)
              if (of=="portfolio") trades <- xts(rowSums(trades), order.by=index(trades))

              # consider relative costs
              ret <- ret - trades * costs.rel
              # absolute performance with relative costs
              perf1 <- cumprod(ret + 1)*volume
              # add fix costs
              costs.fix.cum <- cumsum(trades * costs.fix)
              # consider fix costs
              perf2 <- perf1 - costs.fix.cum
              # compute performance after cost
              performance <- perf2*NA # init
              for (i in 1:ncol(performance)) performance[,i] <- perf2[,i]/as.numeric(perf2[1,i])
            } else {
              # realized performance
              performance <- cumprod(1 + ret)
            }


            if (type == "logReturns") {
              performance <- .PricesToLogReturns(performance)
            } else if (type == "returns") {
              performance <- ret
            }

            return(performance)
          }
)



#' @export
#' @name sharpe
#' @title Get Sharpe Ratio of Performance
#' @description Get the sharpe ratio of the performance of an object of class \code{Strategy}.
#' @usage sharpe(object, rf=0, of="portfolio"
#'      , from=NULL, until=NULL, which=NULL
#'      , scaling.periods=NULL, include.costs=TRUE
#'      , use.backtest=FALSE)
#' @param object An object of class \code{Strategy}.
#' @param rf Risk free rate in decimal, e.g. \code{rf=0.01} equals \code{1 percent}.
#' @param which Names or number of assets that should be included in calculation.
#' @param of Sharpe ratio to be calculated for assets separately or the portfolio sharpe.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which performance shall be considered. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which performance shall be considered. If \code{NULL}, no restriction is made.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is obsolete if no costs are given.
#' @param use.backtest If \code{TRUE}, the performance of the backtesting output is considered for sharpe ratio calculation. If \code{FALSE}, the performance of the initial strategy execution are used.
#' @param scaling.periods Vector with annualization factors for sharpe ratio calculation. Default is 252, 52, 12, 4, 1 for daily, weekly, monthly, quarterly and yearly data respectively.
#' @examples
#' ## Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get sharpe of MA(200)-Strategy portfolio
#' sharpe(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' # Get backtest annualized sharpe of MA(200)-Strategy (daily data = 252 trading days)
#' # sharpe(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=TRUE, scaling.periods=252)
#'
#' ## End(Not run)
setGeneric(name = "sharpe",
           def = function(object, rf=0, of="portfolio", from=NULL, until=NULL, which=NULL, scaling.periods=NULL, include.costs=TRUE, use.backtest=FALSE) {
             standardGeneric("sharpe")
           }
)



#' @rdname sharpe
#' @aliases sharpe,Strategy-method
setMethod(f = "sharpe",
          signature = "Strategy",
          definition = function(object, rf, of=c("portfolio", "assets"), from, until, which, scaling.periods, include.costs, use.backtest) {
            of <- match.arg(of)
            if (!is.numeric(rf))
              stop("Please provide risk free return as numeric!")
            if (!is.null(scaling.periods) && !is.numeric(scaling.periods))
              stop("Please provide scaling periods as numeric!")

            logReturns <- performance(object, of=of, from=from, until=until, which=which, type="logReturns", include.costs=include.costs, use.backtest=use.backtest)
            logDiff <- logReturns - log(1+rf)
            returns <- exp(logReturns) - 1

            # get annualization factors if needed
            if (is.null(scaling.periods))
              scaling.periods <- .annFactor(logReturns)

            # correction term neglected / + 1/2* vapply(d, sd, 0)^2
            sharpe <- ( exp( vapply(logDiff, mean, 0)*scaling.periods ) - 1 )/ ( vapply(returns, sd, 0) * sqrt(scaling.periods) )
            return(sharpe)
          }
)




#' @export
#' @name loss
#' @title Get the losses of assets or portfolio over time.
#' @description Losses over time of an assets or portfolio of an object of class \code{Strategy}.
#' @usage loss(object, V=100, method="full", of="portfolio"
#'        , from=NULL, until=NULL, which=NULL
#'        , include.weights=TRUE, include.costs=TRUE
#'        , use.backtest=FALSE)
#' @param object An object of class \code{Strategy}.
#' @param V Volume that is invested. The linear factor for the VaR calculation. Either a single value for portfolio or a vector for each asset.
#' @param method Method of loss calculation. Use \code{linear} for approximation with log returns or \code{full} for calculation with arithmetic returns.
#' @param which Names or number of assets that should be included in calculation.
#' @param of Losses to be calculated for assets separately or the portfolio.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which losses shall be considered. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which losses shall be considered. If \code{NULL}, no restriction is made.
#' @param include.weights Only relevant if \code{of="assets"}: If \code{FALSE}, weights are all set to \code{1}. This might be necessary if only single stock performance without weighting shall be considered.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is obsolete if no costs are given.
#' @param use.backtest If \code{TRUE}, the performance of the backtesting output is considered for loss calculation. If \code{FALSE}, the performance of the initial strategy execution are used.
#' @examples
#' ## Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get VaR of MA(200)-Strategy portfolio
#' myStrat.MA.losses <- loss(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' ## End(Not run)
setGeneric(name = "loss",
           def = function(object, V=100, method="full", of="portfolio", from=NULL, until=NULL, which=NULL, include.weights=TRUE, include.costs=TRUE, use.backtest=FALSE) {
             standardGeneric("loss")
           }
)


#' @rdname loss
#' @aliases loss,Strategy-method
setMethod(f = "loss",
          signature = "Strategy",
          definition = function(object, V, method=c("full", "linear"),  of=c("portfolio", "assets"), from, until, which, include.weights, include.costs, use.backtest) {
            of <- match.arg(of)
            method <- match.arg(method)

            if (!is.logical(include.weights))
              stop("Argument indlude.weights must be a boolean!")

            # match return type
            if (method=="full") {
              return.type <- "returns"
            } else {
              return.type <- "logReturns"
            }

            # get returns
            returns <- performance(object, of=of, from=from, until=until, which=which, type=return.type, include.costs=include.costs, use.backtest=use.backtest)
            if (!is.numeric(V) || length(V) != 1)
              stop("Please provide Volume of portfolio V as single numeric value.")


            # calculate losses with returns
            if (of=="assets") {
              if (include.weights==FALSE) {
              weights <- 1
              } else  {
                weights <- getWeights(object)[index(returns),colnames(returns)]
              }
              L <- - V * (returns * weights)
            } else {
              L <- - V * returns
            }

            return(L)
          }
)



#' @export
#' @name VaR
#' @title Value at Risk
#' @description Value at Risk of the assets or portfolio of an object of class \code{Strategy}.
#' @usage VaR(object, alpha=0.05, V=1, type="normal.distribution"
#'        , method="full", of="portfolio"
#'        , from=NULL, until=NULL, which=NULL
#'        , scaling.periods=NULL, include.weights=TRUE
#'        , include.costs=TRUE, use.backtest=FALSE)
#' @param object An object of class \code{Strategy}.
#' @param alpha The significance level \eqn{\alpha} that is used for propability of cumulative loss at level \eqn{1-\alpha}.
#' @param V Volume that is invested. The linear factor for the VaR calculation. Either a single value for portfolio or a vector for each asset.
#' @param type Type of VaR calculation. Use \code{normal.distribution} for the normal distribution, \code{historical} for the empirical distribution. Default value is \code{historical}.
#' @param method Method of loss calculation. Use \code{linear} for approximation with log returns or \code{full} for calculation with arithmetic returns. Default value is \code{full}.
#' @param which Names or number of assets that should be included in calculation.
#' @param of VaR to be calculated for assets separately or the portfolio.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which losses shall be considered. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which losses shall be considered. If \code{NULL}, no restriction is made.
#' @param scaling.periods Vector with annualization factors for calculation. Default is 252, 52, 12, 4, 1 for daily, weekly, monthly, quarterly and yearly data respectively.
#' @param include.weights Only relevant if \code{of="assets"}: If \code{FALSE}, weights are all set to \code{1}. This might be necessary if only single stock performance without weighting shall be considered.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is obsolete if no costs are given.
#' @param use.backtest If \code{TRUE}, the performance of the backtesting output is considered for VaR calculation. If \code{FALSE}, the performance of the initial strategy execution are used.
#' @examples
#' ## Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get VaR of MA(200)-Strategy portfolio
#' VaR(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' # Get backtest VaR of MA(200)-Strategy
#' # VaR(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=TRUE)
#'
#' ## End(Not run)
setGeneric(name = "VaR",
           def = function(object, alpha=0.05, V=1, type="normal.distribution", method="full", of="portfolio"
                          , from=NULL, until=NULL, which=NULL
                          , scaling.periods=NULL, include.weights=TRUE, include.costs=TRUE, use.backtest=FALSE) {
             standardGeneric("VaR")
           }
)


#' @rdname VaR
#' @aliases VaR,Strategy-method
setMethod(f = "VaR",
          signature = "Strategy",
          definition = function(object, alpha, V, type=c("normal.distribution", "historical")
                                , method=c("full", "linear"),  of=c("portfolio", "assets")
                                , from, until, which, scaling.periods, include.weights, include.costs, use.backtest) {
            of <- match.arg(of)
            method <- match.arg(method)
            type <- match.arg(type)

            if (!is.numeric(alpha) || alpha > 1 || alpha < 0)
              stop("Please provide alpha as numerical between 0 and 1!")
            if (!is.null(scaling.periods) && !is.numeric(scaling.periods))
              stop("Please provide scaling periods as numeric!")

            # get loss time series
            L <- loss(object, V=V, method=method, of=of, from=from, until=until, which=which, include.weights=include.weights, include.costs=include.costs, use.backtest=use.backtest)

            # annualization factors if needed
            if (is.null(scaling.periods))
              scaling.periods <- .annFactor(L)

            # validations
            if (length(scaling.periods) == 1) {
              scaling.periods <- rep(scaling.periods, ncol(L))
            } else if (length(scaling.periods) != ncol(L)) stop("Please provide scaling periods as numeric vector with length 1 or the same number of portfolio assets.")

            if (type=="historical") {
              # deterministic VaR calculation
              VaR <- matrix(apply(coredata(L), FUN=function(x) quantile(x, probs=(1-alpha), names=F), MARGIN=2), ncol=ncol(L))
            } else if (type=="normal.distribution") {
              # analytical VaR calculation with estimated params and normal distribution
              VaR <- matrix(apply(coredata(L), FUN=function(x) qnorm(1-alpha, mean=mean(x), sd=sd(x)), MARGIN=2), ncol=ncol(L))
            }
            # scale
            VaR <- VaR %*% diag(sqrt(scaling.periods), nrow=ncol(L))
            dimnames(VaR) <- list(paste0("alpha=",alpha * 100, "%"), colnames(L))

            attr(VaR, "Portfolio Volume") <- V
            return(VaR)
          }
)



#' @export
#' @name ES
#' @title Expected Shortfall
#' @description Expected Shortfall of the assets or portfolio of an object of class \code{Strategy}.
#' @usage ES(object, alpha=0.05, V=1
#'        , type="normal.distribution", method="full"
#'        , of="portfolio", from=NULL, until=NULL, which=NULL
#'        , scaling.periods=NULL, include.weights=TRUE
#'        , include.costs=TRUE, use.backtest=FALSE)
#' @param object An object of class \code{Strategy}.
#' @param alpha The significance level \eqn{\alpha} that is used for probability of cumulative loss at level \eqn{1-\alpha}.
#' @param V Volume that is invested. The linear factor for the ES calculation. Either a single value for portfolio or a vector for each asset.
#' @param type Type of ES calculation. Use \code{normal.distribution} for the normal distribution, \code{historical} for the empirical distribution.
#' @param method Method of loss calculation. Use \code{linear} for approximation with log returns or \code{full} for calculation with arithmetic returns.
#' @param of ES to be calculated for assets separately or the portfolio.
#' @param which Names or number of assets that should be included in calculation.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which losses shall be considered. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which losses shall be considered. If \code{NULL}, no restriction is made.
#' @param scaling.periods Vector with annualization factors for calculation. Default is 252, 52, 12, 4, 1 for daily, weekly, monthly, quarterly and yearly data respectively.
#' @param include.weights Only relevant if \code{of="assets"}: If \code{FALSE}, weights are all set to \code{1}. This might be necessary if only single stock performance without weighting shall be considered.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for ES calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is obsolete if no costs are given.
#' @param use.backtest If \code{TRUE}, the performance of the backtesting output is considered for VaR calculation. If \code{FALSE}, the performance of the initial strategy execution are used.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get ES of MA(200)-Strategy portfolio
#' ES(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' # Get backtest ES of MA(200)-Strategy (backtest would need to be executed first!)
#' # ES(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=TRUE)
#'
#' ##End(Not run)
setGeneric(name = "ES",
           def = function(object, alpha=0.05, V=1
                          , type="normal.distribution", method="full", of="portfolio"
                          , from=NULL, until=NULL, which=NULL
                          , scaling.periods=NULL, include.weights=TRUE, include.costs=TRUE, use.backtest=FALSE) {
             standardGeneric("ES")
           }
)


#' @rdname ES
#' @aliases ES,Strategy-method
setMethod(f = "ES",
          signature = "Strategy",
          definition = function(object, alpha, V, type=c("normal.distribution", "historical")
                                , method=c("full", "linear"),  of=c("portfolio", "assets")
                                , from, until, which, scaling.periods, include.weights, include.costs, use.backtest) {
            of <- match.arg(of)
            method <- match.arg(method)
            type <- match.arg(type)

            if (!is.numeric(alpha) || !all(alpha < 1) || !all(alpha > 0))
              stop("Please provide alpha as numeric (vector) between 0 and 1!")
            if (!is.null(scaling.periods) && !is.numeric(scaling.periods))
              stop("Please provide scaling periods as numeric!")

            L <- loss(object, V=V, method=method, of=of, from=from, until=until, which=which, include.weights=include.weights, include.costs=include.costs, use.backtest=use.backtest)

            # annualization factors if needed
            if (is.null(scaling.periods))
              scaling.periods <- .annFactor(L)

            # validations
            if (length(scaling.periods) == 1) scaling.periods <- rep(scaling.periods, ncol(L))
            if (length(scaling.periods) != ncol(L)) stop("Please provide scaling periods as numeric vector with length 1 or number of portfolio assets.")

            if (type=="historical") {
              # deterministic expected shortfall calculation, columnwise for each alpha
              ES <- matrix(apply(coredata(L), MARGIN=2, FUN=function(x) {
                VaR <- quantile(x, probs=(1-alpha), names=F)
                sapply(VaR, FUN=function(VaR.i) mean(x[x>=VaR.i]))
              }), ncol=ncol(L))
            } else if (type=="normal.distribution") {
              # analytical VaR calculation with estimated params and normal distribution
              # xdnorm <- function(x, mean, sd) x * dnorm(x, mean=mean, sd=sd)
              ES <- matrix(apply(L, MARGIN=2, FUN=function(x) {
                mu <- mean(x)
                sigma <- sd(x)
                # for each alpha compute ES
                sapply(alpha, FUN=function(a) {
                  mu + sigma * dnorm(qnorm(1-a))/ a
                  # alternative computation method:
                  #VaR <- qnorm(1-a, mean=mu, sd=sigma)
                  #integrate(xdnorm, VaR, Inf, mean=mu, sd=sigma, subdivisions = 1E6, stop.on.error = FALSE)$value / a
                })
              }), ncol=ncol(L))
            }
            # scale ES
            ES <- ES %*% diag(sqrt(scaling.periods), nrow=ncol(L))
            dimnames(ES) <- list(paste0("alpha=",alpha * 100, "%"), colnames(L))

            attr(ES, "Portfolio Volume") <- V
            return(ES)
          }
)


#' @export
#' @name MDD
#' @title Strategy Performance Maximum Drawdown
#' @description Gets the maximum drawdown of the performance of an object of class \code{Strategy}.
#' @usage MDD(object, of="portfolio"
#'        , from=NULL, until=NULL, which=NULL
#'        , type="relative", include.costs=TRUE
#'        , use.backtest=FALSE)
#' @param object An object of class \code{Strategy}.
#' @param which Names or number of assets that should be included in calculation.
#' @param of Maximum Drawdown to be calculated for assets separately or the portfolio.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which performance shall be considered. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which performance shall be considered. If \code{NULL}, no restriction is made.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is obsolete if no costs are given.
#' @param use.backtest If set to \code{TRUE}, the signals from the backtesting output are considered for maximum drawdown calculation. If \code{FALSE}, the signals from the initial strategy execution are used.
#' @param type If the \code{absolute} or \code{relative} drawdown of the performance shall be returned.
#' @examples
#' ## Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get MDD of MA(200)-Strategy portfolio
#' MDD(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' # Get MDD of MA(200)-Strategy (daily data = 252 trading days)
#' # MDD(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=TRUE)
#'
#' ## End(Not run)
setGeneric(name = "MDD",
           def = function(object, of="portfolio", from=NULL, until=NULL, which=NULL, type="relative", include.costs=TRUE, use.backtest=FALSE) {
             standardGeneric("MDD")
           }
)


#' @rdname MDD
#' @aliases MDD,Strategy-method
setMethod(f = "MDD",
          signature = "Strategy",
          definition = function(object, of=c("portfolio", "assets"), from, until, which, type=c("absolute", "relative"), include.costs, use.backtest) {
            of <- match.arg(of)
            type <- match.arg(type)
            vals <- performance(object, of=of, from=from, until=until, which=which, include.costs=include.costs, use.backtest=use.backtest)

            maxdrawdown <- function(x)
            {
              x.dates <- index(x)
              x <- as.matrix(x)
              # get Drawdowns
              dd <- cummax(x) - x
              # get Max Drawdown Position
              to.pos <- last(which(dd==max(dd))) # to which position
              # get absolute MDD
              mdd <- dd[to.pos]
              # get min value (dropdown end)
              to <- x[to.pos]
              # from which position drops
              from.pos <- last(which(x==cummax(x)[to.pos]))
              # get max value (mdd drop start)
              from <- x[from.pos]
              if (type=="relative")
                mdd <- mdd/from # in percent
              # set dates as names
              names(to) <- x.dates[to.pos]
              names(from) <- x.dates[from.pos]
              return(list(mdd = mdd, from = from, to = to))
            }
            mdd <- lapply(vals, maxdrawdown)
            return(mdd)
          }
)




#' @export
#' @name hitratio
#' @title Strategy Hit Ratio
#' @description Gets the hitratio of the signals of an object of class \code{Strategy}.
#' @usage hitratio(object, of="portfolio"
#'        , from=NULL, until=NULL, which=NULL
#'        , type="per.signal", include.costs=TRUE
#'        , use.backtest=FALSE)
#' @param object An object of class \code{Strategy}.
#' @param which Names or number of assets that should be included in calculation.
#' @param of Hit Ratio to be calculated for assets separately or the portfolio (weighted hit ratios according to average asset weights).
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which returns shall be considered. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which returns shall be considered. If \code{NULL}, no restriction is made.
#' @param type If the hitratio shall be calculated per trade with \code{per.trade} or per signal \code{per.signal}.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is obsolete if no costs are given.
#' @param use.backtest If set to \code{TRUE}, the signals from the backtesting output are considered for maximum drawdown calculation. If \code{FALSE}, the signals from the initial strategy execution are used.
#' @examples
#' ## Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get hit ratio of MA(200)-Strategy portfolio
#' hitratio(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' # Get hit ratio of MA(200)-Strategy (daily data = 252 trading days)
#' # hitratio(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=TRUE)
#'
#' ## End(Not run)
setGeneric(name = "hitratio",
           def = function(object, of="portfolio", from=NULL, until=NULL, which=NULL, type="per.signal", include.costs=TRUE, use.backtest=FALSE) {
             standardGeneric("hitratio")
           }
)


#' @rdname hitratio
#' @aliases hitratio,Strategy-method
setMethod(f = "hitratio",
          signature = "Strategy",
          definition = function(object, of=c("portfolio", "assets"), from, until, which, type=c("per.signal", "per.trade"), include.costs, use.backtest) {
            of <- match.arg(of)
            type <- match.arg(type)

            # performance to include costs, treated as price series
            prices <- performance(object, of="assets", from=from, until=until, which=which, include.costs=include.costs, use.backtest=use.backtest)
            #prices <- getPrices(object, from=from, until=until, which=which)
            signals <- getSignals(object, which=which, from=from, until=until, use.backtest=use.backtest)

            # init variable hits
            hits <- list()

            if (type == "per.trade") {
              trades <- lag(abs(diff(signals, na.pad=TRUE))>0, k=-1) #last date of period
              trades[nrow(trades),] <- T #last period mark

              hitFUN <- function(price, signal, trade){
                price <- price[index(trade[trade==TRUE])]
                ret <- .PricesToLogReturns(price)
                # exclude cash signals
                signal <- signal[index(ret)]
                signal <- signal[signal!=0]
                if (is.null(nrow(signal))) return(0)
                hits <- as.numeric(sign(ret[index(signal)])) == as.numeric(sign(signal))
                return(hits)
              }
              for (i in 1:ncol(prices)) { #i<-1
                hits[[i]] <- hitFUN(prices[,i], signals[,i], trades[,i])
              }

            } else if (type == "per.signal") {
              logReturns <- .PricesToLogReturns(prices)[index(signals)]
              signals <- signals[index(logReturns)]
              for (i in 1:ncol(prices)) { #i<-1
                # exclude cash signals
                idx <- index(signals[,i]!=0)
                hits[[i]] <- sign(logReturns[idx,i]) == sign(signals[signals[idx,i]!=0,i])
              }
            }

            if (of=="portfolio") {
              hitlen <- Reduce(sum, lapply(hits, function(hit) length(hit)))
              nhits <- Reduce(sum, lapply(hits, function(hit) sum(hit)))
              hitratios <- nhits/hitlen
              names(hitratios) <- "Portfolio"
            } else {
              hitlen <- Reduce(c, lapply(hits, function(hit) length(hit)))
              nhits <- Reduce(c, lapply(hits, function(hit) sum(hit)))
              hitratios <- nhits/hitlen
              names(hitratios) <- colnames(prices)
            }

            return(hitratios)
          }
)



#' @export
#' @name performanceIndicators
#' @title Strategy Performance Indicators
#' @description Get a list of the performance indicators of an object of class \code{Strategy}.
#' @usage performanceIndicators(object, of="portfolio"
#'        , from=NULL, until=NULL, which=NULL, alpha=0.05
#'        , scaling.periods=NULL, include.weights=TRUE
#'        , include.costs=TRUE, use.backtest=FALSE)
#' @param object An object of class \code{Strategy}.
#' @param which Names or number of assets that should be included in calculation.
#' @param alpha The significance level \eqn{\alpha} that is used for propability of cumulative loss at level \eqn{1-\alpha}.
#' @param of Indicators to be calculated for assets separately or the portfolio.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which performance shall be considered. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which performance shall be considered. If \code{NULL}, no restriction is made.
#' @param scaling.periods Vector with annualization factors for calculation. Default is 252, 52, 12, 4, 1 for daily, weekly, monthly, quarterly and yearly data respectively.
#' @param include.weights Only relevant if \code{of="assets"}: If \code{FALSE}, weights are all set to \code{1}. This might be necessary if only single stock performance without weighting shall be considered.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is obsolete if no costs are given.
#' @param use.backtest If set to \code{TRUE}, the signals from the backtesting output are considered for maximum drawdown calculation. If \code{FALSE}, the signals from the initial strategy execution are used.
#' @examples
#' ## Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Get performance indicators of MA(200)-Strategy assets
#' performanceIndicators(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' ## End(Not run)
setGeneric(name = "performanceIndicators",
           def = function(object, of="portfolio", from=NULL, until=NULL, which=NULL, alpha=0.05
                          , scaling.periods=NULL, include.weights=TRUE, include.costs=TRUE, use.backtest=FALSE) {
             standardGeneric("performanceIndicators")
           }
)


#' @rdname performanceIndicators
#' @aliases performanceIndicators,Strategy-method
setMethod(f = "performanceIndicators",
          signature = "Strategy",
          definition = function(object, of=c("portfolio", "assets"), from, until, which, alpha, scaling.periods, include.weights, include.costs, use.backtest) {

            of <- match.arg(of)

            # annualized sharpe
            sharpe <- sharpe(object, of=of, from=from, until=until, which=which, scaling.periods=scaling.periods, include.costs=include.costs, use.backtest=use.backtest)
            # QRM
            mdd <- sapply(MDD(object, of=of, type="relative", from=from, until=until, which=which, include.costs=include.costs, use.backtest=use.backtest), function(x) x[["mdd"]])
            VaR <- VaR(object, of=of, which=which, scaling.periods=scaling.periods, include.weights=include.weights, include.costs=include.costs, use.backtest=use.backtest)
            ES <- ES(object, of=of, which=which, scaling.periods=scaling.periods, include.weights=include.weights, include.costs=include.costs, use.backtest=use.backtest)
            hitratio <- hitratio(object, from=from, until=until, of=of, which=which, include.costs=include.costs, use.backtest=use.backtest)

            # TRADES
            tradesof <- "signals"
            if (of=="portfolio") {
              tradesof <- "weights"
            }
            trades <- getTrades(object, from=from, until=until, which=which, of=tradesof, use.backtest=use.backtest)

            if (of == "portfolio") {
              tradesSum <- sum(trades)
              names(tradesSum) <- "Portfolio Trades"
            } else {
              tradesSum <- apply(trades, 2, sum)
            }

            # returns
            logReturns <- performance(object, of=of, from=from, until=until, which=which, use.backtest=use.backtest, include.costs=include.costs, type="logReturns")
            # annualization factors if needed
            if (is.null(scaling.periods))
              scaling.periods <- .annFactor(logReturns)
            # annualized returns
            meanReturns <- exp(apply(logReturns, 2, mean) * scaling.periods)  - 1
            returns <- exp(logReturns) - 1
            # annualized vola
            vola <- apply(returns, 2, sd) * sqrt(scaling.periods)

            # INITIALIE performance matrice
            performance_mat <- matrix(ncol=ncol(logReturns), nrow=8)
            rownames(performance_mat) <- c("Ann. Returns", "Ann. Vola", "Ann. Sharpe", "MDD", "Ann. VaR", "Ann. ES", "Hit Ratio", "Trades")
            colnames(performance_mat) <- colnames(logReturns)

            performance_mat[] <- rbind(meanReturns, vola, sharpe, mdd, VaR, ES, hitratio, tradesSum)
            attr(performance_mat, "Ann. Scaling") <- scaling.periods

            return(performance_mat)
          }
)



#' @export
#' @name compare
#' @title Compare performance of \code{Strategy}-objects.
#' @description Compare the portfolio performance indicators of an arbitrary number of objects of class \code{Strategy}.
#' @usage compare(..., from=NULL, until=NULL, which=NULL
#'        , scaling.periods=NULL, include.costs=TRUE
#'        , use.backtest=FALSE, include.params=FALSE)
#' @param ... Objects of class \code{Strategy}.
#' @param which Names or number of assets that should be included in calculation.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which performance shall be considered. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which performance shall be considered. If \code{NULL}, no restriction is made.
#' @param use.backtest If \code{TRUE}, the performance of the backtesting output is considered for performance indicator calculation. If \code{FALSE}, the performance of the initial strategy execution are used.
#' @param scaling.periods Vector with annualization factors for calculation. Default is 252, 52, 12, 4, 1 for daily, weekly, monthly, quarterly and yearly data respectively.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is obsolete if no costs are given.
#' @param include.params If \code{TRUE} the parameters of the strategies are included in their names. E.g. \code{MA(k=200)} instead of \code{MA} as strategy name for moving average.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # EWMA(0.05)-Strategy
#' params <- list(lambda=0.05)
#' myStrat.EWMA <- Strategy(assets=assets, strat="EWMA", strat.params=params)
#'
#' # Compare annualized performance of MA(200) and EWMA(0.05)
#' # compare(myStrat.MA, myStrat.EWMA, use.backtest=TRUE, scaling.periods=252)
#'
#' ##End(Not run)
setGeneric(name = "compare",
           signature = "...",
           def = function(..., from=NULL, until=NULL, which=NULL, scaling.periods=NULL, include.costs=TRUE, use.backtest=FALSE, include.params=FALSE) {
             standardGeneric("compare")
           }
)


#' @rdname compare
#' @aliases compare,Strategy-method
setMethod(f = "compare",
          signature = signature("..."="Strategy"),
          definition = function(..., from, until, which, scaling.periods, include.costs, use.backtest, include.params) {

            args <- list(...)

            # CHECK scaling.periods
            if (is.null(scaling.periods))
              scaling.periods <- Reduce(c, lapply(args, function(obj) min(.annFactor(getPrices(obj)))))
            if (!is.numeric(scaling.periods) || is.null(scaling.periods))
              stop("Please provide scaling.periods as numeric!")
            if (length(scaling.periods) > 1) {
              if (length(scaling.periods) != length(args))
                stop("Please provide as many scaling.periods-values as Strategy-objects!")
            } else {
              scaling.periods <- rep(scaling.periods, length(args))
            }


            # VALIDATE Strategy Objects
            args.strategies <- rep(FALSE, length(args))
            for (i in 1:length(args.strategies)) {
              if ("Strategy" %in% class(args[[i]]))
                args.strategies[i] <- TRUE
            }

            if (length(args.strategies[args.strategies==FALSE]) > 0) {
              if (length(args.strategies[args.strategies==TRUE]) == 0) {
                stop("Please provide only objects of class Strategy to compare!")
              } else {
                warning(paste0("Please provide only objects of class Strategy. The following will be ignored: ", paste0(names(args[which(args.strategies==FALSE)]), collapse=",")))
              }
            }
            # select valid Strategy objects
            strategies.list <- args[which(args.strategies==TRUE)]
            len <- length(strategies.list)
            scaling.periods <- scaling.periods[which(args.strategies==TRUE)]

            # get strategy names
            names <- sapply(strategies.list, function(strat) getStratName(strat, include.params=include.params))

            # INIT performance matrice
            performance_mat <- matrix(ncol=len, nrow=8)
            rownames(performance_mat) <- c("Ann. Returns", "Ann. Vola", "Ann. Sharpe", "MDD", "Ann. VaR", "Ann. ES", "Hit Ratio", "Trades")
            colnames(performance_mat) <- names


            # fill performance matrix
            for (i in 1:len) { #i<-1
              strategy <- strategies.list[[i]]
              performance_mat[,i] <- performanceIndicators(strategy, of="portfolio", from=from, until=until, which=which, include.costs=include.costs, use.backtest=use.backtest, scaling.periods=scaling.periods[i])
            }
            # append attribute: scaling periods used for annualization
            attr(performance_mat, "Ann. Scaling") <- scaling.periods

            return(performance_mat)
          }
)
