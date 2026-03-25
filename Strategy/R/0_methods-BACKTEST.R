#' @export
#' @name backtest
#' @title Backtest Strategy
#' @description Walk forward analysis backtest with the specified parameters on an object of class \code{Strategy}. The backtest calibrates the parameters according to the specification given by the user (in-sample) and returns the trading signals for the following period (out-of-sample). This is iteratively repeated on a shifting time window. Computer performance is critical with this function.
#' @param object An object of class \code{Strategy}.
#' @param horizon The out-of-sample period length.
#' @param data.width The in-sample period length used for calibration.
#' @param keep.history If set to \code{TRUE}, the starting point of in-sample data is kept fixed, so the period extends each iteration.
#' @param optim.param A character vector providing the names of the parameters to be calibrated. Parameters that are not provided will be kept fix.
#' @param optim.param.min A numeric vector providing the minimum values of the parameters that are calibrated.
#' @param optim.param.max A numeric vector providing the maximum values of the parameters that are calibrated.
#' @param optim.param.scale A numeric vector providing the scaling of the parameters that are calibrated. It is advisable to set scaling of the parameters to the smallest unit that makes sense.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which assets shall be considered. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which assets shall be considered. If \code{NULL}, no restriction is made.
#' @param which Names or number of assets that should be included in backtest
#' @param rf Risk free rate in decimal, e.g. \code{rf=0.01} equals \code{1 percent}.
#' @param printSteps This is a feature used mainly for debugging the constructor function in order to localize where unspecified errors occur. If set to true, the different steps run within the constructor is printed to the console.
#' @examples
#' ##Not run:
#' # MA(200)-Strategy
#' params <- list(k=20)
#' # reduce dataset due to computation time
#' assets_r <- assets[tail(zoo::index(assets),100)]
#' myStrat.MA <- Strategy(assets=assets_r, strat="MA", strat.params=params)
#'
#' # Perform backtest on MA(20)-Strategy with
#' # out-of-sample periods of 2 months
#' # and in-sample-calibration of 2 months
#' # This example requires a lot of computation time,
#' # so this is only performed for 1 asset and high scaling.
#' backtest(myStrat.MA, horizon="2m", data.width="2m"
#'          , optim.param="k", optim.param.min=5, optim.param.max=10
#'          , optim.param.scale=5, printSteps = TRUE, which=1)
#' ##End(Not run)
setGeneric(name = "backtest",
           def = function(object, horizon="6m", data.width="24m", keep.history=F,
                          optim.param=NULL, #parameter names (Strings) as vector
                          optim.param.min=1, optim.param.max=10, #numeric vector of mins and max to be tested (not inf)
                          optim.param.scale=.1,
                          from=NULL, until=NULL, which=NULL, rf=0, printSteps=F) {
             standardGeneric("backtest")
           }
)


#' @rdname backtest
#' @aliases backtest,Strategy-method
setMethod(f = "backtest",
          signature = "Strategy",
          definition = function(object, horizon, data.width, keep.history,
                                optim.param, #parameter names (Strings) as vector
                                optim.param.min, optim.param.max, #numeric vector of mins and max to be tested (not inf)
                                optim.param.scale,
                                from, until, which, rf, printSteps) {


            # VALIDATIONS
            if (!is.null(optim.param)) {
              if (!is.character(optim.param)) stop("Please provide parameter name to be optimized as character!")
              if (!(is.null(optim.param.min) || is.null(optim.param.max)
                    || is.numeric(optim.param.min) || is.numeric(optim.param.max))
                  || length(optim.param.min) != length(optim.param) || length(optim.param.max) != length(optim.param)
                  || any(optim.param.min>optim.param.max)
              ) stop("Please provide optimization parameter minimum and maximum as increasing or equal numerics with correct scaling for each parameter!")
              if (!all((optim.param.max-optim.param.min)/optim.param.scale >= 1))
                warning(paste0("The following parameters will be fixed: ", paste(optim.param[which((optim.param.max-optim.param.min)/optim.param.scale < 1)], collapse=", ")))

            } else { # if no params given, take objet's existing params
              warning("No parameters to optimize given in optim.params, values of strategy object taken. Performance will be the same as in strategy execution without backtesting.")
              parameters <- getParameters(object)
              parameters.numeric <- list()
              m <- 1
              for (i in 1:length(parameters)) {
                if (is.numeric(parameters[[i]])) {
                  parameters.numeric[m] <- parameters[i]
                  names(parameters.numeric)[m] <- names(parameters[i])
                  m <- m+1
                }
              }
              if (length(parameters.numeric) > 0) {
                optim.param <- names(parameters.numeric)
                optim.param.min <- optim.param.max <- (1:length(optim.param)) * NA
                optim.param.scale <- rep(1, length(optim.param))
                for (i in 1:length(optim.param)) {
                  optim.param.min[i] <- optim.param.max[i] <- parameters.numeric[[i]]
                }
              } else {
                optim.param <- "pseudo"
                optim.param.min <- optim.param.max <- optim.param.scale <- 1
              }
            } # if no params
            if (!is.numeric(rf) || length(rf) != 1)
              stop("Please provide risk free return as numeric (single) value!")

            # SET VARIABLES
            prices <- getPrices(object, from=from, until=until, which=which)
            weights <- getWeights(object, from=from, until=until, which=which, use.backtest=FALSE)
            strategy <- getStratName(object)
            indicators <- getIndicators(object)
            object.name <- deparse(substitute(object))
            object.env <- parent.frame()

            # save setup
            setup <- matrix(ncol=length(optim.param), nrow=3)
            colnames(setup) <- optim.param
            rownames(setup) <- c("Minimum", "Maximum", "Scaling")
            setup[1,] <- optim.param.min
            setup[2,] <- optim.param.max
            setup[3,] <- optim.param.scale

            # VALIDATE stratFUN
            stratFUN <- getStratFUN(object)

            # Start and End dates of each estimation period
            rolling.dates <- xts.rollingWindows(prices, data.width, horizon, keep.history)
            rolling.from <- rolling.dates$start
            rolling.to <- rolling.dates$end

            # Number of rebalancing periods
            nbPeriods <- length(rolling.to)
            # Predefine calibration parameter xts (for output)
            param.calib_mat <- matrix(nrow=nbPeriods, ncol=length(optim.param))
            rownames(param.calib_mat) <- rolling.to
            colnames(param.calib_mat) <- optim.param
            param.calib <- as.list(rep(NA, ncol(prices)))
            names(param.calib) <- colnames(prices)
            for (i in 1:length(param.calib))
              param.calib[[i]] <- as.xts(param.calib_mat)

            # Predefine signals as list (for output)
            signals <- list()

            # EXPAND Grid
            optim.list <- list()
            for (i in 1:length(optim.param)) { #i<-1
              optim.list[[i]] <- seq(optim.param.min[i], optim.param.max[i], optim.param.scale[i])
            }
            names(optim.list) <- optim.param
            params.grid <- expand.grid(optim.list)


            # SHARPE Ratio Function
            # SET sharp ratio under Strategy as optimization criteria
            # with x: parameter combination for stratFUN
            sharpeFUN <- function(x, rf, calib.data) {

              params <- as.list(x)

              if (printSteps==T)
                print(paste("Params: ", paste(names(params), params, sep="=")))

              stratvals <- stratFUN(prices=calib.data, weights=weights[index(calib.data)], indicators=indicators, parameters=params)
              signals <- stratvals$signals
              logReturns <- stratvals$logReturns[paste0(index(signals))]
              returns <- exp(logReturns * signals) - 1
              vola <- vapply(returns-rf, sd, 0)
              vola[vola==0] <- 1 #if vola=0, it is risk free portfolio and Rp=rf
              sharpe.ratios <- colMeans(returns-rf)/vola

              if (printSteps==T)
                print(paste("Sharpe Ratios: ", paste(paste(names(sharpe.ratios), sharpe.ratios, sep="="), sep=", ")))

              return(sharpe.ratios)
            }

            if (printSteps == T) {
              print("Period Windows:")
              print(t(t(apply(cbind(rolling.from, rolling.to), 1, paste, collapse=" until "))))
              print("Parameter Combinations:")
              print(params.grid)
            }

            # FOR EACH time period: calibrate and get out-of-sample performance

            for(tn in 1:nbPeriods) { #tn<-1#tn<-nbPeriods

              # Always print progress of period
              print(paste0("Period ", tn, " of ", nbPeriods, " started."))

              # Data used for calibration
              calibration.data <- prices[paste0(rolling.from[tn], "::", rolling.to[tn])]
              calibration.data <- calibration.data[-nrow(calibration.data),] #cut last value, is redundant with first in performance.data


              # OPTIMIZATION
              # rowwise=1 apply sharpeFUN to param combination
              # and get max sharpe for each asset
              sharpe.calib <- as.matrix(t(apply(params.grid, 1, sharpeFUN, rf=rf, calib.data=calibration.data)))
              sharpe.calib.max <- apply(sharpe.calib, 2, max, na.rm=T)
              names(sharpe.calib.max) <- colnames(prices)

              # initialize param.max matrix
              param.max <- matrix(nrow=ncol(params.grid), ncol=ncol(prices))
              rownames(param.max) <- colnames(params.grid)
              colnames(param.max) <- colnames(prices)

              # GET Params that maximized sharpe for each asset
              # sharpe.calib[is.na(sharpe.calib)] <- min(sharpe.calib, na.rm=T) #replace NA with minimums
              sharpe.calib.which.max <- sharpe.calib*NA #initialize & clear values
              for (i in 1:ncol(prices)) { #i<-1
                sharpe.calib.which.max[,i] <- sharpe.calib.max[colnames(prices[,i])] == sharpe.calib[,i]
                param.max[,i] <- as.numeric(params.grid[min(which(sharpe.calib.which.max[,i]==1)),])
              }

              # Performance DATA for out-of-sample (following period (steps))
              if (tn < nbPeriods) { #tn<-1
                # GET prices for out-of-sample performance
                performance.data <- prices[paste0(rolling.to[tn], "::", rolling.to[tn+1])]
                performance.data <- performance.data[-nrow(performance.data),]
              } else {
                # GET prices for out-of-sample performance
                performance.data <- prices[paste0(rolling.to[tn], "::")]
              }

              # RUN out-of-sample performance
              strat.signals <- list()
              for ( i in 1:ncol(performance.data)) { #i<-1
                params.perf <- as.list(param.max[,i])
                names(params.perf) <- optim.param
                if (printSteps==T) {
                  print(paste("Params for out-of-sample for ", colnames(performance.data[,i]), " ", paste(names(params.perf), params.perf, sep="=", collapse=",")))
                }
                perf.data <- rbind(calibration.data[,i], performance.data[,i])
                stratvals <- stratFUN(perf.data, weights=weights[index(perf.data)], indicators=indicators, parameters=params.perf)
                signals_o <- stratvals$signals[paste0(index(performance.data))] # is shifted, take only performance period
                #logReturns <- stratvals$logReturns[paste0(index(performance.data))] # take only performance period
                #strat.logReturns <- logReturns * signals
                strat.signals[[i]] <- signals_o
              }

              # SAVE signals
              names(strat.signals) <- colnames(performance.data)
              signals[[tn]] <- as.xts(do.call(cbind, strat.signals))


              # SAVE calibration parameters used for period tn for each asset
              for (j in 1:length(param.calib)) #j<-1
                param.calib[[j]][tn,] <- param.max[,j]


            }#foreach

            # Set Names

            # SAVE BACKTEST Results to Strategy object
            signals.xts <- Reduce(rbind, signals)
            object@backtest.signals <- signals.xts
            object@backtest.parameters <- param.calib
            object@backtest.setup <- setup
            assign(object.name, object, envir=object.env)


          } # method function
) # method
