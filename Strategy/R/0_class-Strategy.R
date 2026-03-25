## from 2015, all packages except "base" have to be listed
## http://developer.r-project.org/blosxom.cgi/R-devel/NEWS/2015/06/29#n2015-06-29


#' @import stats
#' @importFrom utils head
#' @import graphics
#' @import methods
#' @importFrom grDevices rainbow
#' @import zoo
#' @import xts


setOldClass("xts") # formally declare S3 class
setClassUnion("funNULL", members = c("function","NULL")) # function or NULL for plotFUN slot

# CLASS DEFINITION ------------------------------------------------
#' @title \code{Strategy}-Class
#' @description An S4 class to store quantitative strategies and compute various performance figures.
#' @slot prices Price data of the assets. If return data was given within the constructor, starting at 100.
#' @slot weights Time series of class \code{xts} indicating row wise weights of the assets.
#' @slot indicators List of indicators of class \code{xts}.
#' @slot strat Name of the strategy function to be called. Could be a full file path to a custom strategy.
#' @slot strat.params List of parameters as input for the strategy function. List entry names should match parameter names.
#' @slot stratFUN Contains the custom strategy function or \code{NULL}.
#' @slot plotFUN Contains the custom strategy function or \code{NULL}.
#' @slot filters List with filtered price data (e.g. MA(200)-data).
#' @slot signals Time series with trading signals of class \code{xts}.
#' @slot backtest.signals Time series with trading signals of the backtest of class \code{xts}.
#' @slot backtest.parameters List of parameters of the backtest.
#' @slot backtest.setup Matrix showing the backtest preferences.
#' @slot volume Numeric vector indicating the initial investment volume per asset.
#' @slot costs.fix Numeric vector indicating the fixed costs per trade per asset.
#' @slot costs.rel Numeric vector indicating the relative costs per trade per asset.
setClass(Class="Strategy",
         slots = list(
           # input
           prices = "xts",
           weights = "xts",
           indicators = "list",
           strat = "character",
           strat.params = "list",
           # output
           stratFUN = "function",
           plotFUN = "funNULL",
           filters = "list",
           signals = "xts",
           backtest.signals = "xts",
           backtest.parameters = "list",
           backtest.setup = "matrix",
           volume = "numeric",
           costs.fix = "numeric",
           costs.rel = "numeric"
         )
         , prototype = prototype(
           weights = xts(),
           indicators = list(),
           filters = list(),
           backtest.signals = xts("Call backtest(StratObj, ...) to perform backtest.", order.by=Sys.Date()),
           backtest.parameters = list(),
           backtest.setup = matrix(),
           volume = 100000,
           costs.fix = 0,
           costs.rel = 0
         )
)

# CONSTRUCTOR ------------------------------------------------


#' @export
#' @name Strategy
#' @aliases Strategy
#' @title Create Strategy Object
#' @description Creates an object of class \code{Strategy} with the given portfolio data and strategy-function.
#' @usage Strategy(assets, strat = "buyhold"
#'      , assetValueType = c("price", "logReturn"), weights = NULL, indicators = list()
#'      , strat.params = list(), volume = 1000000
#'      , costs.fix = 0, costs.rel = 0
#'      , printSteps = FALSE)
#' @param assets Time series of class \code{xts} of asset values in either price or log return form on
#' which the strategy function shall be applied. This is the portfolio of assets.
#' @param strat The name of the strategy that should be applied. This can be either a
#'   predefined strategy like MA or EWMA or a self-written function in
#'   which case the full path to the function file to be called must be supplied.
#' @param assetValueType Assets can be passed as prices or log returns. In order to identify the
#'   asset value types, either one of the types has to be selected.
#' @param weights The portfolio weights for the given assets as time series (dynamic) or numerical (constant) weights.
#' @param indicators A list of indicators that might be used within customized strategies. It is recommended to pass a named list.
#' @param strat.params The list of parameters and their values required by the strategy function selected with parameter strat.
#' @param volume Portfolio volume for trading. Default value is 1 Million.
#' @param costs.fix The fix trading costs per trade.
#' @param costs.rel The trading costs, relative to the volume. I.e. a value of \code{10E-4} reflects the costs of 10 basis points of the traded volume.
#' @param printSteps This is a feature used mainly for debugging the constructor function in order to localize where unspecified errors occur. If set to true, the different steps run within the constructor is printed to the console.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Own MA-strategy-function
#' # myStrat.MA <- Strategy(assets=assets, strat="C:/MA_function.R")
#'
#' ##End (Not run)
Strategy <- function(assets,
                     strat = "buyhold",
                     assetValueType = c("price", "logReturn"),
                     weights = NULL,
                     indicators = list(),
                     strat.params = list(),
                     volume = 1000000,
                     costs.fix = 0,
                     costs.rel = 0,
                     printSteps = FALSE) {

  # VALIDATIONS
  # -------------------------------

  # CHECK assets
  if (!is.xts(assets)) stop("Please provide assets in xts format!")
  if (ncol(assets) == 0) stop("Assets does not have any data column!")
  index(assets) <- as.Date(index(assets), tz="") #index as Date class
  assets <- as.xts(assets) #as.xts to prevent conversion probs
  if (anyNA(assets)) {
    assets <- na.omit(na.locf(assets))
    message("NA found in prices. Replaced with prior and cut if within first value(s). See na.locf() and na.omit() documentation.")
  }

  # CHECK assetValueType (match.arg has its own algo to check)
  assetValueType <- match.arg(assetValueType)

  if (printSteps==T) print("Assets checked.")

  # CHECK Asset Weights
  if (is.null(weights)) weights <- rep(1/ncol(assets), ncol(assets))
  if (is.xts(weights)) {
    index(weights) <- as.Date(as.character(index(weights)))
      if (ncol(weights) != ncol(assets)) stop("Asset Weights must have the same number of columns as assets!")
  } else if (is.numeric(weights)) { # constant weights
      if (length(weights) != ncol(assets)) stop("Asset Weights must have the same number of columns as assets!")
      else {
        weights.tmp <- assets
        weights.tmp[,] <- matrix(rep(weights, nrow(assets)), byrow=T, nrow=nrow(assets))
        rownames(weights.tmp) <- index(assets)
        weights <- as.xts(weights.tmp)
      }
  } else {
      stop("Please provide asset weights as numeric or xts!")
  }
  # check weights sums --> scale
  if (length(lineNo <- which(rowSums(weights) != 1)) > 0) {
    warning("Asset weights sum is not = 1 in rows: ", paste(lineNo, collapse = ", "),
            "\nAsset weights scaled accordingly.")
    weights[lineNo,] <- weights[lineNo,]/rowSums(weights)[lineNo]
  }
  if (printSteps==T) print("Weights checked.")

  # CHECK indicators
  if (is.xts(indicators)) {
    if (!is.null(ncol(indicators))) {
      ind <- indicators
      indicators <- lapply(indicators, FUN = function(x) x)
      if (is.null(colnames(ind))) names(indicators) <- paste0("Ind.", 1:length(indicators))
    } else {
      indicators <- list()
    }
  } # if indicator was in xts format, it is list now
  if (!is.list(indicators)) {
    stop("Please provide all indicators as univariate xts within a list or as single multivariate xts!")
  } else {
    if (length(indicators) > 0) {
      for (i in 1:length(indicators)) { #i<-1
        if (!is.xts(indicators[[i]])) {
          stop("Please provide all indicators within ist as univariate xts format!")
        } else if (ncol(indicators[[i]])!=1) {
          indicators[[i]] <- indicators[[i]][,1]
          warning("Please provide all indicators within ist as UNIVARIATE xts format! Only first column will be used.")
        }
      }
    }
  }
  if (printSteps==T) print("Indicators checked.")

  # MATCH strategy function and plot function
  if (is.null(strat)) stop("Please provide strategy function (in accordance with file name excl. extension) or custom file path!")
  if (length(grep("\\||/", strat)) > 0) { # when local strategy path passed
    stratFUN.src <- strat
    if (!file.exists(stratFUN.src)) stop("Could not find function file in ", stratFUN.src)
    strat <- basename(stratFUN.src)
    strat <- substr(strat, 1, nchar(strat)-2)
    source(stratFUN.src)
  }

  # SET strategy and plot function
  plotFUN <- NULL
  tryCatch({
    stratFUN <- get(paste0("strategy.", tolower(strat)), envir = environment(Strategy))
  }, error = function(e) stop(paste0("Strategy ", strat, " could not be found. Consult Strategy()-documentation for available strategies.")) )
  tryCatch({
    plotFUN <- get(paste0("plot.", tolower(strat)), envir = environment(Strategy))
  }, error = function(e) {}#message("No plot function defined. Generic plotting will be used. This is just an information.")
    )

  if (printSteps==T) print("Strategy function(s) checked.")

  # VOLUME
  if (!is.numeric(volume) || length(costs.fix) != 1)
    stop("Argument volume must be a single numeric value!")

  # COSTS
  if (!is.numeric(costs.fix) || length(costs.fix) != 1)
    stop("Argument costs.fix must be a single numeric value!")
  if (!is.numeric(costs.rel) || length(costs.rel) != 1)
    stop("Argument costs.rel must be a single numeric value!")





  # PREPARE ASSET TYPES
  # -------------------------------

  # CALCULATE Prices / log returns
  if (assetValueType == "logReturn")
    prices <- .LogReturnsToPrices(assets)
  if (assetValueType == "price")
    prices <- assets
  # no prices <= 0
  if (length(which(assets<=0)) > 0)
    message("Asset values < 0 found. With value type 'prices' this is unusual. This might fail in log return calculation!")


  if (printSteps==T) print("Prices set")



  # STRATEGY OUTPUT
  # -------------------------------

  # CALL strategy function and set values
  strat.Out <- stratFUN(prices = prices, weights = weights, indicators = indicators, parameters = strat.params)
  strat.params <- strat.Out[["parameters"]]
  filters <- strat.Out[["filters"]]
  signals <- as.xts(strat.Out[["signals"]]) #as.xts() and xts() to prevent conversion probs
  signals <- xts(signals, order.by=as.Date(index(signals), tz=""))
  prices <- as.xts(strat.Out[["prices"]])   #as.xts() and xts() to prevent conversion probs
  prices <- xts(prices, order.by=as.Date(index(prices), tz=""))
  weights <- abs(as.xts(strat.Out[["weights"]]))#as.xts and xts() to prevent conversion probs, absolute weights due to sign in SIGNAL!
  weights <- xts(weights, order.by=as.Date(index(weights), tz=""))
  indicators <- strat.Out[["indicators"]]

  if (!is.list(indicators) || length(names(indicators)) != length(indicators))
    stop("There must be an indicators output list of the strategy function within which all list entries are named! Can be simply an empty list()-object.")
  if (!is.list(filters) || length(names(filters)) != length(filters))
    stop("There must be a filter output list of the strategy function within which all list entries are named! Can be simply an empty list()-object.")

    # Ensure same date format
  # index(prices) <- as.Date(index(prices), tz="")
  # index(signals) <- as.Date(index(signals), tz="")
  # index(weights) <- as.Date(index(weights), tz="")

  # Ensure time consistency
  weights <- weights[index(signals)]

  # check date compatibility
  if (sum(index(signals) %in% index(prices)) == 0) warning("Signals and prices indexes do not match at all!")
  if (sum(index(weights) %in% index(prices)) == 0) warning("Signals and weight indexes do not match at all!")


  if (printSteps==T) print("Strategy function executed.")


  # CREATE S4 << Strategy >> OBJECT
  # -------------------------------

  new(Class = "Strategy"
      , prices = prices
      , weights = weights
      , indicators = indicators
      , stratFUN = stratFUN
      , plotFUN = plotFUN
      , strat = strat
      , strat.params = strat.params
      , filters = filters
      , signals = signals
      # , backtest.performance = NOT SET HERE, will be set in backtesting method
      # , backtest.parameters = NOT SET HERE, will be set in backtesting method
      # , backtest.setup = NOT SET HERE, will be set in backtesting method
      , volume = volume
      , costs.fix = costs.fix
      , costs.rel = costs.rel
      )
}
