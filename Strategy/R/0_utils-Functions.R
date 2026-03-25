# Make new Strategy from Strategy-Template
# name: name of new function
# file.path: path to store template to
# overwrite: overwrite existing file?
#' @export
#' @aliases newStrategyFunction
#' @title Create Own Strategy
#' @description Creates a strategy function template file. This file can be used as template for the development of customized strategies.
#' @param name String as name of the new function (without spaces).
#' @param file.path Valid file path of existing directory where the new function shall be stored in format file.path/name.R.
#' @param overwrite If the strategy file already exists, it will be overwritten if value is \code{TRUE}.
#' @examples
#' ##Not run:
#'
#' # Creates a file myNewStrat.R at the specific file path
#' newStrategyFunction(name="myNewStrat", file.path=getwd(), overwrite=T)
#'
#' ##End(Not run)
newStrategyFunction <- function(name=NULL, file.path=getwd(), overwrite=FALSE) {
  if (is.null(name)) stop("Please define function name!")
  if (!file.exists(file.path)) stop("Please define valid directory to store function in!")
  file.to <- paste0(file.path, "/", name, ".R")
  if (file.exists(file.to)) {
    if (overwrite==T) file.copy(from=system.file("newStrategy.R", "Strategy"), to=file.to, overwrite=overwrite)
    else stop("File already exists and default is not to overwrite files!")
    } else {
      file.from <- file.path(path.package("Strategy"), "newStrategy.R")
      if (!file.exists(file.from)) stop("Source file does not exist. Please update package and/or contact administrator!")
      file.copy(from=file.from, to=file.to)
      }
  cat(paste0("File copied to ", file.to), "\n")
}


# Add Periods to date.
# date: current date to add periods to
# add.periods: number of periods
# period: type of period
add.periods <- function(date, add.periods = 1, period = "months") {
  periods <- c("days", "weeks", "months", "quarters", "years")
  period.valid <- pmatch(period, periods)
  if (is.na(period.valid))
    stop("Please provide valid period of days, weeks, months, quarters or years.")
  period <- periods[period.valid]
  new.date <- seq.Date(from=date, by=paste(add.periods, period, collapse=""), length.out=2)[2]
  return(new.date)
}


# Get Environment from variable x
getEnv <- function(x) {
  x.name <- deparse(substitute(x))
  globals <- ls(envir=.GlobalEnv)
  envirs <- globals[sapply(globals, function(var) is.environment(get(var)))]
  envirs <- c('.GlobalEnv', envirs)
  x.envir <- sapply(envirs, function(env) x.name %in% ls(envir=get(env)))
  envirs[x.envir]
}

# Validate which values for column-based or list-based data selection
# used within methods to validate selected assets, filters and indicators
validWhich <- function(which, data) {
  if (is.null(data) || (!is.xts(data) && !is.list(data)))
    stop("Please provide data as list or xts!")

  which.out <- NULL

  # select valid which arguments to which.out for either list or xts
  if (is.list(data)) {
    if (length(data) == 0) return(NULL)
    if (is.numeric(which)) {
      which.out <- which[which %in% 1:length(data)]
    } else if (is.null(which)) {
      which.out <- 1:length(data)
    } else if (is.character(which)) {
      which.out <- names(data)[tolower(names(data)) %in% tolower(which)]
    }
  } else if (is.xts(data)) {
    if (is.null(ncol(data))) return(NULL)
    if (is.numeric(which)) {
      which.out <- which[which %in% 1:ncol(data)]
    } else if (is.null(which)) {
      which.out <- 1:ncol(data)
    } else if (is.character(which)) {
      which.out <- colnames(data)[tolower(colnames(data)) %in% tolower(which)]
    }
  }

  # verification
  if (length(which.out) == 0)
    stop("Which values all invalid!")
  # warn for cutted which values or stop if none available
  if (!is.null(which) && length(which) != length(which.out))
    warning(paste0("Which values have been removed because there was no such data/-number: "
                   , paste0(which[which(! which %in% which.out)], collapse=", ")))

  # return
  return(which.out)
}



# ---HIDDEN -------------------------------------------------------------------
# GET PRICES from Returns
.LogReturnsToPrices <- function(logReturns, price_t0=100) {
  prices <- price_t0*cumprod(exp(logReturns))
  return(prices)
}
# GET logReturns from Prices
.PricesToLogReturns <- function(prices) {
  logReturns <- diff(log(prices), lag=1)
  logReturns[1,] <- 0
  return(logReturns)
}

# PERIOD Calculation
.toPeriod <- function(data=NULL, period=NULL) {
  if (is.null(data) || !is.xts(data)) stop("Please provide data and period (as in xts-package)!")
  # PERIOD in NOUNS! e.g. "days" or "months"
  periods <- c("days", "weeks", "months", "quarters", "years")
  period.valid <- pmatch(period, periods)
  if (is.na(period.valid))
    stop("Please provide valid period of days, weeks, months, quarters or years.")
  period <- periods[period.valid]
  data.tmp <- Reduce(cbind, lapply(data, FUN=function(x, period){ to.period(x, period=period)[, 4] }, period=period)) # Col 4 = Close
  colnames(data.tmp) <- colnames(data)
  data <- as.xts(data.tmp)
  return(data)
}

# Standard annualization factors
.annFactor <- function(data) {
  if (!is.xts(data)) stop("Please provide data as xts!")
  freq <- periodicity(data[,1])$scale
  scalevec <- rep(1, ncol(data))
  switch(freq,
         minute = {
           stop("Data periodicity too high")
         }, hourly = {
           stop("Data periodicity too high")
         }, daily = {
           scale = 252
         }, weekly = {
           scale = 52
         }, monthly = {
           scale = 12
         }, quarterly = {
           scale = 4
         }, yearly = {
           scale = 1
         })
  scalevec <- rep(scale, ncol(data))
  return(scalevec)
}


# Check if submitted parameters match the required names

# Standard declare parameters function - new Strategy --> edit default parameters
.stratFUN.declareParams <- function(defaultParams, parameters = NULL, printWarnings=T) {

  # IF no parameters given return directly defaults
  if (is.null(parameters)) return(defaultParams)
  if (!is.list(parameters)) stop("Please provide parameter stratFUN.parameters in list format!")

  # Check if names of categories are correct
  nmsC <- names(defaultParams)
  namc <- names(parameters)

  # Check for unknown parameter names
  if ( length(noNms <- namc[!(namc %in% nmsC)]) )
    if (printWarnings==T) warning("Unknown name in parameters: ", paste(noNms, collapse = ", "))

  # Check for each category if parameters are correct
  for (category in names(defaultParams)) {
    obj <- defaultParams[[category]]
    pars <- parameters[[category]]
    if (!is.null(pars)) {
      if (is.list(obj)) {
        nmsC <- names(obj)
        obj[(namc <- names(pars))] <- pars
        if ( length(noNms <- namc[!(namc %in% nmsC)]) )
          if (printWarnings==T) warning("Unknown name in parameters: ", paste(noNms, collapse = ", "))
      } else {
        # Pass direct to object
        obj <- pars
      }
    }
    defaultParams[[category]] <- obj
  }

  return(defaultParams)
}



# LATEX OUTPUT -------------------------------------------------------------------

# used in bold headings
bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}


# BACKTESTING required FUNS ----------------------------------------------------------------------

# Retrieve nb of periods until horizon based on
# frequency of data (xts object))
# i.e 1 month horizon for daily data corresponds to 21 days
periods2Horizon <- function(data, horizon){
  # Nb of periods per year corresponding to frequency of data
  data.frequency <- retrievePeriodicity(data)

  # Extracts numeric from horizon
  periodLength = as.numeric(substr(horizon, 1, nchar(horizon) - 1))
  # Extracts characters from horizon
  periodUnit = gsub(periodLength, "", horizon)

  # Retrieve nb of periods per year corresponding to horizon
  target.frequency <- switch(tolower(substr(periodUnit, 1, 1)),
                             d = 252, w = 52, m = 12,
                             q = 4, y = 1, s = 21772800)
  # Treats minutes specially
  if (tolower(substr(periodUnit, 1 ,2))=="mi")
    target.frequency = 362880

  # Calculate how many periods remains up to horizon
  periods <- data.frequency/target.frequency * periodLength

  # Output
  return(periods)
}

# Retrieve periodicity of data
# i.e 12 for monthly data, 4 for quarterly, 1 for yearly
retrievePeriodicity <- function (object)
{
  if (is.character(object)) {
    period <- tolower(object)
  }
  else {
    period <- periodicity(object)$scale
  }
  periodicity <- switch(tolower(substr(period, 1, 1)), d = 252,
                        w = 52, m = 12, q = 4, y = 1, s=21772800)

  # Treats minutes specially
  if (tolower(substr(period, 1 ,2))=="mi")
    periodicity = 362880

  # Output
  return(periodicity)
}


# Change the data frequency of a given dataset
# frequency     : "years", "quarters", "months", "days", "minutes"
# fun2Apply     : allows to specify a function (i.e "mean" or "sum")
#                 in order to apply to dataset within periods
# columns2Apply : can be a numeric indicating to which columns
#                 the function fun2Apply will be applied.
#                 can also be character strings corresponding to
#                 columns' names that will be partially matched
#                 (pattern)
changePeriodicity <- function(data, frequency, col.names=NULL,
                              fun2Apply=NULL,
                              columns2Apply=NULL){

  periodicity <- switch(tolower(substr(frequency, 1, 1)),
                        d = "days", w = "weeks", m = "months",
                        q = "quarters", y = "years", s = "seconds")
  # Treats minutes specially
  if (tolower(substr(frequency, 1 ,2))=="mi")
    periodicity = "minutes"

  column.names <- colnames(data)
  if (!is.null(col.names)){
    if (length(col.names)!=dim(data)[2])
      stop("col.names should have the same dimension as the data")

    column.names <- col.names
  }

  dataset <- data

  # Should a function applied to the subsetted data
  # i.e for costs you can add up the values over a month
  flagsFun <- rep(FALSE, ncol(dataset))
  if (!is.null(fun2Apply)){
    if (is.numeric(columns2Apply)){
      if (ncol(dataset)>=max(columns2Apply)){
        flagsFun[columns2Apply] <- TRUE
      } else {
        stop("Selected columns are not in the dataset !")
      }

    }
    if (is.character(columns2Apply)){
      if (length(columns2Apply)==1 && tolower(columns2Apply)=="all"){
        flagsFun <- rep(TRUE, ncol(dataset))
      } else {
        sapply(columns2Apply, FUN=function(x){
          match.idx <- grep(x, column.names)
          if (length(match.idx)!=0)
            flagsFun[match.idx] <<- TRUE
        })
      }
    }
  }

  res <- Reduce(
    cbind, lapply(
      1:dim(dataset)[2], FUN=function(x, periodicity){
        data <- dataset[, x]

        # Retrieve data for given data frequency
        period.data <- to.period(data, period=periodicity)
        period.data <-  period.data[, "data.Close"]



        # If you want to aggregate and not only subset data
        # for instance costs sum up over month
        flagFun <- flagsFun[x]
        if (!is.null(fun2Apply) && isTRUE(flagFun)){
          fun2Apply <- match.fun(fun2Apply)

          agg.data <- lapply(1:length(index(period.data)), FUN=function(idx, period.data, data){
            if (idx==1){
              sub.data <- subset(data, index(data)<=index(period.data)[idx])
            } else {
              sub.data <- subset(data, index(data)<=index(period.data)[idx]  & index(data)>index(period.data)[(idx-1)])
            }

            return(sub.data)
          }, period.data=period.data, data=data)

          agg.data <- Reduce(c, lapply(agg.data, fun2Apply))
          period.data[, "data.Close"] <- agg.data
        }

        # output
        return(period.data)
      }, periodicity=periodicity)
  )

  colnames(res) <- column.names

  return(res)
}



# Extract rolling windows from data given a period width and a
# time step (i.e. extract for each month a 12m period)
# option to keep history over time
xts.rollingWindows <- function (x, period = "12m", by = "1m",
                                keep.history = FALSE)
{

  # Extracts numeric from period
  periodLength = as.numeric(substr(period, 1, nchar(period) - 1))
  # Extracts characters from period
  periodUnit = gsub(periodLength, "", period)

  # Extracts numeric and characters from by
  byLength = as.numeric(substr(by, 1, nchar(by) - 1))
  byUnit = gsub(byLength, "", by)

  # Change periodicity of data to match period
  x <- changePeriodicity(x, periodUnit)

  # Check if unit of period and by are the same
  if (periodUnit != byUnit)
    stop("period and by should have the same frequency !")

  positions = time(x)
  datePositions = unique((positions))
  numberOfPositions = length(datePositions)
  startSeq <- seq(from = 1, to = (numberOfPositions - periodLength + 1), by = byLength)
  if (isTRUE(keep.history))
    startSeq <- rep(startSeq[1], length(startSeq)) #start all periods at initial start date

  startDates = as.character(datePositions[startSeq])
  endSeq <- seq(from = periodLength, to = numberOfPositions, by = byLength)
  endDates = as.character(datePositions[endSeq])
  windows = list(start = startDates, end = endDates)
  attr(windows, "control") = list(start = start(positions), end = end(positions), period = period, by = by)
  windows
}


