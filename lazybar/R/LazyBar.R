LazyBar <- R6::R6Class("LazyBar", public = list(
  n = NULL,
  i = 0,
  time = NULL,
  fn = NULL,
  dotdotdot = NULL,
  initialize = function(n, method = "drift", fn = NULL, ...){
    self$n <- n
    self$time <- numeric(n+1)
    self$start()
    self$dotdotdot <- list(...)
    # self$method <- method
    if(!is.null(fn)){
      self$fn <- match.fun(fn, descend = FALSE)
    } else {
      self$fn <- switch(method[[1]],
                        average = .average_fn,
                        drift = .drift_fn,
                        naive = .naive_fn,
                        snaive = .snaive_fn)
    }
  },

  start = function(){
    self$i <- 0
    self$time[[1]] <- proc.time()[[3]]
    invisible(self)
  },

  tick = function(){
    if(self$i >= self$n) stop("Process ended")
    self$i <- self$i + 1
    self$time[[self$i+1]] <- proc.time()[[3]]
    self
  },

  print = function(){
    if(!interactive()) return(invisible(self))
    now <- proc.time()[[3]]


    dtime <- diff(self$time[seq(1, self$i+1)])
    #
    # eta <- sum(((dtime[[self$i]]-dtime[[1]])/ifelse((te <- self$i-1) ==0, 1, te))*seq(1, left)) + left*dtime[[self$i]]
    # if(eta<0){
    #   eta <- mean(dtime)*left
    # }
    if(length(self$dotdotdot) != 0){
      dots <- paste0(names(self$dotdotdot), "=self$dotdotdot$", names(self$dotdotdot), collapse = ", ")
      if(self$i < self$n)
        eval(parse(text = paste0("eta <- self$fn(dtime, self$i, self$n,",dots ,")")))
    } else {
      if(self$i < self$n)
        eta <- self$fn(dtime, self$i, self$n)
    }

    # eta <- self$fn(dtime, self$i, self$n, self$dotdotdot)


    width <- getOption("width")- nchar("|100.0% ~elapsed: 99 h 00 m 00 s") - 2

    bar <- paste0(c(
      "|",
      paste(rep.int("=", floor(self$i/self$n * width))),
      paste(rep.int("-", width- floor(self$i/self$n * width))),
      "|",
      format(round(self$i/self$n *100, 1), width = 4),
      "% ",
      "~",
      if(self$i<self$n){
        c("eta: ", print_time(eta))
      } else {
        c("elapsed: ", print_time(self$time[[self$n]] - self$time[[1]]))
      }
    ),
    collapse = "")
    blank <- max(c(0, getOption("width") - nchar(bar, "width")))
    cat("\r", bar, rep.int(" ", blank), sep = "")
    if(self$i==self$n) cat("\n")
    utils::flush.console()
    invisible(self)
  }


)
)

#' Progress bar with customisable estimated remaining time
#'
#' Display a progress bar displaying the estimated time.
#' The purpose of having various estimation methods is to
#' provide a more accurate estimation when the run time between
#' ticks is assumed to be different, e.g., online estimation,
#' time series cross validation, expanding window approach, etc.
#'
#'
#' Four simple forecasting methods are available for
#' the estimation of the remaining time:
#' Average method (default), Drift method, Naive method and
#' Seasonal naive method.
#' For the summary of the simple methods, see Chapter 3 of \code{References}.
#' User can also supply their customised estimation method as a function.
#' See \code{Arguments} and \code{Examples}.
#'
#' @param n Integer. Total number of ticks
#' @param method Character. The embedded forecasting method of remaining time:
#' \code{drift} (default), \code{average}, \code{naive}, or \code{snaive}.
#' Ignored if \code{fn} is not \code{NULL}.
#' \describe{
#' \item{\code{average} (default)}{Average method. The run time between future ticks are assumed to
#' be the average run time of the past ticks.
#' This is the most common estimation method for remaining time.}
#' \item{\code{drift}}{Drift method. The run time between future ticks are
#' assumed to increase (decrease), and the level changed is set to be the average change
#' of the run time of the past ticks.
#' This is to assume the run time between ticks is linearly increasing or decreasing.}
#' \item{\code{naive}}{Naive method. The run time between future ticks are assumed to be
#' the run time between the last two ticks,}
#' \item{\code{snaive}}{Seasonal naive method. If this method is chosen, an argument of \code{s}
#' needs to be supplied in the \code{...}.
#' The run time between future ticks is set to be the run time \code{s} times before.
#' By default \code{s} is set to be 1/10 of the total number of ticks.}
#' }
#'
#' @param fn Function. User defined function to estimate the remaining time.
#' The function should predict the remaining time using the arguments and
#' return a scalar.
#' It should have at least three arguments in the order of \code{dtime}, \code{i}, and \code{n},
#' which represent the status of the progress bar at the current tick:
#' \describe{
#' \item{\code{dtime}}{A numeric vector of the run time between past ticks.}
#' \item{\code{i}}{The number of the current tick.}
#' \item{\code{n}}{The number of total ticks.}
#' }
#' @param ... Other arguments to pass to estimation method.
#' The arguments need to be named.
#'
#' @return An R6 object with methods \code{tick()} and \code{print()}.
#' @author Yangzhuoran Fin Yang
#' @references Hyndman, R.J., & Athanasopoulos, G. (2018) Forecasting: principles and practice, 2nd edition, OTexts: Melbourne, Australia. OTexts.com/fpp2. Accessed on 24/04/2020.
#' @examples
#' \donttest{
#' pb <- lazyProgressBar(4)
#' pb$tick()
#' pb$tick()
#' pb$tick()
#' pb$tick()
#'
#' # With linearly increasing run time
#' pb <- lazyProgressBar(4, method = "drift")
#' for(i in 1:4){
#'   Sys.sleep(i * 0.2)
#'   pb$tick()$print()
#' }
#'
#' # With user defined forecast function
#' # The forecast function itself will
#' # require certain computational power
#' forecast_fn <- function(dtime, i, n, s = 10){
#'   # When the number of ticks is smaller than s
#'   # Estimate the future run time
#'   # as the average of the past
#'   if(i<s){
#'     eta <- mean(dtime)*(n-i)
#'   }
#'
#'   # When the number of ticks is larger than s
#'   # Fit an arima model every s ticks
#'   # using forecast package
#'   if(i>=s){
#'     if(i %% s ==0){
#'       model <- forecast::auto.arima(dtime)
#'     }
#'     runtime <- forecast::forecast(model, h=n-i)$mean
#'     if(i %% s !=0){
#'       runtime <- runtime[-seq_len(i %% s)]
#'     }
#'     eta <- sum(runtime)
#'   }
#'   return(eta)
#' }
#'
#' pb <- lazyProgressBar(10, fn = forecast_fn, s=3)
#' for(i in 1:10){
#'   Sys.sleep(i * 0.2)
#'   pb$tick()$print()
#' }
#' }
#' @export
lazyProgressBar <- function(n,
                            method = "average",
                            fn = NULL,
                            ...){

  LazyBar$new(n, method, fn, ...)
}

print_time <- function(x) {
  if (x < 60) {
    paste(round(x), "s")
  } else if (x < 60 * 60) {
    paste(floor(x / 60), "m", round(x %% 60), "s" )
  } else {
    paste(floor(x / (60 * 60)), "h",
          floor(x %% (60*60)/60), "m",
          round(x %% (60 * 60) %% 60), "s")
  }
}

## ---- forecasting method ----
.drift_fn <- function(dtime, i, n){
  left <- n-i
  eta <- sum(((dtime[[i]]-dtime[[1]])/ifelse((te <- i-1) ==0, 1, te))*seq(1, left)) + left*dtime[[i]]
  if(eta<0){
    eta <- mean(dtime)*left
  }
  return(eta)
}

.average_fn <- function(dtime, i, n){
  left <- n-i
  eta <- left*mean(dtime)
  return(eta)
}

.naive_fn <- function(dtime, i, n){
  left <- n-i
  eta <- left*dtime[[i]]
  return(eta)
}

.snaive_fn <- function(dtime, i, n, s = max(1, floor(n/10))){
  left <- n-i
  if(i<s){
    eta <- (c(rep(dtime[1:i], floor(left/i)),dtime[seq_len(left %% i)]))
  } else {
    # eta <- (c(rep(dtime[1:i][seq.int(to = i, length.out = s)], ceiling(left / i)),
    # dtime[1:i][seq.int(from = 1, length.out = left %% i)]))
    eta <- rep(dtime[1:i][seq.int(to = i, length.out = s)], ceiling(left / s))
    if(left%%s!=0){
      eta <- eta[-seq.int(to = length(eta), length.out = s-(left %%s))]
    }
  }
  return(sum(eta))
}

# pb <- lazyProgressBar(10, method = "snaive", s=2)
# for(i in 1:10){
#   Sys.sleep(i * 0.2)
#   pb$tick()$print()
# }

