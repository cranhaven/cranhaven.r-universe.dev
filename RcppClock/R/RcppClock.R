#' RcppClock
#' 
#' Time Rcpp functions and summarize, print, and plot runtime statistics
#'
#' @rdname RcppClock
#' 
#' @details 
#' See https://github.com/zdebruine/RcppClock/readme.md for information on how to use the package.
#' 
#' @section RcppClock functions:
#' See the RcppClock README on \code{https://github.com/zdebruine/RcppClock#readme} for basic usage examples.
#' 
#' When the Rcpp `Rcpp::clock::stop()` method is called in Rcpp code, an S3 \code{RcppClock} object
#' will be created in the global environment. This object contains three methods:
#' 
#' * \code{summary}: computes runtime summary statistics and returns a \code{data.frame}
#' * \code{print}:  runs \code{summary} and then prints the resulting \code{data.frame}
#' * \code{plot}:  a ggplot2 violin plot with jitter points showing runtimes for each expression
#' 
#' The \code{\link{fibonacci}} function is a simple example of how to use RcppClock. 
#' See the source code on \code{github.com/zdebruine/RcppClock/src/fibonacci.cpp}
#' 
#' @docType package
#' @name RcppClock
#' @useDynLib RcppClock
#' @importFrom stats aggregate sd
#' @import ggplot2
#' @aliases RcppClock, RcppClock-package, RcppClock-class
#' @seealso \code{\link{fibonacci}}
#' @md
#' @examples
#' library(RcppClock)
#' fibonacci(n = 25:35, reps = 10)
#' # this function creates a global environment variable "clock"
#' #   that is an S3 RcppClock object
#' clock
#' plot(clock)
#' summary(clock, units = "ms")
#' 
#' \dontrun{
#' # this is the Rcpp code behind the "fibonacci" example function
#'
#' ```{Rcpp}
#' //[[Rcpp::depends(RcppClock)]]
#' #include <RcppClock.h>
#'
#' int fib(int n) {
#' return ((n <= 1) ? n : fib(n - 1) + fib(n - 2));
#' }
#'
#' //[[Rcpp::export]]
#' void fibonacci(std::vector<int> n, int reps = 10) {
#'   Rcpp::Clock clock;
#'   while(reps-- > 0){
#'     for(auto number : n){
#'       clock.tick("fib" + std::to_string(number));
#'       fib(number);
#'       clock.tock("fib" + std::to_string(number));
#'     }
#'  }
#'  clock.stop("clock");
#' }
#' ```
#' }
NULL

#' @rdname RcppClock
#' @param x RcppClock object
#' @param object RcppClock object
#' @param units nanoseconds (\code{"ns"}), microseconds (\code{"us"}), milliseconds (\code{"ms"}), seconds (\code{"s"}), or auto (\code{"auto"})
#' @param ... arguments to other functions
#' @export
#' 
summary.RcppClock <- function(object, units = "auto", ...){
  min_time <- min(object$timer[object$timer != 0])
  if(is.na(min_time)) min_time <- 0
  if(units == "auto"){
    if(min_time > 1e8){
      units <- "s"
    } else if(min_time > 1e5){
      units <- "ms"
    } else if(min_time > 1e2){
      units <- "us"
    } else {
      units <- "ns"
    }
  }
  if(units == "s"){
    object$timer <- object$timer / 1e9
  } else if (units == "ms") {
    object$timer <- object$timer / 1e6
  } else if (units == "us") {
    object$timer <- object$timer / 1e3
  }
  
  # summarize results
  object <- data.frame("timer" = object$timer, "ticker" = object$ticker)
  df2 <- aggregate(object$timer, list(ticker = object$ticker), mean)
  colnames(df2)[2] <- "mean"
  df2$sd <- aggregate(object$timer, list(ticker = object$ticker), sd)$x
  df2$min <- aggregate(object$timer, list(ticker = object$ticker), min)$x
  df2$max <- aggregate(object$timer, list(ticker = object$ticker), max)$x
  object$timer <- 1
  df2$neval <- aggregate(object$timer, list(ticker = object$ticker), sum)$x

  long_units <- c("seconds", "milliseconds", "microseconds", "nanoseconds")
  short_units <- c("s", "ms", "us", "ns")
  attr(df2, "units") <- long_units[which(short_units == units)]
  df2
}

#' @method print RcppClock
#' @rdname RcppClock
#' @export
print.RcppClock <- function(x, ...){
  df <- summary(x, units = "auto")
  cat("Unit:", attr(df, "units"), "\n")
  print(df, digits = 4, row.names = FALSE)
  invisible(x)
}

#' @export
#' @method plot RcppClock
#' @rdname RcppClock
plot.RcppClock <- function(x, ...) {
  min_time <- min(x$timer[x$timer != 0])
  if(is.na(min_time)) min_time <- 0
  if(min_time > 1e8) {
    units <- "s"
    x$timer <- x$timer / 1e9
  } else if(min_time > 1e7) {
    units <- "ms"
    x$timer <- x$timer / 1e6
  } else if(min_time > 1e2) {
    units <- "us"
    x$timer <- x$timer / 1e3
  } else {
    units <- "ns"
  }

  long_units <- c("seconds", "milliseconds", "microseconds", "nanoseconds")
  short_units <- c("s", "ms", "us", "ns")

  df <- data.frame("timer" = x$timer, "ticker" = x$ticker)

  suppressWarnings(print(ggplot(df, aes_string(y = "ticker", x = "timer")) + 
    geom_violin() + 
    geom_jitter(height = 0.1) + 
    theme_classic() + 
    scale_x_continuous(trans = "log10") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(y = "", x = paste0("runtime (", long_units[which(short_units == units)], ")"))))
}
