#' Interval statistics for use with df_stats()
#' 
#' Function builders for calculating intervals. These must *always* be evaluated
#' with the *result* being handed as a argument to `df_stats()`.
#'
#' @param level Number in 0 to 1 specifying the confidence level for the interval. (Default: 0.95)
#' 
#' @rdname intervals
#' @aliases ci.mean ci.median ci.sd 
#' 
#' @examples
#' cover <- coverage(0.95)
#' df_stats(hp ~ cyl, data = mtcars, c95 = cover)
#' 
#' @export
coverage <- function(level = 0.95) {
  function(x, na.rm = TRUE) {
    bottom = (1 - level) / 2
    top = 1 - bottom
    res <- quantile(x, probs = c(bottom, top), 
                    na.rm = na.rm, type = 7, names = TRUE)
    names(res) <- paste("coverage", c("lower", "upper"), sep = "_")
    names(res) <- c("lower", "upper") # Do we prefer a short form?
    
    res
  }
}

#' @export
ci.mean <- function(level = 0.95) {
  level <- check.level(level)
  
  function(x, na.rm = TRUE) {
    n <- length(x)
    lowerq <- stats::qt((1 - level) / 2, df = n - 1)
    res <- 
      base::mean(x, na.rm = na.rm) + 
        c(lowerq, -lowerq) *
        stats::sd(x, na.rm = na.rm)/sqrt(length(x))
    
    # names(res) <- paste("ci_mean", c("lower", "upper"), sep = "_")
    names(res) <- c("lower", "upper") # Do we prefer a short form?
    
    res
  }
}

#' @export
ci.median <- function(level = 0.95) {
  level <- check.level(level)
  
  function(x, na.rm = TRUE) {
    x <- sort(x)
    n <- length(x)
    bottom <- (1 - level) / 2
    top <- 1 - bottom
    ci_index <- (n + qnorm(c(bottom, top)) * sqrt(n))/2 + c(0,1)
    res <- as.numeric(x[round(ci_index)])
    
    #names(res) <- paste("ci_median", c("lower", "upper"), sep = "_")
    names(res) <- c("lower", "upper") 
    res
  }
}

#' @export
ci.sd <- function(level = 0.95) {
  level <- check.level(level)

  function(x, na.rm = TRUE) {
    n <- length(x)
    bottom <- (1 - level) / 2
    top <- 1 - bottom
    res = (n - 1) * 
      stats::sd(x, na.rm = TRUE)^2 /
      qchisq(c(top, bottom), df = n - 1)
    res <- sqrt(res)
    #names(res) <- paste("ci_sd", c("lower", "upper"), sep = "_")
    names <- c("lower", "upper") 
    res 
  }
}

#' Function builder for proportions.
#' 
#' Evaluate this and hand the result to `df_stats()`
#' @param nm The level for which to find the proportion
#' 
#' @examples
#' \dontrun{
#' df_stats(mtcars, ~ cyl, proportion(6))
#' }
#' @export
proportion <- function(nm = NULL) {
  function(x) {
    if (is.null(nm)) {
      nm <- if (is.logical(x)) TRUE else unique(x)[1]
    }
  
    c(prop = mean(x == nm[1]))
  }
}

#' Function builder for confidence intervals on proportions
#' 
#' Similar to `proportion`, but 
#' 
#' @param level The confidence interval (Default: 0.95)
#' @param nm The level for which to find the proportion
#' @examples
#' \dontrun{
#' df_stats(mtcars, ~ cyl, cyl_prop = ci.proportion(6, level = 0.90))
#' }
#' @export
ci.proportion <- function(nm = NULL, level = 0.95) {
  level <- check.level(level)

  function(x) {
    if (is.null(nm)) {
      nm <- if (is.logical(x)) TRUE else unique(x)[1]
    }
    
    prob <- mean(x == nm[1])
    n <- length(x)
    bottom <- (1 - level) / 2
    top <- 1 - bottom
    res <- qbinom(c(bottom, top), size = length(x), prob = prob) / n
    # names(res) <- paste(nm, c("lower", "upper"), sep = "_")
    names <- c("lower", "upper")
    res 
  }
} 

# internal use
check.level <- function(level) {
  res <- ifelse(level > 1, 1, ifelse(level < 0, 0, level))
  if (level < 0 | level > 1) warning("level ", level, " is not between 0 and 1. Setting to ", res)

  res
  }  
