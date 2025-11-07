#' @title
#' Standardize numeric variables
#'
#' @description
#' Standardize the numeric variables in a data frame
#'
#' @details
#' \code{standardize} transforms all the numeric variables
#' in a data frame to have the same mean and standard deviation.
#' By default, this will be a mean of 0 and standard deviation of 1.
#' Character variables and factors are left unchanged. By default,
#' dummy coded variables are also left unchanged. Use
#' \code{include_dummy=TRUE} to transform these variables as well.
#'
#' @param data a data frame.
#' @param mean mean of the transformed variables.
#' @param sd standard deviation of the transformed variables.
#' @param include_dummy logical. If \code{TRUE}, transform
#' dummy coded (0,1) variables.
#'
#' @return a data frame
#' @export
#' @examples
#' head(cars74)
#'
#' cars74_st <- standardize(cars74)
#' head(cars74_st)
standardize <- function(data, mean = 0, sd = 1,
                        include_dummy=FALSE) {
  if(!is.data.frame(data)) stop("data must be a data frame")
  std <- function(x) {
    # is this a numeric variable?
    number <- is.numeric(x)

    # is this a dummy coded variable?
    values <- unique(x)
    values <- values[order(values)]
    cond1 <- length(values) == 2
    cond2 <- values[1] == 0
    cond3 <- values[2] == 1
    dummy <- cond1 & cond2 & cond3

    # variables to standardize
    if (include_dummy){
      doit <- number
    } else {
      doit <- number & !dummy
    }
    if (doit) {
      x <- (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
      x <- x * sd + mean
    }
    return(x)
  }
  for (i in 1:ncol(data)){
    data[, i] <- std(data[[i]])
  }
  return(data)
}

