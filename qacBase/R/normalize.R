#' @title
#' Normalize numeric variables
#'
#' @description
#' Normalize the numeric variables in a data frame
#'
#' @details
#' \code{normalize} transforms all the numeric variables
#' in a data frame to have the same minimum and maximum values.
#' By default, this will be a minimum of 0 and maximum of 1.
#' Character variables and factors are left unchanged.
#'
#' @note
#' Use this function to be transform variables into a given range. The default
#' is [0, 1], but [-1, 1], [0, 100], or any other range is permissible.
#'
#' @param data a data frame.
#' @param new_min minimum for the transformed variables.
#' @param new_max maximum for the transformed variables.
#'
#' @return a data frame
#' @export
#' @examples
#' head(cars74)
#'
#' cars74_st <- normalize(cars74)
#' head(cars74_st)
normalize <- function(data, new_min = 0, new_max = 1) {
  if(!is.data.frame(data)) stop("data must be a data frame")

  rerange <- function(x) {
      if (is.numeric(x)) {
        oldmin <- min(x, na.rm=TRUE)
        newmax <- max(x, na.rm=TRUE)
        x <- (new_max - new_min)/(newmax-oldmin)*(x-oldmin)+new_min
    }
    return(x)
  }
  for (i in 1:ncol(data)){
    data[, i] <- rerange(data[[i]])
  }
  return(data)
}
