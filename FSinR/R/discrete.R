checkIfValuesAreFew <- function(collection) {
  ammountLimitPercentage <- 0.15
  return(length(unique(collection)) / length(collection) <= ammountLimitPercentage)
}

thereAreFewPossibleValuesOrCollectionIsShort <- function(collection) {
  sizeThreshold <- 30
  collectionIsLong <- length(collection) > sizeThreshold
  if (collectionIsLong) {
    return(checkIfValuesAreFew(collection))
  }
  return(TRUE)
}

allElementsAreNumbersOrStringifiedNumbers <- function(collection) {
  return(suppressWarnings(!any(is.na(as.numeric(as.character(collection))))))
}

allElementsAreIntegerNumbers <- function(collection) {
  return(isTRUE(all((collection == suppressWarnings(as.integer(collection))))) || all(typeof(collection) == "integer"))
}

allElementsAreStrings <- function(collection) {
  return(all(is.character(collection)))
}

is.discrete <- function(collection) {
  if (is.factor(collection)) {
    return(TRUE)
  }
  if (allElementsAreNumbersOrStringifiedNumbers(collection)) {
    return(allElementsAreIntegerNumbers(collection) && thereAreFewPossibleValuesOrCollectionIsShort(collection))
  }
  return(allElementsAreStrings(collection))
}

#' @author Alfonso Jiménez Vílchez
#' @title isDataFrameDiscrete(dataframe)
#' @description Estimate if all variables in a data frame are discrete
#' @param dataframe - A data frame
#'
#' @return - True if all variables are discrete, False otherwise
#' @export
#'
#' @examples
#' isDataframeDiscrete(mtcars)
#' isDataframeDiscrete(iris)
isDataframeDiscrete <- function(dataframe) {
  return(all(sapply(dataframe, is.discrete)))
}

#' @author Alfonso Jiménez Vílchez
#' @title isDataframeContinuous(dataframe)
#' @description Estimate if all variables in a data frame are continuous
#' @param dataframe - A data frame
#'
#' @return - True if all variables are continuous, False otherwise
#' @export
#'
#' @examples
#' isDataframeContinuous(mtcars)
#' isDataframeContinuous(iris)
isDataframeContinuous <- function(dataframe) {
  return(all(sapply(dataframe, is.numeric)))
}