#' @name format_y
#' @author Kolja Becker
#' @title format_y
#' @description Formats response variable based on the ml-type variable passed by the config file.
#' For regression analyses the response variable will be explicitly transformed to type numeric.
#' For Classification experiments the response variable will be explicitly transformed to a factor.
#' Time-to-event models are to be implemented in the near future.
#'
#' @param y vector of response varibale.
#' @param ml.type type of experiment (chracater).
#' @return a transformed version of the response variable y.
format_y = function(y, ml.type){
  if (ml.type == 'classification'){
    y = as.factor(y)
  } else if (ml.type == 'regression') {
    y = as.numeric(y)
  } else if (ml.type == 'survival') {
    stop("implementation of survival models pending - 2023-01-24")
  } else {
    stop("ml.type must be of 'classification', 'regression'")
  }
  return(y)
}
