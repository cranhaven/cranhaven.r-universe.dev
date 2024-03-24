#' @title Machine learning data partition
#'
#' @description A method of partitioning data between training and testing sets based on the fraction of data used for training
#' @param x,y Numeric data
#' @param l Numeric labels data
#' @param train_size Fraction of total data that the SVM will train on
#' @param rand_state Value of the random state used to set the seed
#' @return Two data frames and a list of indicies for the training set
#' @keywords machine-learning data
#' @export
pac.partition <- function(x,y,l, train_size=0.7, rand_state=sample(1:2^15, 1)){
  set.seed(rand_state)
  train_idx <- sample(1:length(x), trunc(length(x)*train_size), replace=FALSE)
  test_idx  <- (1:length(x))[-(train_idx)]

  train     <- data.frame(x[train_idx],
                          y[train_idx],
                          factor(c(l[train_idx])))
  colnames(train) <- c("x", "y", "labels")

  test    <- data.frame(x[test_idx],
                          y[test_idx],
                        factor(c(l[test_idx])))
  colnames(test) <- c("x", "y", "labels")

  return(list(train=train, test=test, train_idx=train_idx))
}
#' @title Radian angle conversion
#'
#' @description Conversion between radians and degrees
#' @param rad Angle in radians
#' @return Angle in degrees
#' @keywords conversion
#' @export
rad2deg <- function(rad) {(rad * 180) / (pi)}

#' @title Degree angle conversion
#'
#' @description Conversion between degrees and radians
#' @param deg Angle in degrees
#' @return Angle in radians
#' @keywords conversion
#' @export
deg2rad <- function(deg) {(deg * pi) / (180)}

#' @title Linear map
#'
#' @description A function that will map a range of values to a different set of values.
#' @param x Range of values to be mapped
#' @param i Lowest value
#' @param f Largest value
#' @return A set of values spanning from i to f
#' @export
linMap  <- function(x,i, f) {(x - min(x))/max(x - min(x)) * (i - f) + f}

#' @title Unit formatting
#'
#' @description Converts unit inputs into a format that can be displayed. Support is restricted to `degC`, `degF`.
#' @param unit Unit input
#' @return A list of formatted units
#' @export
unit_format <- function(unit) {
  # Unit formating for temperature
  if (identical(unit, "degC")) {
    unit_box <- "*o*C"
    unit <- "\u00B0C"
  } else if (identical(unit, "degF")) {
    unit_box <- "*o*F"
    unit <- "\u00B0F"
  } else{
    unit_box <- unit
  }
  return(list(unit=unit, unit_box=unit_box))
}
