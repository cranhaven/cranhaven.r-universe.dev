#' Tests for Structure of Covariance Matrices
#'
#' Performs Tests for the structure of covariance matrices.
#'
#' @param x data as a list of matrices
#' @param Sigma Population covariance matrix as a matrix
#' @param ... other options passed to covTest method
#'
#' @return A list with class "htest" containing the following components:
#'
#'\tabular{ll}{
#' \code{statistic} \tab the value of equality of covariance test statistic \cr
#' \tab \cr
#' \code{parameter} \tab the degrees of freedom for the chi-squared statistic \cr
#' \tab \cr
#' \code{p.value} \tab the p=value for the test \cr
#' \tab \cr
#' \code{estimate} \tab the estimated covariances if less than 5 dimensions \cr
#' \tab \cr
#' \code{null.value} \tab the specified hypothesized value of the covariance difference \cr
#' \tab \cr
#' \code{alternative} \tab a character string describing the alternative hyposthesis \cr
#' \tab \cr
#' \code{method} \tab a character string indicating what type of equality of covariance test was performed \cr
#' \tab \cr
#' \code{data.name} \tab a character string giving the names of the data
#'}
#'  
#' @family Testing for Structure of Covariance Matrices   
#' @name structureStatistics
#' 
#' @examples Chen2010(as.matrix(iris[1:50, 1:3]))
NULL