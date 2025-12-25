#' Tests for Homogeneity of Covariance Matrices
#' 
#' 
#' Performs tests for homogeneity of 2 and k covariance matrices.
#' 
#' @param x data as a list of matrices
#' @param ... other options passed to covTest method
#'
#' @return A list with class "htest" containing the following components:
#'
#'\tabular{ll}{
#' \code{statistic} \tab the value of homogeneity of covariance test statistic \cr
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
#' \code{method} \tab a character string indicating what type of homogeneity of covariance test was performed \cr
#' \tab \cr
#' \code{data.name} \tab a character string giving the names of the data
#'}
#' 
#' @family Testing for Homogeneity of Covariance Matrices
#' @name homogeneityStatistics
#' 
#' @examples 
#' irisSpecies <- unique(iris$Species)
#' 
#' iris_ls <- lapply(irisSpecies, 
#'     function(x){as.matrix(iris[iris$Species == x, 1:4])}
#'                  )
#'                  
#' names(iris_ls) <- irisSpecies
#' 
#' Ahmad2017(iris_ls)
NULL