#' Test Wrapper for Structure of a Covariance Matrices
#' 
#' Performs a structure of a covariance matrix test.
#'
#' @param x data
#' @param Sigma Population covariance matrix
#' @param ... other options passed to covTest method
#' @param covTest structure of covariance matrix test method
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
#' @details The \code{\link{structureCovariances}} function is a wrapper function that formats the data 
#'   for the specific \code{covTest} functions.
#' @family Testing for Structure of Covariance Matrices  
#' @export
#'
structureCovariances <- function(x, Sigma = "identity", ..., covTest = Nagao1973){
  UseMethod("structureCovariances")
}

#' @export
#' @keywords internal
#' @importFrom rlang enexprs invoke list2
#' @importFrom purrr map
structureCovariances.data.frame <- function(x, Sigma = "identity", group, ..., covTest = Nagao1973){
  
  dots <- enexprs(...)
  group <- enexpr(group)
  
  x <- map(split(x, eval_tidy(group, x)), function(y){
    
    as.matrix(y[-which(names(y) == paste(group))], ncol = length(names(y)) - 1)
  })
  
  
  lapply(x, function(y){
    call <- list2(y, Sigma = Sigma, group = group, dots)
    mat <- invoke(covTest, call)
    mat
  })
}

#' @export
#' @keywords internal
#' @importFrom rlang enexprs invoke list2
#' @importFrom purrr map
structureCovariances.grouped_df <- function(x, Sigma = "identity", ..., covTest = Nagao1973){
  
  dots <- enexprs(...)
  
  group <- attributes(x)$vars
  
  x <- map(split(x, attributes(x)$labels), function(y){
    
    as.matrix(y[-which(names(y) == paste(group))], ncol = length(names(y)) - 1)
  })
  
  lapply(x, function(y){
    call <- list2(y, Sigma = Sigma, group = group, dots)
    mat <- rlang::invoke(covTest, call)
    mat
  })
 

}

