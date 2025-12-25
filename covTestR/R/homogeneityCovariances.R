#' Test Wrapper for Homogeneity of Covariance Matrices
#' 
#' Performs 2 and k sample homogeneity of covariance matrices test using test, 
#' 'covTest.'
#' 
#' @param x data as a data frame, list of matrices, grouped data frame, or resample object
#' @param ... other options passed to covTest method
#' @param covTest homogeneity of covariance matrices test method
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
#' @details The \code{\link{homogeneityCovariances}} function is a wrapper function that formats the data 
#'   for the specific \code{covTest} functions.
#'
#' @export
#' @family Testing for Homogeneity of Covariance Matrices
#'
#' @examples homogeneityCovariances(iris, group = Species)
homogeneityCovariances <- function(x, ..., covTest = BoxesM){
  UseMethod("homogeneityCovariances")
}

#' @export
#' @keywords internal
#' @importFrom rlang enexprs enexpr eval_tidy invoke list2
#' @importFrom purrr map
homogeneityCovariances.data.frame <- function(x, group, ..., covTest = BoxesM){
  dots <- enexprs(...)
  group <- enexpr(group)

  x <- map(split(x, eval_tidy(group, x)), function(y){
    
    as.matrix(y[-which(names(y) == paste(group))], ncol = length(names(y)) - 1)
  })
  
  ls <- list2(x, group, dots)
   mat <- rlang::invoke(covTest, ls)
   mat
}

#' @export
#' @keywords internal
#' @importFrom rlang enexprs invoke list2
#' @importFrom purrr map
homogeneityCovariances.grouped_df <- function(x, ..., covTest = BoxesM){
 
  dots <- enexprs(...)
  
  group <- attributes(x)$vars
  
  x <- map(split(x, attributes(x)$labels), function(y){
    
    as.matrix(y[-which(names(y) == paste(group))], ncol = length(names(y)) - 1)
  })
  
  ls <- list2(x, group = group, dots)
  mat <- rlang::invoke(covTest, ls)
  mat
}


#' @export
#' @keywords internal
#' @importFrom rlang invoke list2 enexprs
homogeneityCovariances.list <- function(x, ..., covTest = BoxesM){
  dots <- enexprs(...)
  matrix_ls <- x
  ls <- list2(x, dots)
    mat <- rlang::invoke(covTest, ls)
    mat
}
