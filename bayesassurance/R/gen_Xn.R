#' Design Matrix Generator
#'
#' Constructs design matrix using given sample size(s).
#' Used for assurance analysis in the Bayesian setting.
#' @param n vector of sample sizes. Length of `n` corresponds
#' to the number of groups being assessed in the study design
#' as well as the column dimension of the design matrix.
#' @return Xn: a design matrix that can be used to assess the
#' Bayesian assurance through Monte Carlo sampling using
#' functions presented in this package.
#' @seealso \code{\link{gen_Xn_longitudinal}}
#'
#' @examples 
#' ## In the following example, notice that passing in a vector
#' ## of length 4 returns a design matrix of column dimension 4, where
#' ## each column is comprised of ones vectors with lengths that correspond
#' ## to the inputted sample sizes.
#'
#' n <- c(1,3,5,8)
#' gen_Xn(n = n)
#' @export
#'
gen_Xn <- function(n){
  p <- length(n)
  Xn <- matrix(0, nrow=sum(n), ncol=p)
  for(i in 1:p){
    ones <- rep(1, n[i])
    if(i == 1){
      Xn[1:n[i], i] <- ones
    }else{
      row_begin <- sum(n[1:i-1])
      row_end <- sum(n[1:i])
      Xn[(row_begin+1):(row_end), i] <- ones
    }
  }
  return(Xn)
}
