#' @title Generate multi-dimensional test data for spline smoothing. 
#' @description Generate a \code{P}-dimensional test data set based on a sigmoid function.
#'
#' @param n Numer of samples
#' @param P Spatial dimension
#' @param split A value between \code{0} and \code{1} for the train / test split.
#' @return A list of the covarite matrices for the train and test data \code{X_train} and \code{X_test} and of the variable of interest \code{y_train} and \code{y_test}.
#'
#' @examples
#' generate_test_data(100, 2)
#'
#' @export
generate_test_data <- function(n, P, split = 0.8){
  
  ### check for valid input parameter
  if( !is.vector(n) | length(n) != 1 | n < 1 ){ cat("Error: n has to be a positive integer \n") ; return(NULL) }
  if( !is.vector(P) | length(P) != 1 | P < 1 ){ cat("Error: P has to be a positive integer \n") ; return(NULL) }
  if( !is.vector(split) | length(split) != 1 | split < 0 | split > 1 ){ cat("Error: split has to be a between o and 1 \n") ; return(NULL) }
  n <- round(n)
  P <- round(P)
  
  
  ### generate data (sigmoid function)
  X <- sapply(1:P, function(p) stats::runif(n,0,1))
  t <- sapply( 1:n, function(i) -16*( (sum(X[i,]^2) / length(X[i,])) -0.5) )
  fx <- 1 / ( 1 + exp(t) )
  y <- fx + stats::rnorm(n, 0, 0.1)
  
  
  ### Train / test split
  index <- sample(1:n, floor(n*split), replace = F)
  X_train <- X[index,]
  X_test <- X[-index,]
  y_train <- y[index]
  y_test <- y[-index]
  return( list("X_train" = X_train, "y_train" = y_train, "X_test" = X_test, "y_test" = y_test) )

}
