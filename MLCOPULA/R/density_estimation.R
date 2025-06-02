#' @title Estimation of the density of some distribution
#' @param X Matrix with predictor variables.
#' @param distribution Distribution to be used: normal or kernels, 
#' by default normal.  


density.estimation <- function(X, distribution = 'normal'){
  switch(distribution,
         'normal' = normal.estimation(X),
         'kernel' = kernel.estimation(X)
  )
}