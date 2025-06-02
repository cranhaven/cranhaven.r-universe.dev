mrf_regression_lsm_optimization <- function(points_in_future, lsmatrix){
  # INPUT
  # points_in_future[1:n] vector: n many values of the time series, for which there
  #                       is an equation from a prediction scheme.
  # lsmatrix[m,n]         Matrix carrying predictive equations associated with a
  #                       specific value of the time series.
  #
  # OUTPUT
  # weights               Array of weights carrying the solution for a matrix
  #                       problem, which was solves with ordinary least squares.
  #
  # Author: QS, 02/2021

  #if(!is.vector(points_in_future)){
  #  message("points_in_future must be of type vector")
  #  return()
  #}
  #if(!is.matrix(lsmatrix)){
  #  message("lsmatrix must be of type matrix")
  #  return()
  #}

  if (!requireNamespace('limSolve', quietly = TRUE)) {
    message(
      "Package limSolve is missing in function regression_lsm_optimization
      No computations are performed.
      Please install the packages which are defined in 'Suggests'"
    )
    return()
  }
  weights = limSolve::Solve(lsmatrix, points_in_future)
  return(weights)
}



