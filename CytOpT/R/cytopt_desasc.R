#' Function to estimate the type cell proportions in an unclassified cytometry data set denoted X_s
#' by using the classification Lab_source from an other cytometry data set X_s. With this function
#' the computation of the estimate of the class proportions is done with a descent ascent algorithm.
#'
#'@param X_s a cytometry dataframe. The columns correspond to the different biological markers tracked.
#'One line corresponds to the cytometry measurements performed on one cell. The classification
#'of this Cytometry data set must be provided with the Lab_source parameters.
#'
#'@param X_t a cytometry dataframe. The columns correspond to the different biological markers tracked.
#'One line corresponds to the cytometry measurements performed on one cell. The CytOpT algorithm
#'targets the cell type proportion in this Cytometry data set
#'
#'@param Lab_source a vector of length \code{n} Classification of the X_s cytometry data set
#'
#'@param theta_true If available, gold-standard proportions in the target data 
#'set \code{X_t} derived from manual gating. It allows to assess the gap between 
#'the estimate and the gold-standard. Default is \code{NULL}, in which case no 
#'assessment is performed.
#'
#'@param eps an float value of regularization parameter of the Wasserstein distance. Default is \code{1e-04}.
#'
#'@param n_out an integer number of iterations in the outer loop. This loop corresponds to the gradient
#'descent algorithm to minimize the regularized Wasserstein distance between the source and
#'target data sets. Default is \code{5000}.
#'
#'@param n_stoc an integer number of iterations in the inner loop. This loop corresponds to the stochastic
#'algorithm that approximates a maximizer of the semi-dual problem. Default is \code{10}.
#'
#'@param step_grad an integer number step size of the gradient descent algorithm 
#'of the outer loop. Default is \code{10}.
#'
#'@param monitoring boolean indicating whether Kullback-Leibler divergence should be 
#'monitored and store throughout the optimization iterations. Default is \code{FALSE}.
#'
#'@importFrom reticulate import_from_path

#'@export
#'
#'@return A list with the following elements:\code{h_hat}


cytopt_desasc_r <- function(X_s, X_t, Lab_source, theta_true=NULL,
                            eps=1e-04, n_out=5000, n_stoc=10,
                            step_grad=10, monitoring = FALSE){
  stopifnot(!is.null(X_s))
  stopifnot(!is.null(X_t))
  stopifnot(!is.null(Lab_source))

  # READ PYTHON FILES WITH RETICULATE
  python_path <- system.file("python", package = "CytOpT")
  pyCode <- reticulate::import_from_path("CytOpTpy", path = python_path)


  X_s <- as.matrix(X_s)
  X_t <- as.matrix(X_t)

  Lab_source <- pyCode$minMaxScale$convertArray(Lab_source)

  # Preprocessing of the data
  X_s <- X_s * (X_s > 0)
  X_t <- X_t * (X_t > 0)

  X_s <- pyCode$minMaxScale$Scale(X_s)
  X_t <- pyCode$minMaxScale$Scale(X_t)

  output_desasc <- pyCode$Tools_CytOpt_Descent_Ascent$cytopt_desasc(X_s, X_t, Lab_source=Lab_source, eps=eps, n_out=n_out,
                                                                    n_stoc=n_stoc, step_grad=step_grad, theta_true=theta_true, monitoring=monitoring)

  return(output_desasc)
}
