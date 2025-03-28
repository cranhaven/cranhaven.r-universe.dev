#' Function to estimate the type cell proportions in an unclassified cytometry data set denoted X_s
#' by using the classification Lab_source from an other cytometry data set X_s. With this function
#' an additional regularization parameter on the class proportions enables a faster computation of the
#' estimator.
#'
#'@param X_s Cytometry data set. The columns correspond to the different biological markers tracked.
#'One line corresponds to the cytometry measurements performed on one cell. The classification
#'of this Cytometry data set must be provided with the Lab_source parameters.
#'
#'@param X_t Cytometry data set. The columns correspond to the different biological markers tracked.
#'One line corresponds to the cytometry measurements performed on one cell. The CytOpT algorithm
#'targets the cell type proportion in this Cytometry data set.
#'
#'@param Lab_source Classification of the X_s Cytometry data set
#'
#'@param eps Regularization parameter of the Wasserstein distance
#'
#'@param theta_true If available, gold-standard proportions in the target data 
#'set \code{X_t} derived from manual gating. It allows to assess the gap between 
#'the estimate and the gold-standard. Default is \code{NULL}, in which case no 
#'assessment is performed.
#'
#'@param step Constant that multiply the step-size policy. Default is \code{5}.
#'
#'@param power the step size policy of the gradient ascent method is step/n^power.
#' Default is \code{0.99}.
#'
#'@param lbd an float constant that multiply the step-size policy. Default is \code{1e-04}.
#'
#'@param n_iter an integer Constant that iterate method select. Default is \code{10000}.
#'
#'@param monitoring boolean indicating whether Kullback-Leibler divergence should be 
#'monitored and store throughout the optimization iterations. Default is \code{FALSE}.
#'
#'@importFrom reticulate import_from_path

#'@export
#'
#'
#'@return A list with the following elements:\code{Results_Minmax}


cytopt_minmax_r <- function(X_s, X_t, Lab_source, theta_true = NULL,
                            eps = 1e-04, lbd = 1e-04, n_iter = 10000,
                            step = 5, power =0.99, monitoring = FALSE){
  
  # READ PYTHON FILES WITH RETICULATE
  python_path <- system.file("python", package = "CytOpT")
  pyCode <- reticulate::import_from_path("CytOpTpy", path = python_path)
  
  
  
  stopifnot(!is.null(X_s))
  stopifnot(!is.null(X_t))
  stopifnot(!is.null(Lab_source))
  
  X_s <- as.matrix(X_s)
  X_t <- as.matrix(X_t)
  Lab_source <- pyCode$minMaxScale$convertArray(Lab_source)
  
  # Preprocessing of the data
  X_s <- X_s * (X_s > 0)
  X_t <- X_t * (X_t > 0)
  
  X_s <- pyCode$minMaxScale$Scale(X_s)
  X_t <- pyCode$minMaxScale$Scale(X_t)
  
  output_minmax <- pyCode$Tools_CytOpt_MinMax_Swapping$cytopt_minmax(X_s, X_t, Lab_source, eps=eps, lbd=lbd, n_iter=n_iter,
                                                                     theta_true=theta_true, step=step, power=power, monitoring=monitoring)
  
  return(output_minmax)
  
}
