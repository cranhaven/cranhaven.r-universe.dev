#' Computes a classification on the target data
#' 
#' Computes a classification on the target data thanks to the approximation of 
#' the transport plan and the classification of the source data.
#' Transport plan is approximated with the stochastic algorithm.
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
#'@param eps an float value of regularization parameter of the Wasserstein distance. Default is \code{1e-04}
#'
#'@param const an float constant. Default is \code{1e-01}
#'
#'@param n_iter an integer Constant that iterate method select. Default is \code{4000}
#'
#'@param monitoring a logical flag indicating to possibly monitor the gap between the estimated proportions and the manual
#'gold-standard. Default is \code{FALSE}
#'
#'@param minMaxScaler a logical flag indicating to possibly Scaler
#'
#'@param thresholding a logical flag.
#'
#'@return a \code{\link[ggplot2]{ggplot}} object
#'
#'@importFrom reticulate import_from_path
#'@export
#'
#'@return a vector of length \code{nrow(X_t)} with the propagated labels
#'
#'@examples
#'
#'if(interactive()){
#'
#'res <- Label_Prop_sto_r(X_s = HIPC_Stanford_1228_1A, X_t = HIPC_Stanford_1369_1A, 
#'              Lab_source = HIPC_Stanford_1228_1A_labels)
#'
#'}
#'


Label_Prop_sto_r <- function (X_s,X_t,
                              Lab_source,
                              eps=1e-04, 
                              const=1e-01, 
                              n_iter=4000,
                              minMaxScaler=TRUE, 
                              monitoring=TRUE, 
                              thresholding=TRUE){
  
  # check
  stopifnot(is.data.frame(X_s) | is.array(X_s))
  stopifnot(is.data.frame(X_t) | is.array(X_t))
  stopifnot(!is.null(Lab_source))
  stopifnot(is.logical(monitoring))
  
  
  # READ PYTHON FILES WITH RETICULATE
  python_path <- system.file("python", package = "CytOpT")
  pyCode <- reticulate::import_from_path("CytOpTpy", path = python_path)

  if(is.factor(Lab_source)){
    levels_save <- levels(Lab_source)
    Lab_source_fact <- Lab_source
    Lab_source <- as.numeric(Lab_source)
  }
  
  Lab_source <- pyCode$minMaxScale$convertArray(Lab_source)
  
  if(thresholding){
    X_s <- X_s * (X_s> 0)
    X_t <- X_t * (X_t > 0)
  }
  if(minMaxScaler){
    X_s <- pyCode$minMaxScale$Scale(X_s)
    X_t <- pyCode$minMaxScale$Scale(X_t)
  }
  
  I <- nrow(X_s)
  J <- nrow(X_t)
  
  alpha <- 1/I * rep(1,I)
  beta <- 1/J * rep(1,J)
  
  alpha <- pyCode$minMaxScale$convertArray(alpha)
  beta <- pyCode$minMaxScale$convertArray(beta)
  
  u_last <- pyCode$Tools_CytOpt_Descent_Ascent$Robbins_Wass(X_s, X_t,
                                                            alpha=alpha,beta=beta,
                                                            eps=eps, const=const, n_iter=n_iter)
  
  message("Approximating transport plan...")
  t0 <- Sys.time()
  Result_LP <- pyCode$Tools_CytOpt_Descent_Ascent$Label_Prop_sto(Lab_source, u_last,
                                                                 X_s, X_t,
                                                                 alpha, beta, eps)
  elapsed_time <- Sys.time() - t0
  message("Done in ", round(elapsed_time, digits = 1), " ", units(elapsed_time))
  
  if(!is.null(levels_save)){
    Lab_target_hat <- levels_save[Result_LP[[2]]]
  }else{
    Lab_target_hat <- Result_LP[[2]]
  }

  return(Lab_target_hat)
}