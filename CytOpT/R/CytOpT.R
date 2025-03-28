#' Function to estimate the type cell proportions in an unclassified cytometry data set denoted X_s
#' by using the classification Lab_source from an other cytometry data set X_s. With this function
#' the computation of the estimate of the class proportions is done with a descent ascent or minmax
#' or two algorithms.
#'
#'@param X_s a cytometry dataframe with only \code{d} numerical variables for \code{ns} observations.
#' The columns correspond to the different biological markers measured.
#'One line corresponds to the cytometry measurements performed on one cell. The classification
#'of this Cytometry data set must be provided with the Lab_source parameters.
#'
#'@param X_t a cytometry dataframe with only \code{d} numerical variables for \code{nt} observations.
#'The columns correspond to the different biological markers measured.
#'One line corresponds to the cytometry measurements performed on one cell. The CytOpT algorithm
#'targets the cell type proportion in this Cytometry data set
#'
#'@param Lab_source a vector of length \code{ns} Classification of the X_s cytometry data set
#'@param Lab_target a vector of length \code{nt} Classification of the X_t cytometry data set
#'
#'@param method a character string indicating which method to use to
#'compute the cytopt, either \code{'minmax'}, \code{'desasc'}
#' or  \code{'both'} for comparing both Min-max swapping and descent-ascent procedures.
#'Default is \code{'minmax'}.
#'
#'@param theta_true If available, gold-standard proportions in the target data
#'set \code{X_t} derived from manual gating. It allows to assess the gap between
#'the estimate and the gold-standard. Default is \code{NULL}, in which case no
#'assessment is performed.
#'
#'@param eps a float value of regularization parameter of the Wasserstein distance. Default is \code{1e-04}
#'
#'@param n_iter an integer Constant that iterate method select. Default is \code{10000}
#'
#'@param power a float constant the step size policy of the gradient ascent method is step/n^power. Default is \code{0.99}
#'
#'@param step_grad an integer number step size of the gradient descent algorithm of the outer loop.
#'Default is \code{10}
#'
#'@param step an integer constant that multiply the step-size policy. Default is \code{5}
#'
#'@param lbd a float constant that multiply the step-size policy. Default is \code{1e-04}
#'
#'@param n_out an integer number of iterations in the outer loop. This loop corresponds to the gradient
#'descent algorithm to minimize the regularized Wasserstein distance between the source and
#'target data sets. Default is \code{1000}
#'
#'@param n_stoc an integer number of iterations in the inner loop. This loop corresponds to the stochastic
#'algorithm that approximates a maximizer of the semi dual problem. Default is \code{10}
#'
#'@param monitoring a logical flag indicating to possibly monitor the gap between the estimated proportions and the manual
#'gold-standard. Default is \code{FALSE}.
#'
#'@param minMaxScaler a logical flag indicating to whether to scale observations
#'between 0 and 1. Default is \code{TRUE}.
#'
#'@param thresholding a logical flag indicating whether to threshold negative
#'values.  Default is \code{TRUE}.
#'
#'
#'@return a object of class \code{CytOpt}, which is a list of two elements:\itemize{
#'   \item \code{proportions} a \code{data.frame} with the (optionally true and)
#'   estimated proportions for each \code{method}
#'   \item \code{monitoring} a list of estimates over the optimization iterations
#'   for each \code{method} (listed within)
#' }
#'
#'@importFrom reticulate import_from_path
#'@importFrom stats sd
#'
#'@export
#'
#'@examples
#'if(interactive()){
#'
#'res <- CytOpT(X_s = HIPC_Stanford_1228_1A, X_t = HIPC_Stanford_1369_1A,
#'              Lab_source = HIPC_Stanford_1228_1A_labels,
#'              method='minmax')
#'summary(res)
#'plot(res)
#'
#'}

CytOpT <- function (X_s,
                    X_t,
                    Lab_source,
                    Lab_target = NULL,
                    theta_true = NULL,
                    method = c("minmax", "desasc", "both"),
                    eps=1e-04, n_iter=10000, power=0.99, step_grad=10,
                    step=5, lbd=1e-04, n_out=5000, n_stoc=10,
                    minMaxScaler=TRUE, monitoring=FALSE, thresholding=TRUE){

  # Sanity checks ----
  stopifnot(is.data.frame(X_s) | is.array(X_s))
  stopifnot(is.data.frame(X_t) | is.array(X_t))
  stopifnot(!is.null(Lab_source))
  stopifnot(is.logical(monitoring))

  if(is.data.frame(X_s)){
    message("Converting `X_s` from data.frame to matrix type")
    X_s <- as.matrix(X_s)
  }
  if(is.data.frame(X_t)){
    X_t <- as.matrix(X_t)
    message("Converting `X_t` from data.frame to matrix type")
  }

  if(!is.numeric(Lab_source)){
    if(!is.factor(Lab_source)){
      Lab_source <- as.factor(Lab_source)
    }
    Lab_source_fact <- Lab_source
    Lab_source <- as.numeric(Lab_source)
  }

  if(is.null(theta_true)) {
    message("theta_true is NULL")
    if(!is.null(Lab_target)){
      message("imputing theta_true from Lab_target")
      theta_true <- c(table(Lab_target)/length(Lab_target))
    }else{
      message("No gold-standard available")
      if(monitoring){
        warning("Monitoring is impossible without a gold standard.")
      }
    }
  }

  if(!is.null(theta_true)) {
    if(length(theta_true) != nlevels(Lab_source_fact)){
      stop("Number of cell populations is different between `Lab_source` and `theta_true`!")
    }
    if(!is.null(names(theta_true))){
      if(any(names(theta_true) != levels(Lab_source_fact))){
        stop("Cell population order differ between `Lab_source` and `theta_true`!")
      }
    }
  }

  if(!is.null(Lab_target) & !is.numeric(Lab_target)){
    if(!is.factor(Lab_target)){
      Lab_target <- factor(Lab_target, levels=levels(Lab_source_fact))
    }
    Lab_target_fact <- Lab_target
    Lab_target <- as.numeric(Lab_target)
  }


  # READ PYTHON FILES WITH RETICULATE ----
  python_path <- system.file("python", package = "CytOpT")
  pyCode <- reticulate::import_from_path("CytOpTpy", path = python_path)


  # Preprocessing ----
  Lab_source <- pyCode$minMaxScale$convertArray(Lab_source)
  labSourceUnique <- unique(Lab_source)
  if (length(labSourceUnique) <2){
    warning("length(labSourceUnique) <2")
  }

  if(length(method)>1) method <- method[1]

  if(thresholding){
    X_s <- X_s * (X_s > 0)
    X_t <- X_t * (X_t > 0)
  }
  if(minMaxScaler){
    X_s <- pyCode$minMaxScale$Scale(X_s)
    X_t <- pyCode$minMaxScale$Scale(X_t)
  }

  if(length(method)>1) {
    method <- method[1]
  }
  stopifnot(method %in% c("desasc", "minmax", "both"))


  # Optimization ----

  h <- list()
  monitoring_res <- list()

  if(!is.null(theta_true)){
    h[["Gold_standard"]] <- c(theta_true)
  }

  if(method %in% c("desasc", "both")) {
    message("Running Descent-ascent optimization...")
    t0 <- Sys.time()
    res_desasc <- cytopt_desasc_r(X_s, X_t,Lab_source,
                                  theta_true=theta_true,eps=eps, n_out=n_out,
                                  n_stoc=n_stoc, step_grad=step_grad,
                                  monitoring=monitoring)
    elapsed_time_desac <- Sys.time() - t0
    message("Done in ", round(elapsed_time_desac, digits = 1), " ",
            units(elapsed_time_desac))

    h[["Descent_ascent"]] <- res_desasc[1][[1]]
    monitoring_res[["Descent_ascent"]] <- res_desasc[2][[1]]
  }

  if(method %in% c("minmax", "both")) {
    message("Running MinMax optimization...")
    t0 <- Sys.time()
    res_minmax <-  cytopt_minmax_r(X_s, X_t, Lab_source,
                                   eps=eps, lbd=lbd, n_iter=n_iter,
                                   theta_true=theta_true, step=step,
                                   power=power, monitoring=monitoring)
    elapsed_time_minmax <- Sys.time() - t0
    message("Done in ", round(elapsed_time_minmax, digits = 1), " ",
            units(elapsed_time_minmax))
    h[["MinMax"]] <- res_minmax[1][[1]]
    monitoring_res[["MinMax"]] <- res_minmax[2][[1]]
  }

  # Output ----
  if(is.null(names(h[[1]]))){
    names(h[[1]]) <- levels(Lab_source_fact)
  }
  res <- list("proportions" = do.call(cbind.data.frame, h),
              "monitoring" = if(monitoring){monitoring_res}else{NULL}
  )
  class(res) <- "CytOpt"
  return(res)
}
