#' @title High-dimensional spline smoothing using a matrix-free CG-method.
#' @description Fits a smooth spline to a set of given observations using penalized splines with curvature or difference penalty and multiple covariates. The underlying linear system is solved with a matrix-free conjugated gradient (CG) method.
#'
#' @param m Vector of non-negative integers. Each entry gives the number of inner knots for the respective covariate.
#' @param q Vector of positive integers. Each entry gives the spline degree for the respective covariate.
#' @param lambda Positive number as weight for the penalty term.
#' @param X Matrix containing the covariates as columns and the units as rows.
#' @param y Vector of length \code{nrow(X)} as the variable of interest.
#' @param pen_type Utilized penalization method. Either \code{"curve"} for the curvature penalty or \code{"diff"} for the difference penalty. Defaults to \code{"curve"}.
#' @param l Positive integer vector of length \code{P} indicating for the penalty degree. Only required if \code{pen_type = "diff"}.
#' @param alpha_start Vector of length \code{prod(m+q+1)} as starting value for the CG-method. Defaults to zero.
#' @param K_max Positive integer as upper bound for the number of CG-iterations. Defaults to \code{prod(m+q+1)}.
#' @param tolerance Positive number as error tolerance for the stopping criterion of the CG-method. Defaults to \code{1e-6}.
#' @param print_error Logical, indicating if the iteration error should be printed or not.
#' @return Returns a list containing the input \code{m}, \code{q}, and \code{Omega}. Further gives the fitted spline coefficients \code{alpha}, the fitted values \code{fitted_values}, the residuals \code{residuals}, the root mean squared error \code{rmse} and the R-squared value \code{R_squared}.  
#'
#' @examples
#' data <- generate_test_data(100, 2)
#' X <- data$X_train
#' y <- data$y_train
#' CG_smooth(m = c(7,7), q = c(3,3), lambda = 0.1, X = X, y = y)
#'
#' @export
CG_smooth <- function(m, q, lambda, X, y, pen_type = "curve", l = NULL, alpha_start = NULL, K_max = NULL, tolerance = 1e-6, print_error = TRUE){
  
  ### check for valid input parameter
  if( !is.vector(m) | any(m<0) ){ cat("Error: m has to be an integer vector \n") ; return(NULL) }
  if( !is.vector(q) | any(q<1) ){ cat("Error: q has to be a positive integer vector \n") ; return(NULL) }
  if( length(m) != length(q) ){ cat("Error: m and q have to be of same length \n") ; return(NULL) }
  if( !is.vector(lambda) | length(lambda) > 1 | lambda <= 0 ){ cat("Error: lambda has to be a number greater than 0 \n") ; return(NULL) }
  if( !is.matrix(X) ){ cat("Error: X has to be a matrix \n") ; return(NULL) }
  if( ncol(X) != length(m) ){ cat("Error: number of columns of X differs to the length of m \n") ; return(NULL) }
  if( !is.vector(y) ){ cat("Error: y has to be a vector \n") ; return(NULL) }
  if( nrow(X) != length(y) ){ cat("Error: number of rows of X differs to the length of y \n") ; return(NULL) }
  if( !pen_type %in% c("curve", "diff") ){ cat("Error: pen_type has to be 'curve' or 'diff' \n") ; return(NULL) }
  if( pen_type == "diff" ){ 
    if( !is.null(l) & ( !is.vector(l) | any(l<1) ) ){ cat("Error: has to be a positive integer vector \n") ; return(NULL) }
    if( !is.null(l) & length(l) != length(q) ){ cat("Error: l has to be of same length as q \n") ; return(NULL) }
  }
  if( !is.null(alpha_start) & !is.vector(alpha_start) ){ cat("Error: alpha_start has to be a vector \n") ; return(NULL) }
  if( !is.null(K_max) ){
    if( !is.vector(K_max) | length(K_max) != 1 | K_max <= 0 ){ cat("Error: K_max has to be an integer greater than 0 \n") ; return(NULL) }
  }
  if( !is.vector(tolerance) | length(tolerance) != 1 | tolerance <= 0 ){ cat("Error: tolerance has to be greater than 0 \n") ; return(NULL) }
  if( !is.logical(print_error) ){ cat("Error: print_error has to be logical \n") ; return(NULL) }
  
  
  ### spline setup
  m <- round(m)
  q <- round(q)
  P <- length(m)
  Omega <- lapply(1:P, function(p) c( floor(min(X[,p])), ceiling(max(X[,p])) ))
  J <- m+q+1                                  # number of directional basis functions
  K <- prod(J)                                # total number of basis functions
  if( !is.null(alpha_start) & length(alpha_start) != K  ){ cat(paste0("Error: alpha_start has to be of length ",K," \n")) ; return(NULL) }
  tPhi_list <- lapply(1:P, function(p) t( bspline_matrix(X[, p], m[p], q[p] ,Omega[[p]]) ) )     # spline matrices
  if(pen_type == "curve"){   # curvature penalty
    Psi_list <- curvature_penalty(m, q, Omega)
  }
  if(pen_type == "diff"){   # difference penalty
    if( is.null(l) ){ l <- rep(2,P) }
    if( !is.null(l) ){ l <- round(l) }
    Psi_list <- lapply(1:P, function(p) crossprod( diff(diag(J[p]), diff=l[p]) ) )
  }
  b <- MVP_khatrirao_rcpp(tPhi_list, y)       # right-hand side vector
  norm_b <- sqrt(sum(b^2))
  
  
  ### CG algorithm 
  if( !is.null(alpha_start) ){
    alpha <- alpha_start
    Aalpha <- MVP_spline(tPhi_list, alpha) + lambda*MVP_penalty(Psi_list, alpha, pen_type = pen_type)
    r <- b-Aalpha
  } else{
    alpha <- rep(0, K)
    r <- b
  }
  d <- r
  r_square <- crossprod(r)
  if( is.null(K_max) ){ K_max <- K }
  if( !is.null(K_max) ){ K_max <- min(K, round(K_max)) }
  if(print_error){
    cat("iter  |  error norm", "\n")
    cat(0, "  |  " , sqrt(r_square) / norm_b, "\n")
  }
  for(i in 1:K_max){                              # loop of CG-iteration
    
    Ad <- MVP_spline(tPhi_list, d) + lambda*MVP_penalty(Psi_list, d, pen_type = pen_type)
    t <- as.numeric( r_square / crossprod(d, Ad) )
    alpha <- alpha+t*d
    r_new <- r-t*Ad
    r_new_square <- crossprod(r_new)
    if(print_error){ cat(i, "  |  " , sqrt(r_new_square) / norm_b, "\n") }
    if(sqrt(r_new_square) / norm_b <= tolerance){
      break
    }
    beta <- as.numeric( r_new_square / r_square )   
    r <- r_new                                      
    r_square <- r_new_square                        
    d <- r+beta*d                 
    
  }

  
  ### Output
  results <- list()
  results$m <- m
  results$q <- q
  results$Omega <- Omega
  results$coefficients <- alpha
  results$fitted_values <- MVP_khatrirao_trans_rcpp(tPhi_list, alpha)
  results$residuals <- y - results$fitted_values
  results$rmse <- sqrt(mean(results$residuals^2))
  results$R_squared <- 1 - ( sum(results$residuals^2) / sum( (y-mean(y))^2 ) )
  return(results)
  
}


#' @title High-dimensional spline smoothing using a matrix-free PCG-method.
#' @description Fits a smooth spline to a set of given observations using penalized splines with curvature or difference penalty and multiple covariates. The underlying linear system is solved with a matrix-free preconditioned conjugated gradient (PCG) method using a diagonal preconditioner.
#'
#' @param m Vector of non-negative integers. Each entry gives the number of inner knots for the respective covariate.
#' @param q Vector of positive integers. Each entry gives the spline degree for the respective covariate.
#' @param lambda Positive number as weight for the penalty term.
#' @param X Matrix containing the covariates as columns and the units as rows.
#' @param y Vector of length \code{nrow(X)} as the variable of interest.
#' @param pen_type Utilized penalization method. Either \code{"curve"} for the curvature penalty or \code{"diff"} for the difference penalty. Defaults to \code{"curve"}.
#' @param l Positive integer vector of length \code{P} indicating for the penalty degree. Only required if \code{pen_type = "diff"}.
#' @param alpha_start Vector of length \code{prod(m+q+1)} as starting value for the PCG-method. Defaults to zero.
#' @param K_max Positive integer as upper bound for the number of PCG-iterations. Defaults to \code{prod(m+q+1)}.
#' @param tolerance Positive number as error tolerance for the stopping criterion of the PCG-method. Defaults to \code{1e-6}.
#' @param print_error Logical, indicating if the iteration error should be printed or not.
#' @return Returns a list containing the input \code{m}, \code{q}, and \code{Omega}. Further gives the fitted spline coefficients \code{alpha}, the fitted values \code{fitted_values}, the residuals \code{residuals}, the root mean squared error \code{rmse} and the R-squared value \code{R_squared}.  
#'
#' @examples
#' data <- generate_test_data(100, 2)
#' X <- data$X_train
#' y <- data$y_train
#' PCG_smooth(m = c(7,7), q = c(3,3), lambda = 0.1, X = X, y = y)
#'
#' @export
PCG_smooth <- function(m, q, lambda, X, y, pen_type = "curve", l = NULL, alpha_start = NULL, K_max = NULL, tolerance = 1e-6, print_error = TRUE){
  
  ### check for valid input parameter
  if( !is.vector(m) | any(m<0) ){ cat("Error: m has to be an integer vector \n") ; return(NULL) }
  if( !is.vector(q) | any(q<1) ){ cat("Error: q has to be a positive integer vector \n") ; return(NULL) }
  if( length(m) != length(q) ){ cat("Error: m and q have to be of same length \n") ; return(NULL) }
  if( !is.vector(lambda) | length(lambda) > 1 | lambda <= 0 ){ cat("Error: lambda has to be a number greater than 0 \n") ; return(NULL) }
  if( !is.matrix(X) ){ cat("Error: X has to be a matrix \n") ; return(NULL) }
  if( ncol(X) != length(m) ){ cat("Error: number of columns of X differs to the length of m \n") ; return(NULL) }
  if( !is.vector(y) ){ cat("Error: y has to be a vector \n") ; return(NULL) }
  if( nrow(X) != length(y) ){ cat("Error: number of rows of X differs to the length of y \n") ; return(NULL) }
  if( !pen_type %in% c("curve", "diff") ){ cat("Error: pen_type has to be 'curve' or 'diff' \n") ; return(NULL) }
  if( pen_type == "diff" ){ 
    if( !is.null(l) & ( !is.vector(l) | any(l<1) ) ){ cat("Error: has to be a positive integer vector \n") ; return(NULL) }
    if( !is.null(l) & length(l) != length(q) ){ cat("Error: l has to be of same length as q \n") ; return(NULL) }
  }
  if( !is.null(alpha_start) & !is.vector(alpha_start) ){ cat("Error: alpha_start has to be a vector \n") ; return(NULL) }
  if( !is.null(K_max) ){
    if( !is.vector(K_max) | length(K_max) != 1 | K_max <= 0 ){ cat("Error: K_max has to be an integer greater than 0 \n") ; return(NULL) }
  }
  if( !is.vector(tolerance) | length(tolerance) != 1 | tolerance <= 0 ){ cat("Error: tolerance has to be greater than 0 \n") ; return(NULL) }
  if( !is.logical(print_error) ){ cat("Error: print_error has to be logical \n") ; return(NULL) }
  
  
  ### spline setup
  m <- round(m)
  q <- round(q)
  P <- length(m)
  Omega <- lapply(1:P, function(p) c( floor(min(X[,p])), ceiling(max(X[,p])) ))
  J <- m+q+1                                  # number of directionla basis functions
  K <- prod(J)                                # total number of basis functions
  tPhi_list <- lapply(1:P, function(p) t( bspline_matrix(X[,p], m[p], q[p] ,Omega[[p]]) ) )     # spline matrices
  if( pen_type == "curve" ){
    Psi_list <- curvature_penalty(m, q, Omega)  # curvature penalty
  }
  if( pen_type == "diff" ){
    if( is.null(l) ){ l <- rep(2,P) }
    if( !is.null(l) ){ l <- round(l) }
    Psi_list <- lapply(1:P, function(p) crossprod( diff(diag(J[p]), diff=l[p]) ) )
  }
  b <- MVP_khatrirao_rcpp(tPhi_list, y)       # right-hand side vector
  norm_b <- sqrt(sum(b^2))

  
  ### diagonal preconditioner
  diag_spline <- diag_khatrirao_rcpp(tPhi_list)
  if( pen_type == "curve" ){
    diag_pen <- rowSums(sapply( 1:length(Psi_list), function(j) diag_kronecker_rcpp(Psi_list[[j]]) ) )
  }
  if( pen_type == "diff" ){
    Pj <- length(Psi_list)
    Jj <- sapply(1:Pj, function(p) dim(Psi_list[[p]])[2] )
    Kj <- prod(Jj)
    n_left <- c(1, sapply(1:(Pj-1), function(p) prod(Jj[1:p]) ))
    n_right <- c(rev( sapply(1:(Pj-1), function(p) prod(rev(Jj)[1:p]) ) ), 1)
    if(Pj==1) n_left <- n_right <- 1
    diag_pen <- rowSums( sapply(1:Pj, function(p)  rep(1,n_left[p]) %x% diag(Psi_list[[p]]) %x% rep(1,n_right[p]) ) )
  }
  diag_inv <- 1 / (diag_spline + lambda*diag_pen )
  
  ### PCG algorithm
  if( !is.null(alpha_start) ){
    alpha <- alpha_start
    Aalpha <- MVP_spline(tPhi_list, alpha) + lambda*MVP_penalty(Psi_list, alpha, pen_type = pen_type)
    r <- b-Aalpha
  } else{
    alpha <- rep(0, K)
    r <- b
  }
  d <- r
  z <- diag_inv*r
  d <- z
  rz <- as.numeric( crossprod(r,z) )
  if( is.null(K_max) ){ K_max <- K }
  if( !is.null(K_max) ){ K_max <- min(K, round(K_max)) }
  if(print_error){
    cat("iter  |  error norm", "\n")
    cat(0, "  |  ", sqrt(sum(r^2)) / norm_b, "\n")
  }
  for(i in 1:K_max){         # loop of the PCG iteration
    
    Ad <- MVP_spline(tPhi_list, d) + lambda*MVP_penalty(Psi_list, d, pen_type = pen_type)
    t <- as.numeric( rz / crossprod(d, Ad) )
    alpha <- alpha+t*d
    rz_old <- rz
    r <- r-t*Ad
    if(print_error){ cat(i, "  |  ", sqrt(sum(r^2)) / norm_b, "\n") }
    if(sqrt(sum(r^2)) / norm_b <= tolerance){
      break
    }
    z <- diag_inv*r
    rz <- crossprod(r,z)
    beta <-  as.numeric( rz / rz_old )
    d <- z + beta*d
    
  }
  
  
  ### output
  results <- list()
  results$m <- m
  results$q <- q
  results$Omega <- Omega
  results$coefficients <- alpha
  results$fitted_values <- MVP_khatrirao_trans_rcpp(tPhi_list, alpha)
  results$residuals <- y - results$fitted_values
  results$rmse <- sqrt(mean(results$residuals^2))
  results$R_squared <- 1 - ( sum(results$residuals^2) / sum( (y-mean(y))^2 ) )
  return(results)
  
}


#' @title High-dimensional spline smoothing using a matrix-free multigrid preconditioned CG-method.
#' @description Fits a smooth spline to a set of given observations using penalized splines with curvature penalty and multiple covariates. The underlying linear system is solved with a matrix-free preconditioned conjugated gradient method using a geometric multigrid method as preconditioner.
#'
#' @param G Positive integer greater than one for the maximum number of grids.
#' @param q Vector of positive integers. Each entry gives the spline degree for the respective covariate.
#' @param lambda Positive number as weight for the penalty term.
#' @param X Matrix containing the covariates as columns and the units as rows.
#' @param y Vector of length \code{nrow(X)} as the variable of interest.
#' @param w Damping factor of the Jacobi smoother. Defaults to \code{0.1}.
#' @param nu Two-dimensional vector of non-negative integers. Gives the number of pre- and post-smoothing steps in the multigrid algorithm.
#' @param alpha_start Vector of length \code{prod(m+q+1)} as starting value for the MGCG-method. Defaults to zero.
#' @param K_max Positive integer as upper bound for the number of MGCG-iterations. Defaults to \code{prod(m+q+1)}.
#' @param tolerance Positive number as error tolerance for the stopping criterion of the MGCG-method. Defaults to \code{1e-6}.
#' @param print_error Logical, indicating if the iteration error should be printed or not.
#' @param coarse_grid_solver Utilized coarse grid solver. Either \code{"PCG"} for diagonal preconditioned CG or \code{"Cholesky"} for Cholesky decomposition. Defaults to \code{"Cholesky"}.
#' @return Returns a list containing the input \code{m = 2^G-1}, \code{q}, and \code{Omega}. Further gives the fitted spline coefficients \code{alpha}, the fitted values \code{fitted_values}, the residuals \code{residuals}, the root mean squared error \code{rmse} and the R-squared value \code{R_squared}.  
#'
#' @references Siebenborn, M. and Wagner, J. (2019) A Multigrid Preconditioner for Tensor Product Spline Smoothing. arXiv:1901.00654
#'
#' @examples
#' data <- generate_test_data(100, 2)
#' X <- data$X_train
#' y <- data$y_train
#' MGCG_smooth(G = 3, q = c(3,3), lambda = 0.1, w = 0.8, X = X, y = y)
#'
#' @importFrom Matrix Matrix KhatriRao t
#' @export
MGCG_smooth <- function(G, q, lambda, X, y, w = 0.1, nu = c(3,1), alpha_start = NULL, K_max = NULL, tolerance = 1e-6, print_error = TRUE, coarse_grid_solver = "Cholesky"){
  
  ### check for valid input parameter
  if( !is.vector(G) | length(G) > 1 | G < 2  ){ cat("Error: G has to be a an integer greater than 1 - For G=1 use PCG_smooth instead \n") ; return(NULL) }
  if( !is.vector(q) ){ cat("Error: q has to be a vector \n") ; return(NULL) }
  if( !is.vector(lambda) | length(lambda) > 1 | lambda <= 0 ){ cat("Error: lambda has to be a number greater than 0 \n") ; return(NULL) }
  if( !is.matrix(X) ){ cat("Error: X has to be a matrix \n") ; return(NULL) }
  if( ncol(X) != length(q) ){ cat("Error: number of columns of X differs to the length of q \n") ; return(NULL) }
  if( !is.vector(y) ){ cat("Error: y has to be a vector \n") ; return(NULL) }
  if( nrow(X) != length(y) ){ cat("Error: number of rows of X differs to the length of y \n") ; return(NULL) }
  if( !is.vector(w) | length(w) > 1 | w <= 0 | w > 1  ){ cat("Error: w has to be number between 0 and 1 \n") ; return(NULL) }
  if( !is.vector(nu) | length(nu) != 2 | any(nu<0) ){ cat("Error: nu has to be a two-dimensional integer vector \n") ; return(NULL) }
    if( !is.null(alpha_start) & !is.vector(alpha_start) ){ cat("Error: alpha_start has to be a vector \n") ; return(NULL) }
  if( !is.null(K_max) ){
    if( !is.vector(K_max) | length(K_max) != 1 | K_max <= 0 ){ cat("Error: K_max has to be an integer greater than 0 \n") ; return(NULL) }
  }
  if( !is.vector(tolerance) | length(tolerance) != 1 | tolerance <= 0 ){ cat("Error: tolerance has to be greater than 0 \n") ; return(NULL) }
  if( !is.logical(print_error) ){ cat("Error: print_error has to be logical \n") ; return(NULL) }
  if( !coarse_grid_solver %in% c("Cholesky", "PCG") ){ cat("Error: coarse_grid_solver has to be 'Cholesky' or 'PCG' \n") ; return(NULL) }
  
  
  ### spline setup
  q <- round(q)
  nu <- round(nu)
  P <- length(q)
  m <- lapply( 1:G, function(g) rep(2^g-1,P) )
  Omega <- lapply(1:P, function(p) c( floor(min(X[,p])), ceiling(max(X[,p])) ))
  J <- lapply(1:G, function(g) m[[g]]+q+1)        # number of directionla basis functions
  K <- prod(J[[G]])                               # total number of basis functions
  tPhi_list <- lapply(1:G, function(g) lapply(1:P, function(p) t( bspline_matrix(X[,p], m[[g]][p], q[p] ,Omega[[p]]) ) ) )    # spline matrices
  Psi_list <- lapply(1:G, function(g)  curvature_penalty(m[[g]], q, Omega) )   # curvature penalty
  b <- MVP_khatrirao_rcpp(tPhi_list[[G]], y)      # right-hand side vector
  norm_b <- sqrt(sum(b^2))
  
  
  ### multigrid setup
  Prol <- lapply( 2:G, function(g) lapply( 1:P, function(p) prolongation_matrix(J[[g-1]][p],J[[g]][p],q[p]) ) )     # prolongation matrices
  Rest <- lapply( 2:G, function(g) lapply( 1:P, function(p) restriction_matrix(J[[g]][p],J[[g-1]][p],q[p]) ) )      # restriction matrices

  
  ### Cholesky for coarse grid solver if P not too large
  U_chol <- F
  if(coarse_grid_solver == "Cholesky"){
    tryCatch( {
      if(P==1){
        A_coarse <- tcrossprod((tPhi_list[[1]][[1]])) + lambda*Psi_list[[1]][[1]][[1]] #Reduce("+",lapply(1:length(Psi_list[[1]]), function(j) Psi_list[[1]][[j]][[1]]))
      } else{
        tPhi_list_sparse_1 <- lapply(1:length(tPhi_list[[1]]), function(i) Matrix::Matrix(tPhi_list[[1]][[i]], sparse = TRUE) )
        tPhi_sparse <- Reduce(Matrix::KhatriRao, tPhi_list_sparse_1)
        #TODO: Remove this line
        #A_coarse <- tPhi_sparse %*% Matrix::t(tPhi_sparse) + lambda*Matrix::Matrix( Reduce("+", lapply( 1:length(Psi_list[[1]]), function(j) rTensor::kronecker_list(Psi_list[[1]][[j]]) ) ), sparse = TRUE )
        A_coarse <- tPhi_sparse %*% Matrix::t(tPhi_sparse) + lambda*Matrix::Matrix( Reduce("+", lapply( 1:length(Psi_list[[1]]), function(j) Reduce("%x%", Psi_list[[1]][[j]]) ) ), sparse = TRUE )      
      }
      U_chol <- chol(A_coarse)
    }, error = function(e){
      cat("Warning: Cholesky decomposition not possible - PCG is used as coarse grid solver instead \n")
    } )
  }

  ### MGCG algorithm
  if( !is.null(alpha_start) ){
    alpha <- alpha_start
    Aalpha <- MVP_spline(tPhi_list, alpha) + lambda*MVP_penalty(Psi_list, alpha)
    r <- b-Aalpha
  } else{
    alpha <- rep(0, K)
    r <- b
  }
  d <- r
  if(print_error){
    cat("iter  |  error norm", "\n")
    cat(0, "  |  ", sqrt(sum(r^2)) / norm_b, "\n")
  }
  z <- v_cycle(tPhi_list, Psi_list, Rest, Prol, lambda, b, nu, U_chol, w, alpha)    # apply MG as preconditioner
  d <- z
  rz <- as.numeric( crossprod(r,z) )
  if(is.null(K_max)){
    K_max <- K
  }
  for(i in 1:K_max){         # loop of the MGCG iteration
    
    Ad <- MVP_spline(tPhi_list[[G]], d) + lambda*MVP_penalty(Psi_list[[G]], d)
    t <- as.numeric( rz / crossprod(d, Ad) )
    alpha <- alpha+t*d
    rz_old <- rz
    r <- r-t*Ad
    if(print_error){ cat(i, "  |  ", sqrt(sum(r^2)) / norm_b, "\n") }
    if(sqrt(sum(r^2)) / norm_b <= tolerance){
      break
    }
    z <- v_cycle(tPhi_list, Psi_list, Rest, Prol, lambda, r, nu, U_chol, w)     # apply MG as preconditioner
    rz <- crossprod(r,z)
    beta <-  as.numeric( rz / rz_old )
    d <- z + beta*d
    
  }
  
  
  ### output
  results <- list()
  results$m <- m[[G]]
  results$q <- q
  results$Omega <- Omega
  results$coefficients <- alpha
  results$fitted_values <- MVP_khatrirao_trans_rcpp(tPhi_list[[G]], alpha)
  results$residuals <- y - results$fitted_values
  results$rmse <- sqrt(mean(results$residuals^2))
  results$R_squared <- 1 - ( sum(results$residuals^2) / sum( (y-mean(y))^2 ) )
  return(results)
  
}


#' @title Predictions from model
#' @description Makes predictions of new observations from a fitted spline model.
#'
#' @param model_smooth A spline model resulting from \code{CG_smooth}, \code{PCG_smooth}, or \code{MGCG_smooth}.
#' @param X Matrix containing the new observations.
#' @return Vector of length \code{nrow(X)} of predictions.
#' 
#' @examples
#' data <- generate_test_data(100, 2)
#' X <- data$X_train
#' y <- data$y_train
#' result <- PCG_smooth(m = c(7,7), q = c(3,3), lambda = 0.1, X = X, y = y, print_error = FALSE)
#' X_test <- data$X_test
#' predict_smooth(model_smooth = result, X = X_test)
#'
#' @export
predict_smooth <- function(model_smooth, X){
  
  ### spline setup
  m <- model_smooth$m
  q <- model_smooth$q
  P <- length(q)
  tPhi_list <- lapply(1:P, function(p) t( bspline_matrix(X[,p], m[p], q[p] ,model_smooth$Omega[[p]]) ) )     # spline matrices
  predicts <- MVP_khatrirao_trans_rcpp(tPhi_list, model_smooth$coefficients)
  return(predicts)
  
}


#' @title Trace estimation of the hat matrix.
#' @description Estimates the trace of the (unknown) hat-matrix by stochastic estimation in a matrix-free manner.
#'
#' @param m Vector of non-negative integers. Each entry gives the number of inner knots for the respective covariate.
#' @param q Vector of positive integers. Each entry gives the spline degree for the respective covariate.
#' @param lambda Positive number as weight for the penalty term.
#' @param X Matrix containing the covariates as columns and the units as rows.
#' @param pen_type Utilized penalization method. Either \code{"curve"} for the curvature penalty or \code{"diff"} for the difference penalty. Defaults to \code{"curve"}.
#' @param l Positive integer vector of length \code{P} indicating for the penalty degree. Only required if \code{pen_type = "diff"}.
#' @param n_random Positive integer for the number of random vectors in the trace estimate. Defaults to \code{5}.
#' @return An estimate of the trace of the hat-matrix.
#' 
#' @examples
#' data <- generate_test_data(100, 2)
#' X <- data$X_train
#' estimate_trace(m = c(7,7), q = c(2,2), lambda = 0.1, X = X)
#'
#' @export
estimate_trace <- function(m, q, lambda, X, pen_type = "curve", l = NULL, n_random = 5){
  
  ### check for valid input parameter
  if( !is.vector(m) | any(m<0) ){ cat("Error: m has to be an integer vector \n") ; return(NULL) }
  if( !is.vector(q) | any(q<1) ){ cat("Error: q has to be a positive integer vector \n") ; return(NULL) }
  if( length(m) != length(q) ){ cat("Error: m and q have to be of same length \n") ; return(NULL) }
  if( !is.vector(lambda) | length(lambda) > 1 | lambda <= 0 ){ cat("Error: lambda has to be a number greater than 0 \n") ; return(NULL) }
  if( !is.matrix(X) ){ cat("Error: X has to be a matrix \n") ; return(NULL) }
  if( ncol(X) != length(m) ){ cat("Error: number of columns of X differs to the length of m \n") ; return(NULL) }
  if( !pen_type %in% c("curve", "diff") ){ cat("Error: pen_type has to be 'curve' or 'diff' \n") ; return(NULL) }
  if( pen_type == "diff" ){ 
    if( !is.null(l) & ( !is.vector(l) | any(l<1) ) ){ cat("Error: has to be a positive integer vector \n") ; return(NULL) }
    if( !is.null(l) & length(l) != length(q) ){ cat("Error: l has to be of same length as q \n") ; return(NULL) }
  }
  if( !is.vector(n_random) | length(n_random) != 1 | n_random < 1 ){ cat("Error: n_random has to be a positive integer \n") ; return(NULL)  }
  
  ### spline setup
  m <- round(m)
  q <- round(q)
  n_random <- round(n_random)
  P <- length(m)
  Omega <- lapply(1:P, function(p) c( floor(min(X[,p])), ceiling(max(X[,p])) ))
  J <- m+q+1                                  # number of directionla basis functions
  K <- prod(J)                                # total number of basis functions
  tPhi_list <- lapply(1:P, function(p) t( bspline_matrix(X[, p], m[p], q[p] ,Omega[[p]]) ) )     # spline matrices
  if(pen_type == "curve"){
    Psi_list <- curvature_penalty(m, q, Omega)  # curvature penalty
  }
  if(pen_type == "diff"){
    if(is.null(l)){ l <- rep(2,P) }
    Psi_list <- lapply(1:P, function(p) crossprod( diff(diag(J[p]), diff=l[p]) ) )
  }
  
  
  ### Trace estimation
  V <- sapply(1:n_random, function(i) sample(c(-1,1), K, replace=TRUE) )
  V1 <- sapply(1:n_random, function(i) lambda*MVP_penalty(Psi_list, V[,i], pen_type = pen_type) )
  V2 <- sapply(1:n_random, function(i) solve_PCG(tPhi_list, Psi_list, lambda, V1[,i], pen_type = pen_type) )
  trace <- K - mean( sapply(1:n_random, function(i) crossprod(V[,i],V2[,i])) )
  return(trace)
  
}
