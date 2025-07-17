#' Functional Gaussian Graphical Model
#' 
#' Estimates a sparse adjacency matrix representing the conditional dependency structure between features of a multivariate Gaussian process
#' @param y  list of length p containing densely observed multivariate (p-dimensional) functional data. \code{y[[j]]} is an nxm matrix of functional data for n subjects observed on a grid of length m
#' @param t  (optional) grid on which functional data is observed, defaults to \code{seq(0, 1, m)} where \code{m = dim(data[[1]])[2]}.
#' @param alpha  penalty parameter for the common sparsity pattern taking values in \code{[0,1]}.
#' @param gamma  penalty parameter for the overall sparsity pattern taking positive values. 
#' @param L the number of eigenfunctions used for dimension reduction using the partially separable Karhunen-Loeve (PSKL) expansion obtained using `pfpca()`. This argument can take positive integer values greater or equal to 1. 
#' @param thr.FVE this parameter sets a threshold for the minimum percentage of functional variance explained (FVE) by the PSKL eigenfunctions (obtained using `pfpca()`). This criterion is used only if a value for L is not provided or is greater than the maximum possible number of eigenfunctions estimated from \code{y} using \code{pfpca()}.
#' @param include.Omega  logical variable indicating wheter to include the list of precision matrices in the output. Default value is FALSE.
#' @return A list with letters and numbers.
#' \describe{
#'   \item{\code{A}}{Resulting adjacency matrix as the union of all the Omega matrices}
#'   \item{\code{L}}{number of PSKL expansion eigenfunctions considered for the estimation of the graphical model.}
#'   \item{\code{Omega}}{list of of precision matrices obtained using the multivariate functional principal component scores theta obtained using `fpca()`}
#'}
#' @details 
#' This function implements the functional graphical model in Zapata, Oh, and Petersen (2019).
#' The arguments \code{alpha} and \code{gamma} are a reparameterization of the Group Graphical Lasso tuning parameters when using the \code{JGL} package. When using \code{JGL::JGL}, the tuning parameters are computed as \code{lambda1 = alpha*gamma} and \code{lambda2 = (1-alpha)*gamma}
#'@examples
#' ## Variables
#' # Omega - list of precision matrices, one per eigenfunction
#' # Sigma - list of covariance matrices, one per eigenfunction
#' # theta - list of functional  principal component scores
#' # phi - list of eigenfunctions densely observed on a time grid
#' # y - list containing densely observed multivariate (p-dimensional) functional data 
#' 
#' library(mvtnorm)
#' library(fda)
#' 
#' ## Generate data y
#'  source(system.file("exec", "getOmegaSigma.R", package = "fgm"))
#'  theta = lapply(1:nbasis, function(b) t(rmvnorm(n = 100, sigma = Sigma[[b]])))
#'  theta.reshaped = lapply( 1:p, function(j){
#'      t(sapply(1:nbasis, function(i) theta[[i]][j,]))
#'  })
#'  phi.basis=create.fourier.basis(rangeval=c(0,1), nbasis=21, period=1)
#'  t = seq(0, 1, length.out = time.grid.length)
#'  chosen.basis = c(2, 3, 6, 7, 10, 11, 16, 17, 20, 21)
#'  phi = t(predict(phi.basis, t))[chosen.basis,]
#'  y = lapply(theta.reshaped, function(th) t(th)%*%phi)
#'  
#' ## Solve
#'  fgm(y, alpha=0.5, gamma=0.8)
#'  
#' 
#' @keywords pfpca fpca pca fda partial separability principal components 
#' @export
#' @author Javier Zapata, Sang-Yun Oh and Alexander Petersen
#' @references Zapata J., Oh S. and Petersen A. (2019) - Partial Separability and Functional Graphical Models for Multivariate Gaussian Processes. Available at \url{https://arxiv.org/abs/1910.03134}.


fgm = function(y, L, alpha, gamma, t = seq(0, 1, length.out = dim(y[[1]])[2]), thr.FVE = 95, include.Omega = FALSE){
  
  # checking inputs
  if (missing(y)) stop('y is missing')
  if (!is.list(y)) stop('y must be a list of matrices')
  if (length(t) != dim(y[[1]])[2]) stop('length of time grid does not match dim(y[[1]])[2])')
  if (missing(alpha)) stop('alpha is missing.'); 
  if (missing(gamma)) stop('gamma is missing.');
  
  if (!is.logical(include.Omega)) stop('include.Omega needs to be boolean')
  if (gamma < 0) stop('gamma can only take values greater or equal than 0')
  if (alpha < 0 || alpha > 1) stop('alpha can take values in [0,1] only')
  if (thr.FVE < 0 || thr.FVE > 100) stop('thr.FVE can take values in [0,100] only')
  
  res = pfpca(y, t)
  theta = res$theta
  L.star = res$L
  FVE = res$FVE
  L.FVE = min(which(res$FVE >= thr.FVE)) #by construction: L.FVE<=L.star
  
  # thetaHat.FVE = theta[1:L.FVE]
  if (!missing(L)){
    if (L > L.star){
      #warning('Number of basis requested L=' + str(L)+ ' exceeds the maximum possible ='+ str(L.star)+ '. Setting L based on the thr.FVE')
      warning(paste('Number of basis requested L=' , L, 
                    ' exceeds the maximum possible =', L.star, '. Setting L=', L.FVE,' using thr.FVE=', thr.FVE))
      L = L.FVE  
    }
  } else {
    warning(paste('A number of basis explaining', thr.FVE, '% of the variance were considered to estimate the graphical model'))
    L = L.FVE
  }
  
  Omega<-.JGL.wrapper(x = theta[1:L],gamma,alpha)
  A <- Reduce('+', lapply(Omega, abs))
  A[A != 0] = 1
  
  out<-list(L = L, A = A)
  if (include.Omega) out$Omega = Omega
  return(out)
  
  
}


# myJGL:= Wrapper for JGL to modify output
.JGL.wrapper <- function(x, gamma, alpha, seed = 1, scaleInput = TRUE) {
  JGL::JGL(Y = lapply(x, function(x) scale(t(x), center = FALSE, scale = scaleInput) ),
      penalty = 'group',
      lambda1 = alpha*gamma,
      lambda2 = (1 - alpha)*gamma,
      return.whole.theta = TRUE)$theta
}

















