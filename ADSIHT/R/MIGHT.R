#' @title MIGHT: Milti-task iterative graphical hard thresholding
#'
#' @description An implementation of the sparse group selection in joint graphical model.
#'
#' @param X The list of input observation matrices.
#' @param ic.coef A non-negative value used for multiplying the penalty term
#' for choosing the optimal stopping time. Default: \code{ic.coef = 3}.
#' @param ic.scale A non-negative value used for multiplying the penalty term
#' in information criterion. Default: \code{ic.scale = 3}.
#' @param L The length of the sequence of s0. Default: \code{L = 15}.
#' @param coef1 A positive value to control the sub-optimal stopping time.
#' @param coef2 A positive value to control the overall stopping time. A small value leads to larger search range.
#' @param kappa A parameter that controls the rapid of the decrease of threshold. Default is 0.9.
#' @param eta A parameter controls the step size in the gradient descent step.
#' Default: \code{eta = 0.8}.
#' @param center A boolean value indicating whether centralization is required. Default: \code{center = TRUE}.
#' @param scale A positive value to control the column-wise L2 norm of each observation matrix. Default: \code{scale=1}.
#' @param parallel A boolean value indicating whether parallel operation is required. Default: \code{parallel = FALSE}.
#' @param ncpus A positive value that controls the numer of cpus. Default: \code{ncpus = 4}.
#'
#' @return A \code{list} object containing the estimated precision matrices for each dataset.
#'
#' @author Yanhang Zhang, Zhifan Li, Shixiang Liu, Jianxin Yin.
#'
#' @export
#'
#' @examples
#' library(mvnfast)
#' set.seed(1)
#' n = 50; p = 10; K = 4
#' x_list <- lapply(1:K, function(x) rmvn(n, mu=rep(1, p),
#'                                        sigma = toeplitz( (x/2/K)^(1:p-1) ) ) )
#' fit = MIGHT(X=x_list, scale = 10)
#' solve( toeplitz( 0.5^(0:9) ) )
#' fit[[4]]
#'



#### Main function of MIGHT ####
MIGHT <- function(X, ic.coef = 3, ic.scale = 3, L = 15, coef1 = 1, coef2 = 0.1,
                  kappa = 0.9, eta = 0.8, center = TRUE, scale = 1,
                  parallel = FALSE, ncpus = 4) {
  p <- ncol(X[[1]])
  K <- length(X)
  n <- sapply(X, function(x) nrow(x))  # 每个任务的样本量

  if (parallel) {
    snowfall::sfInit(parallel = TRUE, cpus = ncpus)
    snowfall::sfExportAll()

    res <- snowfall::sfLapply(1:p, function(num) {
      MT_DSIHT(num, X = X, n = n, K = K, p = p,
               ic.coef = ic.coef, ic.scale = ic.scale,
               L = L, coef1 = coef1, coef2 = coef2,
               kappa = kappa, eta = eta,
               center = center, scale = scale)
    })

    snowfall::sfStop()
  } else {
    res <- lapply(1:p, function(num) {
      MT_DSIHT(num, X = X, n = n, K = K, p = p,
               ic.coef = ic.coef, ic.scale = ic.scale,
               L = L, coef1 = coef1, coef2 = coef2,
               kappa = kappa, eta = eta,
               center = center, scale = scale)
    })
  }

  res_Omega <- lapply(1:K, function(i) {
    matrix(unlist(purrr::map(res, ~.x[[i]])), ncol = p)
  })

  res_Omega <- lapply(res_Omega, function(A) {
    n <- nrow(A)
    for (i in 2:n) {
      for (j in 1:(i - 1)) {
        temp <- ifelse(abs(A[i, j]) >= abs(A[j, i]), A[j, i], A[i, j])
        A[i, j] <- A[j, i] <- temp
      }
    }
    return(A)
  })

  return(res_Omega)
}


MT_DSIHT <- function(num, X, n, K, p, ic.coef, ic.scale, L,
                     coef1, coef2, kappa, eta, center , scale) {
  y_list <- lapply(X, function(z) z[, num])
  x_list <- lapply(X, function(z) z[, -num])
  fit <- ADSIHT.ML(x_list, y_list, method = "ols",
                   ic.coef = ic.coef, ic.scale = ic.scale,
                   coef1 = coef1, coef2 = coef2,
                   kappa = kappa, eta=eta, L =L,
                   center = center, scale= scale )
  beta <- fit[["beta"]][[which.min(fit[["ic"]])]]
  if( center == TRUE){
    temp <- unlist(y_list) - as.matrix(Matrix::bdiag(x_list)) %*% beta - rep(fit[["intercept"]][[which.min(fit[["ic"]])]], times = n)
  }else{
    temp <- unlist(y_list) - as.matrix(Matrix::bdiag(x_list)) %*% beta
  }

  sigma <- rep(0, K)
  for (i in 1:K) {
    if (i == 1) {
      sigma[i] <- sum(temp[1:n[i]]^2) / n[i]
    } else {
      sigma[i] <- sum(temp[(sum(n[1:(i-1)]) + 1):sum(n[1:i])]^2) / n[i]
    }
  }
  omega <- lapply(1:K, function(i) {
    temp = -1/sigma[i]*beta[((i-1)*(p-1)+1):(i*(p-1))]
    return(append(temp, 1/sigma[i], after = num-1))
  })
  return(omega)
}

