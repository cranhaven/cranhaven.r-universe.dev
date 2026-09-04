#' Kullback-Leibler Divergence between Centered Multivariate Cauchy Distributions
#'
#' Computes the Kullback-Leibler divergence between two random vectors distributed
#' according to multivariate Cauchy distributions (MCD) with zero location vector.
#'
#' @aliases kldcauchy
#'
#' @usage kldcauchy(Sigma1, Sigma2, eps = 1e-06)
#' @param Sigma1 symmetric, positive-definite matrix. The scatter matrix of the first distribution.
#' @param Sigma2 symmetric, positive-definite matrix. The scatter matrix of the second distribution.
#' @param eps numeric. Precision for the computation of the partial derivative of the Lauricella \eqn{D}-hypergeometric function (see Details). Default: 1e-06.
#' @return A  numeric value: the Kullback-Leibler divergence between the two distributions,
#' with two attributes \code{attr(, "epsilon")} (precision of the partial derivative of the Lauricella \eqn{D}-hypergeometric function,see Details)
#' and \code{attr(, "k")} (number of iterations).
#'
#' @details Given \eqn{X_1}, a random vector of \eqn{\mathbb{R}^p} distributed according to the MCD
#' with parameters \eqn{(0, \Sigma_1)}
#' and \eqn{X_2}, a random vector of \eqn{\mathbb{R}^p} distributed according to the MCD
#' with parameters \eqn{(0, \Sigma_2)}.
#' 
#' Let \eqn{\lambda_1, \dots, \lambda_p} the eigenvalues of the square matrix \eqn{\Sigma_1 \Sigma_2^{-1}}
#' sorted in increasing order: \deqn{\lambda_1 < \dots < \lambda_{p-1} < \lambda_p}
#' Depending on the values of these eigenvalues,
#' the computation of the Kullback-Leibler divergence of \eqn{X_1} from \eqn{X_2}
#' is given by:
#' \itemize{
#' \item if \eqn{\lambda_1 < 1} and \eqn{\lambda_p > 1}:\cr
#' \eqn{ \displaystyle{ KL(X_1||X_2) = -\frac{1}{2} \ln{ \prod_{i=1}^p{\lambda_i}} + \frac{1+p}{2} \bigg( \ln{\lambda_p} } } \cr
#' \eqn{ \displaystyle{ - \frac{\partial}{\partial a} \bigg\{ F_D^{(p)} \bigg( a, \underbrace{\frac{1}{2}, \dots, \frac{1}{2}, a + \frac{1}{2}}_p ; a + \frac{1+p}{2} ; 1 - \frac{\lambda_1}{\lambda_p}, \dots, 1 - \frac{\lambda_{p-1}}{\lambda_p}, 1 - \frac{1}{\lambda_p} \bigg) \bigg\}\bigg|_{a=0} \bigg) } }
#' 
#' \item if \eqn{\lambda_p < 1}:\cr
#' \eqn{ \displaystyle{ KL(X_1||X_2) = -\frac{1}{2} \ln{ \prod_{i=1}^p{\lambda_i}} } }
#' \eqn{ \displaystyle{ - \frac{1+p}{2} \frac{\partial}{\partial a} \bigg\{ F_D^{(p)} \bigg( a, \underbrace{\frac{1}{2}, \dots, \frac{1}{2}}_p ; a + \frac{1+p}{2} ; 1 - \lambda_1, \dots, 1 - \lambda_p \bigg) \bigg\}\bigg|_{a=0} } }
#' 
#' \item if \eqn{\lambda_1 > 1}:\cr
#' \eqn{ \displaystyle{ KL(X_1||X_2) = -\frac{1}{2} \ln{ \prod_{i=1}^p{\lambda_i}} - \frac{1+p}{2} \prod_{i=1}^p\frac{1}{\sqrt{\lambda_i}} } } \cr
#' \eqn{ \displaystyle{ \times \frac{\partial}{\partial a} \bigg\{ F_D^{(p)} \bigg( \frac{1+p}{2}, \underbrace{\frac{1}{2}, \dots, \frac{1}{2}}_p ; a + \frac{1+p}{2} ; 1 - \frac{1}{\lambda_1}, \dots, 1 - \frac{1}{\lambda_p} \bigg) \bigg\}\bigg|_{a=0} } }
#' }
#' 
#' where \eqn{F_D^{(p)}} is the Lauricella \eqn{D}-hypergeometric function defined for \eqn{p} variables:
#' \deqn{ \displaystyle{ F_D^{(p)}\left(a; b_1, ..., b_p; g; x_1, ..., x_p\right) = \sum\limits_{m_1 \geq 0} ... \sum\limits_{m_p \geq 0}{ \frac{ (a)_{m_1+...+m_p}(b_1)_{m_1} ... (b_p)_{m_p} }{ (g)_{m_1+...+m_p} } \frac{x_1^{m_1}}{m_1!} ... \frac{x_p^{m_p}}{m_p!} } } }
#' 
#' <!-- The computation of the partial derivative uses the \code{\link{pochhammer}} function. -->
#' 
#' @author Pierre Santagostini, Nizar Bouhlel
#' @references N. Bouhlel, D. Rousseau, A Generic Formula and Some Special Cases for the Kullback–Leibler Divergence between Central Multivariate Cauchy Distributions.
#' Entropy, 24, 838, July 2022.
#' \doi{10.3390/e24060838}
#'
#' @examples
#' \donttest{
#' Sigma1 <- matrix(c(1, 0.6, 0.2, 0.6, 1, 0.3, 0.2, 0.3, 1), nrow = 3)
#' Sigma2 <- matrix(c(1, 0.3, 0.1, 0.3, 1, 0.4, 0.1, 0.4, 1), nrow = 3)
#' kldcauchy(Sigma1, Sigma2)
#' kldcauchy(Sigma2, Sigma1)
#' 
#' Sigma1 <- matrix(c(0.5, 0, 0, 0, 0.4, 0, 0, 0, 0.3), nrow = 3)
#' Sigma2 <- diag(1, 3)
#' # Case when all eigenvalues of Sigma1 %*% solve(Sigma2) are < 1
#' kldcauchy(Sigma1, Sigma2)
#' # Case when all eigenvalues of Sigma1 %*% solve(Sigma2) are > 1
#' kldcauchy(Sigma2, Sigma1)
#' }
#' 
#' @importFrom utils combn
#' @export

kldcauchy <- function(Sigma1, Sigma2, eps = 1e-06) {
  
  # Sigma1 and Sigma2 must be matrices
  if (is.numeric(Sigma1) & !is.matrix(Sigma1))
    Sigma1 <- matrix(Sigma1)
  if (is.numeric(Sigma2) & !is.matrix(Sigma2))
    Sigma2 <- matrix(Sigma2)
  
  # Number of variables
  p <- nrow(Sigma1)
  
  # Sigma1 and Sigma2 must be square matrices with the same size
  if (ncol(Sigma1) != p | nrow(Sigma2) != p | ncol(Sigma2) != p)
    stop("Sigma1 et Sigma2 must be square matrices with rank p.")
  
  # IS Sigma1 symmetric, positive-definite?
  if (!isSymmetric(Sigma1))
    stop("Sigma1 must be a symmetric, positive-definite matrix.")
  lambda1 <- eigen(Sigma1, only.values = TRUE)$values
  if (any(lambda1 < .Machine$double.eps))
    stop("Sigma1 must be a symmetric, positive-definite matrix.")
  
  # IS Sigma2 symmetric, positive-definite?
  if (!isSymmetric(Sigma2))
    stop("Sigma2 must be a symmetric, positive-definite matrix.")
  lambda2 <- eigen(Sigma2, only.values = TRUE)$values
  if (any(lambda2 < .Machine$double.eps))
    stop("Sigma2 must be a symmetric, positive-definite matrix.")
  
  # Eigenvalues of Sigma1 %*% inv(Sigma2)
  lambda <- sort(eigen(Sigma1 %*% solve(Sigma2), only.values = TRUE)$values, decreasing = FALSE)
  
  # prodlambda <- prod(lambda)
  
  derive <- dlauricella(nu1 = 1, nu2 = 1, lambda = lambda, eps = eps)
  
  result <- as.numeric(derive)
  
  attr(result, "epsilon") <- attr(derive, "epsilon")
  attr(result, "k") <- attr(derive, "k")
  
  return(result)
}
