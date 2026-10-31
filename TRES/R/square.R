#' Square simulated data
#'
#' Synthetic data generated from tensor predictor regression (TPR) model. Each response observation is univariate, and each predictor observation is a matrix.
#'
#' @docType data
#'
#' @usage data("square")
#'
#' @format A list consisting of four components:
#' \describe{
#'  \item{x}{A \eqn{32 \times 32 \times 200} tensor, each matrix \code{x@data[,,i]} represents a predictor observation.}
#'  \item{y}{A \eqn{1 \times 200} matrix, each entry represents a response observation.}
#'  \item{coefficients}{A \eqn{32\times 32 \times 1} tensor with a square pattern.}
#'  \item{Gamma}{A list consisting of two \eqn{32 \times 2} envelope basis.}
#' }
#'
#' @details The dataset is generated from the tensor predictor regression (TPR) model:
#' \deqn{Y_i = B_{(m+1)}vec(X_i) + \epsilon_i, \quad i = 1,\ldots, n,}
#' where \eqn{n=200} and the regression coefficient \eqn{B \in R^{32\times 32}} is a given image with rank 2, which has a square pattern. All the elements of the coefficient matrix \eqn{B} are either 0.1 or 1. To make the model conform to the envelope structure, we construct the envelope basis \eqn{\Gamma_k} and the covariance matrices \eqn{\Sigma_k, k=1,2}, of predictor \eqn{X} as following. With the singular value decomposition of \eqn{B}, namely \eqn{B = \Gamma_1 \Lambda \Gamma_2^T}, we choose the envelope basis as \eqn{\Gamma_k \in R^{32 \times 2}, k=1,2}. Then the envelope dimensions are \eqn{u_1 =  u_2 = 2}. We set matrices \eqn{\Omega_k = I_2}  and  \eqn{\Omega_{0k} = 0.01 I_{30}}, \eqn{k=1,2}. Then we generate the covariance matrices \eqn{\Sigma_k = \Gamma_k \Omega_k \Gamma_k^T + \Gamma_{0k}\Omega_{0k}\Gamma_{0k}^T}, followed by normalization with their Frobenius norms. The predictor \eqn{X_i} is then generated from two-way tensor (matrix) normal distribution \eqn{TN(0; \Sigma_1, \Sigma_2)}. And the error term \eqn{\epsilon_i} is generated from standard normal distribution.
#'
#' @keywords datasets
#' @examples
#' ## Fit square dataset with the tensor predictor regression model
#' data("square")
#' x <- square$x
#' y <- square$y
#' # Model fitting with ordinary least square.
#' fit_std <- TPR.fit(x, y, method="standard")
#' # Draw the coefficient plot.
#' plot(fit_std)
#'
#' @references Zhang, X. and Li, L., 2017. Tensor envelope partial least-squares regression. Technometrics, 59(4), pp.426-436.
"square"
