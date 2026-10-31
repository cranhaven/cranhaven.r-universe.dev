#' Bat simulated data
#'
#' Synthetic data generated from tensor response regression (TRR) model. Each response observation is a two-dimensional image, and each binary predictor observation takes values 0 and 1, representing two groups.
#'
#' @docType data
#'
#' @usage data("bat")
#'
#' @format A list consisting of four components:
#' \describe{
#'  \item{x}{A \eqn{1 \times 20} matrix, each entry takes values 0 and 1, representing two groups.}
#'  \item{y}{A \eqn{64\times 64\times 20} tensor, each matrix \code{y@data[,,i]} represents an image.}
#'  \item{coeffiicients}{A \eqn{64\times 64 \times 1} tensor with the bat pattern.}
#'  \item{Gamma}{A list consisting of two \eqn{64 \times 14} envelope basis.}
#' }
#'
#' @details The dataset is generated from the tensor response regression (TRR) model:
#' \deqn{Y_i = B X_i + \epsilon_i,  i = 1,\ldots, n,}
#' where \eqn{n=20} and the regression coefficient \eqn{B \in R^{64\times 64}} is a given image with rank 14, representing the mean difference of the response \eqn{Y} between two groups. To make the model conform to the envelope structure, we construct the envelope basis \eqn{\Gamma_k} and the covariance matrices \eqn{\Sigma_k, k=1,2}, of error term as following. With the singular value decomposition of \eqn{B}, namely \eqn{B = \Gamma_1 \Lambda \Gamma_2^T}, we choose the envelope basis as \eqn{\Gamma_k \in R^{64\times 14}, k=1,2}. Then the envelope dimensions are \eqn{u_1 =  u_2 = 14}. We generate another two matrices \eqn{\Omega_k \in R^{14\times 14} = A_k A_k^T} and  \eqn{\Omega_{0k} \in R^{50\times 50} =  A_{0k}A_{0k}^T}, where \eqn{A_k \in R^{14\times 14}} and \eqn{A_{0k} \in R^{50\times 50}} are randomly generated from Uniform(0,1) elementwise. Then we set the covariance matrices \eqn{\Sigma_k = \Gamma_k\Omega_k \Gamma_k^T + \Gamma_{0k}\Omega_{0k} \Gamma_{0k}^T}, followed by normalization with their Frobenius norms. We set the first 10 predictors \eqn{X_i, i=1,\ldots, 10,} as 1 and the rest as 0. The error term is then generated from two-way tensor (matrix) normal distribution \eqn{TN( 0; \Sigma_1, \Sigma_2)}.
#'
#' @keywords datasets
#' @examples
#' ## Fit bat dataset with the tensor response regression model
#' data("bat")
#' x <- bat$x
#' y <- bat$y
#' # Model fitting with ordinary least square.
#' fit_std <- TRR.fit(x, y, method="standard")
#' # Draw the coefficient and p-value plots
#' plot(fit_std)
#'
#' @references  Li, L. and Zhang, X., 2017. Parsimonious tensor response regression. Journal of the American Statistical Association, 112(519), pp.1131-1146.
"bat"

