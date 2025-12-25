#' AR(1) correlation matrix
#'
#' Generate a correlation matrix for AR(1) model
#'
#' @param n size of matrix
#' @param rho correlation between -1 to 1
#' @return \eqn{n\times n} AR(1) correlation matrix
#' @section Details: 
#' The correlation matrix is created as
#'  \deqn{ \left(\begin{array}{ccccc}
#' 1 & \rho & \rho^2 & \cdots & \rho^{n-1}\\
#' \rho & 1 & \rho & \cdots & \rho^{n-2}\\
#' \vdots & \vdots & \vdots & \ddots & \vdots\\ 
#' \rho^2 & \rho & 1 & \cdots & \rho^{n-3}
#' \end{array}\right)}{ (non-Latex version) }
#' @examples
#' AR1.cor(5, 0.5)

AR1.cor = function(n, rho) {
	exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - (1:n - 1))
	rho^exponent
}
