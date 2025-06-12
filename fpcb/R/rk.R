#' kernel function
#' 
#' Computes the Gram matrix of the gaussian kernel over a grid of values and
#' computes its singular value decomposition.
#' 
#' 
#' @param grid grid of points where the kernel function is evaluated.
#' @param sigma is the temperature of the kernel (standard deviation)
#' @param r the dimension of the basis system of the Gran matrix (K). If
#' missing then r is the rank of K.
#' @param tol A tolerance to keep the first d eigenvalues of A. Default =
#' 1e-08.
#' @return \item{grid}{grid of points where the kernel function is evaluated.}
#' \item{K}{Kernel Gram matrix} \item{U}{first r eigenvectors of K using svd.}
#' \item{D}{first r eigenvectors of K using svd.}
#' @author J. Cugliari and N. Hernández
#' @export
#' @examples
#' 
#' grid = seq(0,1,,100)
#' rk(grid, sigma = 1)
#' 
rk <-
function(grid, sigma = 1, r, tol = 1e-8) {
  p <- length(grid)
  K <- exp(- sigma * outer(grid, grid, "-")^2 )
  if(missing(r)) {                     # if r is missing then propose one
    values <- svd(K, nu = 0, nv = 0)$d #no need of eigenvectors here
    r <- length(values[values > tol])
  }
  svd <- svd(K, nu = r, nv = 0)   # svd con nv = 0 es mucho + rapido
  return(list(grid = grid,
              K = K,
              D = diag(svd$d[1:r],nrow=r),
              U = svd$u))
}
