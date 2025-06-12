#' Constrained Precision Matrix
#'
#' @description Compute the maximum likelihood estimate, given certain elements are constrained to zero
#' (e.g., an adjacency matrix). This approach is described in \insertCite{hastie2009elements;textual}{IRCcheck}.
#'
#' @param Sigma Covariance matrix
#'
#' @param adj Matrix with constraints. A zero indicates that element
#'            should be constrained to zero.
#'
#' @references
#' \insertAllCited{}
#'
#' @return  A list containing the inverse covariance matrix and the covariance matrix.
#'
#' @note The algorithm is written in \code{c++}.
#'
#' @examples 
#' # random adj 
#' # 90 % sparsity (roughly)
#' p <- 20
#' adj <- matrix(sample(0:1, size = p^2, replace = TRUE, 
#'               prob = c(0.9, 0.1) ), 
#'               nrow = p, ncol = p)
#' 
#' adj <-  symm_mat(adj)
#' 
#' diag(adj) <- 1
#' 
#' # random correlation matrix
#' set.seed(1)
#' cors <- cov2cor(
#'   solve(
#'   rWishart(1, p + 2, diag(p))[,,1])
#' )
#' 
#' # constrain to zero
#' net <- constrained(cors, adj = adj)
#' 
#' @importFrom GGMncv constrained
#' @export
constrained <- function(Sigma, adj){
  # change to zeros
  # adj <- ifelse(adj == 1, 0, 1)
  
  # include diagonal!
  diag(adj) <- 1
  
  # call c++
  fit <- GGMncv::constrained(Sigma, adj)
  
  Theta <- round(fit$Theta, 3)
  Sigma <- round(fit$Sigma, 3)
  wadj <- -(cov2cor(Theta) - diag(ncol(adj)))
  returned_object <- list(Theta = Theta, Sigma = Sigma, wadj = wadj)
  
  return(returned_object)
}
