#' Macroevolutionary predictions
#'
#' \code{macro_pred} Macroevolutionary predictions
#'
#' \code{macro_pred} Gives a vector of macroevolutionary predictions for each
#' species based on the other species given the phylogeny and a phylogenetic
#' variance matrix.
#'
#' @param y vector of species means
#' @param V phylogenetic variance matrix, must have same order as \code{y}
#' @param useLFO excludes the focal species when calculating the corresponding
#'   species' mean. The correct way is to use TRUE, but in practice it has
#'   little effect and FALSE will speed up the model fit.
#' @return \code{macro_pred} returns a of macroevolutionary predictions at the
#' tips.
#' @author Geir H. Bolstad
#' @examples
#' # Trait values
#' y <- rnorm(3)
#'
#' # A variance matrix (the diagonal must be the same order as y).
#' V <- matrix(c(1.0, 0.5, 0.2, 0.5, 1.0, 0.4, 0.2, 0.4, 1.0), ncol = 3)
#'
#' # Macroevolutionary predictions (output in the same order as y).
#' macro_pred(y, V)
#' @importFrom Matrix Matrix solve diag
#' @export
macro_pred <- function(y, V, useLFO = TRUE) {
  if (useLFO) {
    # here the mean is calculated leaving out the focal species
    X <- matrix(rep(1, length(y) - 1), ncol = 1) # design matrix
    y_mean <- 
        as.matrix(sapply(as.vector(y), 
                         function(x) {
                             j <- which(as.vector(y) == x)
                             GLS(y = as.vector(y)[-j], 
                                 X = X, R = V[-j, -j], 
                                 coef_only = TRUE)$coef
                             }
                         )
                  )
  } else {
    # not leaving out the focal species
    X <- matrix(rep(1, length(y)), ncol = 1)
    y_mean <- GLS(y = y, X = X, R = V, coef_only = TRUE)$coef
  }
  Vinv <- Matrix::solve(V, sparse = TRUE)
  inv_dVinv <- Matrix::Matrix(diag(x = 1 / Matrix::diag(Vinv)), sparse = TRUE)
  return(y_mean + (V - inv_dVinv) %*% Vinv %*% (y - y_mean))
}
