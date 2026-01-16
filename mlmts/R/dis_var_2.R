

#' Model-based dissimilarity proposed by Maharaj (1999)
#'
#' \code{dis_var_2} returns a pairwise distance matrix based on testing whether
#' each pair of series are or not generated from the same VARMA model
#' \insertCite{maharaj1999comparison}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param max_p The maximum order considered with respect to the fitting of VAR models.
#' @param criterion The criterion used to determine the VAR order.
#' @return The computed pairwise distance matrix.
#' @examples
#' toy_dataset <- Libras$data[c(1, 2)] # Selecting the first two MTS from the
#' # dataset Libras
#' distance_matrix <- dis_var_2(toy_dataset, max_p = 1) # Computing the pairwise
#' # distance matrix based on the distance dis_var_2
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined
#' as \eqn{1-p}, where \eqn{p} is the \eqn{p}-value of the test of hypothesis proposed
#' by . This test is based on checking the equality of the underlying VARMA models
#' of both series. The VARMA structures are approximated by truncated VAR(\eqn{\infty)}
#' models with a common order \eqn{k = \max{(k_x, k_y)}}, where \eqn{k_x} and \eqn{k_y}
#' are determined by the BIC or AIC criterion. The VAR coefficients are automatically fitted.
#' The dissimilarity between both series is given by \eqn{1-p} because this quantity
#' is expected to take larger values the more different both generating processes are.
#' The procedure is able to compare two dependent MTS.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{maharaj1999comparison}{mlmts}
#'
#' }
#' @seealso
#' \code{\link{dis_var_1}}, \code{\link[TSclust]{diss.AR.MAH}}
#' @export



dis_var_2 <- function(X, max_p = 2, criterion = 'BIC') {

  # check_mts(X)
  l <- length(X)
  p_values_matrix <- matrix(0, nrow = l, ncol = l)

  for (i in 1 : l) {

    if(i > 1) {for (j in 1 : (i - 1)) {

      p_values_matrix[i, j] <- auxiliary_var_2_function(X[[i]], X[[j]], max_p = max_p,
                                                        criterion = criterion)

    }

    }

  }

  return(stats::as.dist(p_values_matrix))

}
