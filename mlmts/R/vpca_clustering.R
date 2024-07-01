

#' Performs the fuzzy clustering algorithm of He and Tan (2020).
#'
#' \code{vpca_clustering} performs the fuzzy clustering algorithm proposed
#' by \insertCite{he2018unsupervised;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param k The number of clusters.
#' @param m The fuzziness coefficient (a real number greater than one).
#' @param var_rate Rate of retained variability concerning the
#' dimensionality-reduced MTS samples (default is 0.90).
#' @param max_it The maximum number of iterations (default is 1000).
#' @param tol The tolerance (default is 1e-5).
#' @param crisp Logical. If \code{crisp = FALSE} (default) a fuzzy partition
#' is returned. Otherwise, the function returns the corresponding crisp
#' partition, in which each series is placed in the cluster associated
#' with the maximum membership degree.
#' @return A list with three elements:
#' \itemize{
#' \item \code{U}. If \code{crisp = FALSE} (default), the membership matrix. Otherwise,
#' a vector defining the corresponding crisp partition.
#' \item \code{centroids}. If \code{crisp = FALSE} (default), a list containing the
#' series playing the role of centroids, which are dimensionality-reduced averaged MTS. Otherwise, this
#' element is not returned.
#' \item \code{iterations}. The number of iterations before the algorithm
#' stopped.
#' }
#' @examples
#' fuzzy_clustering <- vpca_clustering(AtrialFibrillation$data, k = 3, m = 1.5)
#' # Executing the fuzzy clustering algorithm in the dataset AtrialFibrillation
#' # by considering 3 clusters and a value of 1.5 for the fuziness parameter
#' fuzzy_clustering$U # The membership matrix
#' crisp_clustering <- vpca_clustering(AtrialFibrillation$data, k = 3, m = 1.5, crisp = TRUE)
#' # The same as before, but we are interested in the corresponding crisp partition
#' crisp_clustering$U # The crisp partition
#' crisp_clustering$iterations # The number of iterations before the algorithm
#' # stopped
#' @details
#' This function executes the fuzzy clustering procedure proposed by
#' . The algorithm represents each MTS in the original collection by means of
#' a dimensionality-reduced MTS constructed through variable-based principal
#' component analysis (VPCA). Then, fuzzy \eqn{K}-means-type procedure is considered
#' for the set of dimensionalityu-reduced samples. A spatial weighted matrix
#' dissimilarity is considered to compute the distances between the reduced
#' MTS and the centroids.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{he2018unsupervised}{mlmts}
#'
#' }
#' @seealso
#' \code{\link{vpca_clustering}}
#' @export

vpca_clustering <- function(X, k, m, var_rate = 0.90, max_it = 1000, tol = 1e-5, crisp = FALSE){

  check_mts(X)
  l <- length(X)
  c <- ncol(X[[1]])

  # Initialization

  auxiliary_list <- auxiliary_vpca_function_1(X, var_rate = var_rate)
  Y <- auxiliary_list$Y # Reduced matrices
  p_s <- auxiliary_list$p_s
  S_matrix <- auxiliary_vpca_function_2(N = c, p_s = p_s)

  random_sample <- sample(l, k)
  Z <- Y[random_sample] # Selecting initial centroids


  # Iterations

  distance_matrix <- matrix(0, l, k)
  mem_matrix <- matrix(0, l, k)
  exponent <- -2/(m-1)
  objective_function <- numeric()
  count_iterations <- 1

  for (p in 1 : max_it) {


    # Computation of distance matrix

    for (i in 1 : l) {

      for (j in 1 : k) {

        distance_matrix[i, j] <- auxiliary_vpca_function_3(Y[[i]], Z[[j]], S = S_matrix)

      }

    }

    distance_matrix[distance_matrix == 0] <- 1e-15


    # Computation of membership matrix

    for (i in 1 : l) {

      for (j in 1 : k) {

        numerator <- distance_matrix[i, j]^exponent
        denominator <- sum(distance_matrix[i,]^exponent)
        mem_matrix[i, j] <- numerator/denominator

      }

    }


    # Updating of centroids

    mem_centroids <- mem_matrix^(m)

    for (i in 1 : k) {

      auxiliary_product_vector <- mapply('*', Y, mem_centroids[,i], SIMPLIFY = FALSE)
      numerator <- Reduce('+' ,auxiliary_product_vector)
      denominator <- sum(mem_centroids[,i])
      Z[[i]] <- numerator/denominator

    }


    # Computing the value of the objective function

    matrix_sum <- mem_centroids * distance_matrix^2
    objective_function[count_iterations] <- sum(matrix_sum)


    if (p > 1) {

      if (abs(objective_function[count_iterations] - objective_function[count_iterations - 1]) < tol) {

        if (crisp == FALSE) {

          return_list <- list(U = t(mem_matrix), centroids = Z, iterations = count_iterations)
          return(return_list)

        } else {
          return_list <- list(U = fuzzytocrisp(mem_matrix), iterations = count_iterations)
          return(return_list)

        }

      }

    }


    count_iterations <- count_iterations + 1

  }


  if (crisp == FALSE) {

    return(fuzzytocrisp(mem_matrix))

  } else {

    return_list <- list(U = t(mem_matrix), centroids = Z, iterations = count_iterations)
    return(return_list)

  }







}
