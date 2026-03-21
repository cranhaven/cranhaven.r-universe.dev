#' expectation_uv: Part of DCEM package.
#'
#' Calculates the probabilistic weights for the univariate data.
#'
#' @param data (matrix): The input data.
#'
#' @param weights (matrix): The probability weight matrix.
#'
#' @param meu (vector): The vector of meu.
#'
#' @param sigma (vector): The vector of sigma (standard-deviations).
#'
#' @param prior (vector): The vector of priors.
#'
#' @param num_clusters (numeric): The number of clusters.
#'
#' @param tolerance (numeric): The system epsilon value.
#'
#' @return
#'         Updated probability weight matrix.
#'
#' @usage
#' expectation_uv(data, weights, meu, sigma, prior, num_clusters, tolerance)
#'
#' @export

expectation_uv <- function(data, weights, meu, sigma, prior, num_clusters, tolerance){

  # Get the probability density for univariate data
  for (clus in 1:num_clusters) {
    weights[clus, ] <- dnorm(data, mean=meu[clus] , sd=sigma[clus]) * prior[clus]
  }

  # Normalize the probability density matrix
  sum_weights <- colSums(weights)
  weights <- sweep(weights, 2, sum_weights, '/')

  # Replace negligibly small values by machine tolerance
  weights[is.nan(weights)] <- tolerance
  weights[weights <= 0.0] <- tolerance

  return(weights)
}

#' maximisation_uv: Part of DCEM package.
#'
#' Calculates meu, sigma and prior based on the updated probability weight matrix.
#'
#' @param data (matrix): The input data.
#'
#' @param weights (matrix): The probability weight matrix.
#'
#' @param meu (vector): The vector of meu.
#'
#' @param sigma (vector): The vector of sigma (standard-deviations).
#'
#' @param prior (vector): The vector of priors.
#'
#' @param num_clusters (numeric): The number of clusters.
#'
#' @param num_data (numeric): The total number of observations in the data.
#'
#' @return
#'         Updated values for meu, sigma and prior.
#'
#' @usage
#' maximisation_uv(data, weights, meu, sigma, prior, num_clusters, num_data)
#'
#' @export

maximisation_uv <- function(data, weights, meu, sigma, prior, num_clusters, num_data){

  # Maximise the parameters (priors, meu and sigma)
  for (clus in 1:num_clusters) {
    prior[clus] = sum(weights[clus, ]) / num_data
    meu[clus] = (sum(data * weights[clus, ]) / sum(weights[clus, ]))
    sigma[clus] = sqrt(sum(((data - meu[clus]) ^ 2) * weights[clus, ]) / sum(weights[clus, ]) )
  }

  return(list(meu=meu, sigma=sigma, prior=prior))
}

#' expectation_mv: Part of DCEM package.
#'
#' Calculates the probabilistic weights for the multivariate data.
#'
#' @param data (matrix): The input data.
#'
#' @param weights (matrix): The probability weight matrix.
#'
#' @param meu (matrix): The matrix of meu.
#'
#' @param sigma (list): The list of sigma (co-variance matrices).
#'
#' @param prior (vector): The vector of priors.
#'
#' @param num_clusters (numeric): The number of clusters.
#'
#' @param tolerance (numeric): The system epsilon value.
#'
#' @return
#'         Updated probability weight matrix.
#'
#' @usage
#' expectation_mv(data, weights, meu, sigma, prior, num_clusters, tolerance)
#'
#' @export

expectation_mv <- function(data, weights, meu, sigma, prior, num_clusters, tolerance){

  # Get the probability density for multivariate data
  for (clus in 1:num_clusters) {
    weights[clus, ] <- dmvnorm(data, meu[clus, ] , sigma[[clus]]) * prior[clus]
  }

  # Normalize the probability density matrix
  sum_weights <- colSums(weights)
  weights <- sweep(weights, 2, sum_weights, '/')

  # Replace negligibly small values by machine tolerance
  weights[is.nan(weights)] <- tolerance
  weights[weights <= 0.0] <- tolerance

  return(weights)
}

#' maximisation_mv: Part of DCEM package.
#'
#' Calculates meu, sigma and prior based on the updated probability weight matrix.
#'
#' @param data (matrix): The input data.
#'
#' @param weights (matrix): The probability weight matrix.
#'
#' @param meu (matrix): The matrix of meu.
#'
#' @param sigma (list): The list of sigma (co-variance matrices).
#'
#' @param prior (vector): The vector of priors.
#'
#' @param num_clusters (numeric): The number of clusters.
#'
#' @param num_data (numeric): The total number of observations in the data.
#'
#' @return
#'         Updated values for meu, sigma and prior.
#'
#' @usage
#' maximisation_mv(data, weights, meu, sigma, prior, num_clusters, num_data)
#'
#' @export

maximisation_mv <- function(data, weights, meu, sigma, prior, num_clusters, num_data){

  # Maximising the meu and prior
  meu <- weights %*% data
  meu <- meu / rowSums(weights)
  prior <- rowSums(weights) / num_data

  # If covariance matrix is singular
  # then add small value along the diagonals
  # to make it invertible.
  # These situations are not discussed in the original
  # EM article.
  for (clus in 1:num_clusters) {

    sigma[[clus]] = 0
    temp =  data - meu[clus]
    temp = stats::cov.wt(
      temp,
      weights[clus, ],
      method = "ML"
    )$cov

    # Take care of the singularity condition.
    if (matrixcalc::is.singular.matrix(temp)) {
      diag(temp) <- diag(temp) + 0.000000000000001
    }
    sigma[[clus]] <- temp
  }

  return(list("meu"=meu, "sigma"=sigma, 'prior'=prior))
}

#' update_weights: Part of DCEM package.
#'
#' Update the probability values for specific data points that change between the heaps.
#'
#' @param temp_weights (matrix): A matrix of probabilistic weights for leaf data.
#'
#' @param weights (matrix): A matrix of probabilistic weights for all data.
#'
#' @param index_list (vector): A vector of indices.
#'
#' @param num_clusters (numeric): The number of clusters.
#'
#' @return
#'         Updated probabilistic weights matrix.
#'
#' @usage
#' update_weights(temp_weights, weights, index_list, num_clusters)
#'
#' @export


update_weights <- function(temp_weights, weights, index_list, num_clusters){

  # Update the probability density matrix.
  for (clus in 1:num_clusters) {
    weights[clus, index_list] = temp_weights[clus, ]
  }

  return(weights)
}
