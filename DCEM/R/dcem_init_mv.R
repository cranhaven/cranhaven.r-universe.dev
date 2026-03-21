#'meu_mv: Part of DCEM package.
#'
#' Initialize the meus(s) by randomly selecting the samples from the dataset. This is the
#' \strong{default} method for initializing the meu(s).
#'
#' @param data (matrix): The dataset provided by the user.
#' @param num_meu (numeric): The number of meu.
#'
#' @return A matrix containing the selected samples from the dataset.
#'
#' @usage
#' # Randomly seeding the mean(s).
#' meu_mv(data, num_meu)
#'
#' @export

meu_mv <- function(data, num_meu) {
  # Randomly sample meu from the data
  return(data[sample(1:nrow(data), num_meu),])
}

#'meu_mv_impr: Part of DCEM package.
#'
#' Initialize the meu(s) by randomly selecting the samples from the dataset. It uses the proposed
#' implementation from K-means++: The Advantages of Careful Seeding, David Arthur and Sergei
#' Vassilvitskii. URL http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf.
#'
#' @param data (matrix): The dataset provided by the user.
#' @param num_meu (numeric): The number of meu.
#'
#' @return A matrix containing the selected samples from the dataset.
#'
#' @usage
#' # Randomly seeding the meu.
#' meu_mv_impr(data, num_meu)
#'

meu_mv_impr <- function(data, num_meu){

  meu_matrix = matrix()

  # Select the next set of centroids based on weighted probability
    if (num_meu == 1){

      # Select the first centroid randomly
      meu_matrix = meu_mv(data, num_meu)

    } else if(num_meu == 2){

      meu_matrix = meu_mv(data, 1)
      dist = rowSums((data - meu_matrix)^2)
      meu_matrix = rbind(meu_matrix, data[which.max(dist), ])

    } else if(num_meu > 2) {

      # Select the first centroid randomly
      meu_matrix = meu_mv(data, 1)

      # Select the 2nd centroid
      dist = rowSums((data - meu_matrix)^2)
      meu_matrix = rbind(meu_matrix, data[which.max(dist), ])

      # Select the next centroids
      for (k in 3:num_meu){
        dist_matrix = data.frame(ncol = k-1, nrow=nrow(data))

        # Calculate the distance of every point from existing centers
        for(i in 1:nrow(meu_matrix)){
          dist_matrix = cbind(dist_matrix, rowSums((data - meu_matrix[i, ])^2))
        }
        # Store the minimum distance
        dist_matrix = apply(dist_matrix, 1, min)
        dist_matrix = dist_matrix/sum(dist_matrix)
        meu_matrix = rbind(meu_matrix, data[which.max(dist_matrix), ])
      }

    }
  return(meu_matrix)
}

#'sigma_mv: Part of DCEM package.
#'
#' Initializes the co-variance matrices as the identity matrices.
#'
#' @param num_sigma (numeric): Number of covariance matrices.
#' @param numcol (numeric): The number of columns in the dataset.
#'
#' @return
#'         A list of identity matrices. The number of entries in the list
#'         is equal to the input parameter (num_cov).
#'
#' @usage
#' sigma_mv(num_sigma, numcol)
#'


sigma_mv <- function(num_sigma, numcol) {
  i = 1
  sigma_vec = list()
  # Initialze a diagonal matrix as the
  # co-variance matrix.
  while (i <= num_sigma) {
    sigma_vec[[i]] <- diag(numcol)
    i = i + 1
  }
  return(sigma_vec)
}

#' get_priors: Part of DCEM package.
#'
#' Initialize the priors.
#'
#' For example, if the user specify 2 priors then the vector will have 2
#' entries (one for each cluster) where each will be 1/2 or 0.5.
#'
#' @param num_priors (numeric): Number of priors one corresponding to each cluster.
#'
#' @return
#' A vector of uniformly initialized prior values (numeric).
#'
#' @usage
#' get_priors(num_priors)


#Initialize the priors
get_priors <- function(num_priors) {
  # Get uniform priors
  prior_vec = rep(1/num_priors, num_priors)
  return(prior_vec)
}
