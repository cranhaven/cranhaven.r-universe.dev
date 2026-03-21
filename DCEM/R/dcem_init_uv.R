#'meu_uv: Part of DCEM package.
#'
#' This function is internally called by the dcem_train to initialize the
#' meu(s). It randomly selects the meu(s) from the
#' range min(data):max(data).
#'
#' @param data (matrix): The dataset provided by the user.
#' @param num_meu (number): The number of meu.
#'
#' @return A vector containing the selected samples from the dataset.
#'
#' @usage
#' # Randomly seeding the meu.
#' meu_uv(data, num_meu)


meu_uv <- function(data, num_meu) {
  # Randomly sample meu from the data
  return(data[sample(1:nrow(data), num_meu), ])
}

#'meu_uv_impr: Part of DCEM package.
#'
#' This function is internally called by the dcem_train to initialize the
#' meu(s). It uses the proposed implementation from
#' K-means++: The Advantages of Careful Seeding, David Arthur and Sergei Vassilvitskii.
#' URL http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf.
#'
#' @param data (matrix): The dataset provided by the user.
#' @param num_meu (number): The number of meu.
#'
#' @return A vector containing the selected samples from the dataset.
#'
#' @usage
#' # Seeding the meu using the K-means++ implementation.
#' meu_uv_impr(data, num_meu)
#'

meu_uv_impr <- function(data, num_meu){

  # Select the next set of centroids based on weighted probability
    if (num_meu == 1){

      # Select the first centroid randomly
      meu_vector = meu_uv(data, num_meu)

    } else if(num_meu == 2){

      # Select the first centroid randomly
      meu_vector = meu_uv(data, 1)
      dist = (meu_vector - data)^2
      dist = dist/sum(dist)

      # Select the next centroid by weighted probability
      meu_vector = c(meu_vector, data[which.max(dist), ])

    } else if(num_meu > 2) {

      # Select the first centroid randomly
      meu_vector = meu_uv(data, 1)

      # Select the next centroid by weighted probability
      dist = (data - meu_vector)^2
      dist = dist/sum(dist)
      meu_vector = c(meu_vector, data[which.max(dist), ])

      # Select the next centroids by weighted probability
      for(k in 3:num_meu){
        dist_matrix = matrix(ncol = k-1, nrow = nrow(data))

        # Calculate the distance of all points from existing centroids
        for(i in 1:length(meu_vector)){
          dist_matrix[, i] = (data - meu_vector[i])^2
        }
        # Store the minimum distance
        dist_matrix = apply(dist_matrix, 1, min)
        dist_matrix = dist_matrix/sum(dist_matrix)
        meu_vector = c(meu_vector, data[which.max(dist_matrix), ])
      }
    }
  return(meu_vector)
}

#'sigma_uv: Part of DCEM package.
#'
#' Initializes the standard deviation for the Gaussian(s).
#'
#' @param data (matrix): The dataset provided by the user.
#' @param num_sigma (number): Number of sigma (standard_deviations).
#'
#' @return
#'         A vector of standard deviation value(s).
#'
#' @usage
#' sigma_uv(data, num_sigma)
#'


sigma_uv <- function(data, num_sigma) {
  # Use the standard deviation of the data
  # as the initial sigma for all the clusters.
  cov_vec = rep(sd(data), num_sigma)
  return(cov_vec)
}
