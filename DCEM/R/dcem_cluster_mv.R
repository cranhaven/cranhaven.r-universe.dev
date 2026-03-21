#The main EM routine.
require(mvtnorm)
require(matrixcalc)

#' dcem_cluster (multivariate data): Part of DCEM package.
#'
#' Implements the Expectation Maximization algorithm for multivariate data. This function is called
#' by the dcem_train routine.
#'
#' @param data A matrix: The dataset provided by the user.
#'
#' @param meu (matrix): The matrix containing the initial meu(s).
#'
#' @param sigma (list): A list containing the initial covariance matrices.
#'
#' @param prior (vector): A vector containing the initial prior.
#'
#' @param num_clusters (numeric): The number of clusters specified by the user. Default value is 2.
#'
#' @param iteration_count (numeric): The number of iterations for which the algorithm should run, if the
#' convergence is not achieved then the algorithm stops. Default: 200.
#'
#' @param threshold (numeric): A small value to check for convergence (if the estimated meu are within this
#' specified threshold then the algorithm stops and exit).
#'
#' \strong{Note: Choosing a very small value (0.0000001) for threshold can increase the runtime substantially
#' and the algorithm may not converge. On the other hand, choosing a larger value (0.1)
#' can lead to sub-optimal clustering. Default: 0.00001}.
#'
#' @param num_data (numeric): The total number of observations in the data.
#'
#' @return
#'         A list of objects. This list contains parameters associated with the
#'         Gaussian(s) (posterior probabilities, meu, co-variance and prior)
#'
#'\enumerate{
#'         \item (1) Posterior Probabilities:  \strong{prob} :A matrix of
#'         posterior-probabilities.
#'
#'         \item (2) Meu: \strong{meu}: It is a matrix of meu(s). Each row in
#'         the matrix corresponds to one meu.
#'
#'         \item (3) Sigma: Co-variance matrices: \strong{sigma}
#'
#'         \item (4) prior: \strong{prior}: A vector of prior.
#'
#'         \item (5) Membership: \strong{membership}: A vector of
#'         cluster membership for data.
#'         }
#'
#' @usage
#' dcem_cluster_mv(data, meu, sigma, prior, num_clusters, iteration_count,
#' threshold, num_data)
#'
#' @references
#' Parichit Sharma, Hasan Kurban, Mehmet Dalkilic DCEM: An R package for clustering big data via
#' data-centric modification of Expectation Maximization, SoftwareX, 17, 100944 URL
#' https://doi.org/10.1016/j.softx.2021.100944
#'

dcem_cluster_mv <-
  function(data,
           meu,
           sigma,
           prior,
           num_clusters,
           iteration_count,
           threshold,
           num_data)

  {
    # Initialize the parameters
    counter = 1
    weights <- matrix(0,
                      nrow = num_clusters,
                      ncol = num_data,
                      byrow = TRUE)

    # Get the machine tolerance for checking null values
    # in liklihood matrix
    tolerance <- .Machine$double.eps
    init_attempt = 1

    # Declare a datadrame to store the data membership values.
    membership = data.frame()

    # Checking for empty partitions
    # and re-attempt initialization.
    while(init_attempt < 5){

      # Expectation
      weights = expectation_mv(data,
                               weights,
                               meu,
                               sigma,
                               prior,
                               num_clusters,
                               tolerance)
      part_size = apply(weights, 2, which.max)

      if (length(unique(part_size)) < num_clusters) {
        print(paste("Retrying on empty partition, attempt: ", init_attempt))
        meu <- meu_mv(data, num_clusters)
        init_attempt = init_attempt + 1
      }
      # Break if no empty partitions
      else if (length(unique(part_size)) == num_clusters){
        break
      }
      # Inform user if non-empty clusters could not be
      # found in 5 attempts.
      else{
        cat("The specified number of clusters:", num_clusters, "results in",
            num_clusters - length(unique(part_size)), "empty clusters.",
            "\nThe data may have lesser modalities. Please retry with less number of clusters.\n")
        stop("Exiting...")
      }
    }

    # Repeat till convergence threshold or iteration whichever is earlier.
    while (counter <= iteration_count) {
      # Store the current meu
      old_meu <- meu

      # Initialize the weight matrix
      weights <- matrix(0,
                        nrow = num_clusters,
                        ncol = num_data,
                        byrow = TRUE)
      # Expectation
      weights = expectation_mv(data,
                               weights,
                               meu,
                               sigma,
                               prior,
                               num_clusters,
                               tolerance)

      # Maximisation
      out = maximisation_mv(data, weights, meu, sigma, prior, num_clusters, num_data)
      meu = out$meu
      sigma = out$sigma
      prior = out$prior

      # Find the difference in the old and estimated meu values
      meu_diff <- sqrt(sum((meu - old_meu) ^ 2))

      # Check convergence
      if (!is.na(meu_diff) && round(meu_diff, 4) < threshold) {
        print((paste(
          "Convergence at iteration number: ", counter
        )))
        break
      }

      # Check iterations
      else if (counter == iteration_count) {
        print("Iteration threshold crossed. Stoping the execution.")
        break
      }
      counter = counter + 1
    }

    # Assign clusters to data
    membership = rbind(membership, apply(weights, 2, which.max))
    colnames(membership) <- seq(1:num_data)

    # Prepare output list
    output = list(
      prob = weights,
      'meu' = meu,
      'sigma' = sigma,
      'prior' = prior,
      'count' = counter,
      'membership' = membership
    )
    return(output)
  }
