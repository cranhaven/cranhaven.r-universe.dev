#The main EM routine.
require(mvtnorm)
require(matrixcalc)

#' dcem_cluster_uv (univariate data): Part of DCEM package.
#'
#' Implements the Expectation Maximization algorithm for the univariate data. This function is internally
#' called by the dcem_train routine.
#'
#' @param data (matrix): The dataset provided by the user (converted to matrix format).
#'
#' @param meu (vector): The vector containing the initial meu.
#'
#' @param sigma (vector): The vector containing the initial standard deviation.
#'
#' @param prior (vector): The vector containing the initial prior.
#'
#' @param num_clusters (numeric): The number of clusters specified by the user. Default is 2.
#'
#' @param iteration_count (numeric): The number of iterations for which the algorithm should run. If the
#' convergence is not achieved then the algorithm stops.
#' Default: 200.
#'
#' @param threshold (numeric): A small value to check for convergence (if the estimated meu(s)
#' are within the threshold then the algorithm stops).
#'
#' \strong{Note: Choosing a very small value (0.0000001) for threshold can increase the runtime
#' substantially and the algorithm may not converge. On the other hand, choosing a larger
#' value (0.1) can lead to sub-optimal clustering. Default: 0.00001}.
#'
#' @param num_data (numeric): The total number of observations in the data.
#'
#' @param numcols (numeric): Number of columns in the dataset (After processing the
#' missing values).
#'
#'
#' @return
#'         A list of objects. This list contains parameters associated with the
#'         Gaussian(s) (posterior probabilities, meu, standard-deviation and prior)
#'
#'\enumerate{
#'         \item (1) Posterior Probabilities: \strong{prob}: A matrix of
#'         posterior-probabilities.
#'
#'         \item (2) Meu(s): \strong{meu}: It is a vector of
#'         meu. Each element of the vector corresponds to one meu.
#'
#'         \item (3) Sigma: Standard-deviation(s): \strong{sigma}: A vector of standard
#'         deviation.
#'
#'         \item (4) prior: \strong{prior}: A vector of prior.
#'
#'         \item (5) Membership: \strong{membership}: A vector of
#'         cluster membership for data.
#'         }
#'
#' @usage
#' dcem_cluster_uv(data, meu, sigma, prior, num_clusters, iteration_count,
#' threshold, num_data, numcols)
#'
#' @references
#' Parichit Sharma, Hasan Kurban, Mehmet Dalkilic DCEM: An R package for clustering big data via
#' data-centric modification of Expectation Maximization, SoftwareX, 17, 100944 URL
#' https://doi.org/10.1016/j.softx.2021.100944

dcem_cluster_uv <-

  function(data,
           meu,
           sigma,
           prior,
           num_clusters,
           iteration_count,
           threshold,
           num_data,
           numcols)

  {

    # Initialize parameters
    counter = 1
    weights = matrix(0,
                     nrow = num_clusters,
                     ncol = num_data,
                     byrow = TRUE)
    # Get the machine tolerance for checking null values
    # in liklihood matrix
    tolerance <- .Machine$double.eps
    init_attempt = 1

    # Declare a dataframe to store the data membership values.
    membership = data.frame()

    # Checking for empty partitions
    # and re-attempt initialization.
    while(init_attempt < 5){

      # Expectation
      weights = expectation_uv(data,
                               weights,
                               meu,
                               sigma,
                               prior,
                               num_clusters,
                               tolerance)
      part_size = apply(weights, 2, which.max)

      if (length(unique(part_size)) < num_clusters) {
        print(paste("Retrying on empty partition, attempt: ", init_attempt))
        # Get new set of meu
        meu <- meu_uv(data, num_clusters)
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

    #Repeat till threshold achieved or convergence whichever is earlier.
    while (counter <= iteration_count) {

      # Store the current meu
      old_meu = meu

      # Expectation
      weights = expectation_uv(data, weights, meu, sigma, prior, num_clusters, tolerance)

      # Maximisation
      out = maximisation_uv(data, weights, meu, sigma, prior, num_clusters, num_data)
      meu = out$meu
      sigma = out$sigma
      prior = out$prior

      # Find the difference in the old and estimated meu values
      mean_diff = sum((meu - old_meu) ^ 2)

      # Check convergence
      if (!is.na(mean_diff) && round(mean_diff, 4) <= threshold) {
        print((paste("Convergence at iteration number: ", counter)))
        break
      }

      # Check iterations
      else if (counter == iteration_count) {
        print("Maximum iterations reached. Halting.")
        break
      }
      counter = counter + 1
    }

    # Assign data to clusters
    membership = rbind(membership, apply(weights, 2, which.max))
    colnames(membership) <- seq(1:num_data)

    # Prepare output list
    output = list('prob' = weights,
                  'meu' = as.vector(meu),
                  'sigma' = sigma,
                  'prior' = prior,
                  'membership' = membership
                  )
    return(output)

  }
