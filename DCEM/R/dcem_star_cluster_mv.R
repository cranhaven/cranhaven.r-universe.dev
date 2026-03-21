#The main EM routine.
require(mvtnorm)
require(matrixcalc)

#' dcem_star_cluster_mv (multivariate data): Part of DCEM package.
#'
#' Implements the EM* algorithm for multivariate data. This function is called
#' by the dcem_star_train routine.
#'
#' @param data (matrix): The dataset provided by the user.
#'
#' @param meu (matrix): The matrix containing the initial meu(s).
#'
#' @param sigma (list): A list containing the initial covariance matrices.
#'
#' @param prior (vector): A vector containing the initial priors.
#'
#' @param num_clusters (numeric): The number of clusters specified by the user. Default value is 2.
#'
#' @param iteration_count (numeric): The number of iterations for which the algorithm should run, if the
#' convergence is not achieved then the algorithm stops and exits. Default: 200.
#'
#' @param num_data (numeric): Number of rows in the dataset.
#'
#' @return
#'         A list of objects. This list contains parameters associated with the
#'         Gaussian(s) (posterior probabilities, meu, co-variance and priors)
#'
#'\enumerate{
#'         \item (1) Posterior Probabilities:  \strong{prob}
#'         A matrix of posterior-probabilities for the points in the dataset.
#'
#'         \item (2) Meu: \strong{meu}: A matrix of meu(s). Each row in
#'         the  matrix corresponds to one meu.
#'
#'         \item (3) Sigma: Co-variance matrices: \strong{sigma}: List of co-variance
#'         matrices.
#'
#'         \item (4) Priors: \strong{prior}: A vector of prior.
#'
#'         \item (5) Membership: \strong{membership}: A vector of cluster membership for data.
#'         }
#'
#' @usage
#' dcem_star_cluster_mv(data, meu, sigma, prior, num_clusters, iteration_count, num_data)
#'
#' @references
#' Parichit Sharma, Hasan Kurban, Mehmet Dalkilic DCEM: An R package for clustering big data via
#' data-centric modification of Expectation Maximization, SoftwareX, 17, 100944 URL
#' https://doi.org/10.1016/j.softx.2021.100944
#'

dcem_star_cluster_mv <-
  function(data,
           meu,
           sigma,
           prior,
           num_clusters,
           iteration_count,
           num_data)

  {
    counter <- 1

    weights <- matrix(0,
                      nrow = num_clusters,
                      ncol = num_data,
                      byrow = TRUE)

    # Create a list of heaps (one heap per cluster, heap is implemneted as a matrix)
    heap_list <- rep(list(), num_clusters)
    index_list <- c()

    old_leaf_values <- c()

    # Get machine tolerance
    tolerance <- .Machine$double.eps
    # Intialization attempts
    init_attempt = 1

    # Declare a dataframe to store the data membership values.
    membership = data.frame()

    # Expectation
    weights = expectation_mv(data, weights, meu, sigma, prior, num_clusters, tolerance)
    heap_index <- apply(weights, 2, which.max)

    # Checking for empty partition.
    while (init_attempt < 5){

    if(length(unique(heap_index)) < num_clusters){
      print(paste("Retrying on empty partition, attempt: ", init_attempt))
      meu = meu_mv(data, num_clusters)

      # Expectation
      weights = expectation_mv(data, weights, meu, sigma, prior, num_clusters, tolerance)
      heap_index <- apply(weights, 2, which.max)
      init_attempt = init_attempt + 1
    }

    # Break if none of the heap is empty
    else if (length(unique(heap_index)) == num_clusters){
      #print("Empty partition fixed.")
      break
    }
    # Inform user if non-empty clusters could not be
    # found in 5 attempts.
    else if (init_attempt==5){
      cat("The specified number of clusters:", num_clusters, "results in",
          num_clusters - length(unique(heap_index)), "empty clusters.",
          "\nThe data may have lesser modalities. Please retry or specify lesser number of clusters.\n")
      stop("Exiting...")
    }

    }

    # Get the max probability for each data point
    data_prob <- apply(weights, 2, max)

    # Store the cluster membership for data in cluster_map
    cluster_map <- heap_index

    # Maximisation
    out = maximisation_mv(data, weights, meu, sigma, prior, num_clusters, num_data)
    meu = out$meu
    sigma = out$sigma
    prior = out$prior

    for (clus in 1:num_clusters) {

      # Put the data in the matrix (data belonging to their own clusters)
      ind <- which(heap_index == clus)
      temp_matrix <- matrix(data_prob[ind])
      temp_matrix <- cbind(temp_matrix, ind)

      heap_list[[clus]] <- temp_matrix
      #print(paste("heap: ", clus, "size: ", nrow(heap_list[[clus]])))

      # Build the heap from matrices
      temp_out <- build_heap(heap_list[[clus]])
      heap_list[[clus]] <- split(temp_out, 1:nrow(temp_out))
    }

    # Seperate the leaf and non-leaf nodes
    out = separate_data(heap_list, num_clusters)
    heap_list <- out[[1]]
    index_list <- unlist(out[[2]])

    # Store the current leaf nodes for comparison with the new leaves
    old_leaf_values <- c(old_leaf_values, index_list)

    # Repeat till convergence threshold or iteration which-ever is earlier.
    while (counter <= iteration_count) {

      new_leaf_values <- c()

      temp_weights <- matrix(0,
                             nrow = num_clusters,
                             ncol = length(index_list),
                             byrow = TRUE)

      # Expectation only for leaf nodes (not for all data - save time and computation)
      temp_weights = expectation_mv(data[index_list, ], temp_weights, meu, sigma, prior, num_clusters, tolerance)
      # Update the weights for leaf nodes only
      weights = update_weights(temp_weights, weights, index_list, num_clusters)

      # Expectation
      sum_weights <- colSums(weights)
      weights <- sweep(weights, 2, sum_weights, '/')
      weights[is.nan(weights)] <- tolerance
      weights[weights <= 0.0] <- tolerance

      # Maximisation
      out = maximisation_mv(data, weights, meu, sigma, prior, num_clusters, num_data)
      meu = out$meu
      sigma = out$sigma
      prior = out$prior

      leaves_ind <- index_list

      # Re-assign the leaf nodes based on their estimated probabilities
      # Only leaf nodes are moved.
      if (length(leaves_ind) == 1){
        new_heap_assign_for_leaves <-which.max(temp_weights)
        new_liklihood_for_leaves <- max(temp_weights)
      }
      else{
        new_heap_assign_for_leaves <- unlist(apply(temp_weights, 2, which.max))
        new_liklihood_for_leaves <- unlist(apply(temp_weights, 2, max))
      }

      # Insert into new heap
      heap_list <- insert_nodes(heap_list, new_heap_assign_for_leaves, new_liklihood_for_leaves, leaves_ind, num_clusters)
      #print(paste("Inserting done: ", length(leaves_ind)))

      out = separate_data(heap_list, num_clusters)
      heap_list <- out[[1]]
      index_list <- out[[2]]

      # Putting all leaf nodes together to re-assign later
      new_leaf_values <- c(new_leaf_values, index_list)

      # Check convergence (if old and new leaves are same across heaps)
      if (round( (length(setdiff(old_leaf_values, new_leaf_values)) / length(new_leaf_values)), 4) <= 0.01) {
        print(paste("Convergence at iteration", counter))
        break
      }

      # Check iterations
      else if (counter == iteration_count) {
        print("Maximum iterations reached. Halting.")
        break
      }

      # Put new leaves into the old leaves variable
      old_leaf_values <- new_leaf_values
      index_list <- new_leaf_values
      counter <- counter + 1

    }

    # Assign clusters to data
    membership = rbind(membership, apply(weights, 2, which.max))
    colnames(membership) <- seq(1:num_data)

    # Prepare the output list
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
