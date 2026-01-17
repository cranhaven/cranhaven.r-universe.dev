# toPES
#
# Converts a posterior probablity adjacency matrix into a posterior probability
# edge state matrix. It only outputs the posterior for the edges in the
# adjacency matrix used as input.
#
# @param pm A posterior probability adjacency matrix.
#
# @param adjMatrix An adjacency matrix indicating which edges will be converted
# to the posterior probability edge state matrix. To consider all possible
# edges use a fully connected adjacency matrix as the input.
#
# @return A matrix with three columns, one for each edge state, and the number
# of rows will be the number of edges in the adjacency matrix used as the
# input.
#
toPES <- function (pm,
                   adjMatrix) {

  # Get the position of the edges present in the adjacency matrix.
  position <- coordinates(adjMatrix)

  # Extract the number of rows and columns in the probability adjacency matrix.
  # This should be a square matrix.
  dims <- dim(pm)

  # The output from partition and order MCMC is not a numeric matrix. Convert
  # the input to a numeric matrix.
  npm <- matrix(as.numeric(pm),
                nrow = dims[1],
                ncol = dims[2])

  # Create an empty matrix to hold the posterior for each edge.
  PES <- matrix(nrow = dim(position)[2],
                ncol = 3)

  # Name the columns according to the edge state.
  colnames(PES) <- c('zero', 'one', 'two')

  # Loop through each edge and only keep the values that correspond to the edges
  # present in the adjacency matrix.
  for(e in 1:dim(position)[2]) {

    # Extract the posterior for the state 'zero'.
    upper <- npm[position[1, e], position[2, e]]

    # Extract the posterior for the state 'one'.
    lower <- npm[position[2, e], position[1, e]]

    # Fill in the posterior probability of the three edge states for the current
    # edge.
    PES[e, ] <- c(upper, lower, 1 - (upper + lower))

  }

  return (PES)

}

# innerMSE
#
# Calculates either the edge-wise or whole graph MSE. If more than one data set
# is input the mean and standard deviation of either the edge-wise or whole
# graph mse will be calculated.
#
# @param posterior A list of baycn objects or posterior probability adjacency
# matrices. For example, the output from the ep() function from the structmcmc
# package and the output from the edges.posterior() funciton from the BiDAG
# package are posterior probability adjacency matrices.
#
# @param expected The expected edge state probabilities for each edge. This
# must be a matrix with the expected probability for the three edge states
# across the columns and each edge down the rows.
#
# @param adjMatrix An adjacency matrix indicating which edges will be converted
# to the posterior probability edge state matrix. To consider all possible
# edges use a fully connected adjacency matrix as the input.
#
# @param type A character string indicating the type of MSE to be calculated.
# The two options are 'emse' - edge-wise MSE or 'gmse' - whole graph MSE.
#
innerMSE <- function (posterior,
                      expected,
                      adjMatrix,
                      type) {

  # Extract the number of lists in the top level of posterior. First, check if
  # the eth element in the posterior list is a single matrix. This step needs
  # to be done because the length function will give the number of elements in
  # the matrix if there is only one matrix in the first element of posterior.
  # For example, if posterior[[1]] is *one* posterior probability adjacency
  # matrix then the length function would give the number of elements in the
  # matrix instead of one (for *one* matrix).
  M <- ifelse(inherits(posterior, 'list'), length(posterior), 1)

  # Determine if the object(s) in posterior are of class 'baycn' depending on
  # whether M = 1.
  is_baycn <- ifelse(M == 1,
                     inherits(posterior, 'baycn'),
                     inherits(posterior[[1]], 'baycn'))


  # Extract the number of edges in the network
  nEdges <- dim(expected)[1]

  # Create a list for the squared error for each element in posterior.
  sqd_error <- vector(mode = 'list',
                      length = M)

  # Create a list for the edge-wise MSE for each edge in the network. Each
  # element of this list will be a matrix of eMSEs.
  emse <- vector(mode = 'list',
                 length = M)

  # Loop through each element of the posterior list and calculate the eMSE.
  for (e in 1:M) {

    # Determine if the current object is from baycn.
    if (is_baycn) {

      # If the posterior object only has one element it doesn't need to be
      # subsetted to calculate the squared error.
      if (M == 1) {

        # Calculate the squared error for each edge in the network.
        sqd_error[[e]] <- (expected - posterior@posteriorES[, 2:4])^2

        # If the posterior object has more than one element it needs to be
        # subsetted in order to calculate the squared error.
      } else {

        # Calculate the squared error for each edge in the network.
        sqd_error[[e]] <- (expected - posterior[[e]]@posteriorES[, 2:4])^2

      }

    } else {

      # If the posterior object only has one element it doesn't need to be
      # subsetted in order to convert it to a posterior edge state matrix.
      if (M == 1) {

        # Convert the probability adjacency matrix to an edge state matrix.
        pamToES <- toPES(pm = posterior,
                         adjMatrix = adjMatrix)

        # If the posterior object only has more than one element it needs to be
        # subsetted in order to convert it to a posterior edge state matrix.
      } else {

        # Convert the probability adjacency matrix to an edge state matrix.
        pamToES <- toPES(pm = posterior[[e]],
                         adjMatrix = adjMatrix)

      }

      # Calcualte the squared error for each edge state matrix.
      sqd_error[[e]] <- as.matrix((expected - pamToES)^2)

    }

    # Create a matrix to hold the edge-wise MSE for each edge in the network.
    emse[[e]] <- matrix(nrow = nEdges,
                        ncol = 1)

    for (v in 1:nEdges) {

      # Calculate the eMSE for each edge by averaging the MSE across the three
      # edge states for each edge.
      emse[[e]][v, ] <- mean(as.numeric(sqd_error[[e]][v, ]))

    }

  }

  # Create a character vector with mean and sd as the elements. This will be
  # used to name the emse or gmse matrices when M > 1.
  mean_sd <- c('mean', 'sd')

  # Create a character vector with eMSE as the element. This will be used to
  # name the emse or gmse matrices when M = 1.
  emse_char <- 'eMSE'

  # Check if M is greater than one. This will determine the structure of the
  # matrix that is output for either emse or gmse.
  if (M > 1) {

    # Calculate the mean and standard deviation of the eMSE for each edge.
    mean_emse <- apply(simplify2array(emse), 1, mean)
    sd_emse <- apply(simplify2array(emse), 1, sd)

  }

  switch(type,

         # edge-wise mse ------------------------

         'emse' = {

           if (M > 1) {

             # Create a matrix with nEdges rows and N*2 columns to hold the mean
             # and standard deviation of the MSE for each edge in the network.
             emse_mat <- matrix(0,
                                nrow = nEdges,
                                ncol = 2)

             # Name the columns of emse_mat.
             colnames(emse_mat) <- mean_sd

             # Fill in the emse matrix with the mean and sd of the eMSE for each
             # edge in the network.
             emse_mat[, 1] <- mean_emse
             emse_mat[, 2] <- sd_emse

             # If only one data set was analyzed return the MSE for each edge.
           } else {

             # Create a matrix with nEdges rows (for the edge-wise MSE) and N
             # columns.
             emse_mat <- matrix(0,
                                nrow = nEdges,
                                ncol = 1)

             # Name the column of emse_mat.
             colnames(emse_mat) <- emse_char

             # Fill in the eMSE for each edge in the network.
             emse_mat[, 1] <- emse[[1]]

           }

           return (emse_mat)

         },

         # graph-wise mse -----------------------

         'gmse' = {

           if (M > 1) {

             # Create a matrix with one row (for the whole graph MSE) and N * 2
             # columns. The columns will contain the mean and standard deviation
             # of the whole graph MSE for the output from baycn for the
             # different combinations of N, ss, and topology used as input.
             gmse_mat <- matrix(1,
                                nrow = 1,
                                ncol = 2)

             # Fill in the gmse matrix with the mean and sd of the MSE of the
             # whole graph.
             gmse_mat[, 1:2] <- c(mean(unlist(emse)),
                                  sd(unlist(emse)))

             # If only one data set was analyzed return the MSE for the whole
             # graph.
           } else {

             # Create a matrix with one row (for the whole graph MSE) and N
             # columns. The columns will contain the whole graph MSE for the
             # output from baycn for the different combinations of N, ss, and
             # topology used as input.
             gmse_mat <- matrix(1,
                                nrow = 1,
                                ncol = 1)

             # Calculate the whole graph MSE.
             gmse_mat[1, 1] <- mean(emse[[1]])

           }

           return (gmse_mat)

         })

}

#' mse
#'
#' Calculates either the edge-wise or whole graph MSE. If more than one data set
#' is input the mean and standard deviation of the MSE will be calculated.
#'
#' @param posterior A list of baycn objects or posterior probability adjacency
#' matrices. Each element in posterior can also be a list. For example, if
#' multiple data sets were generated under the same simulation scheme and baycn
#' was run on each data set, the input list could be a list containing the
#' output from each run of baycn. In this scenario the mean and standard
#' deviation of of the edgewise or whole graph MSE would be output.
#'
#' @param expected The expected edge state probabilities for each edge. This
#' must be a matrix with the expected probability for the three edge states
#' across the columns and each edge down the rows.
#'
#' @param adjMatrix This is only required if the input to posterior is a
#' posterior probability adjacency matrix (for example, output from the BiDAG or
#' structmcmc pacakges). This matrix will be used to convert the posterior
#' probability adjacency matrix to an edge state matrix. Only the edges in
#' adjMatrix will be converted to the edge state matrix. To consider all
#' possible edges use a fully connected adjacency matrix.
#'
#' @param type A character string indicating the type of MSE to be calculated.
#' The two options are 'emse' - edge-wise MSE or 'gmse' - whole graph MSE.
#'
#' @examples
#'
#' set.seed(3)
#'
#' # Generate data from topology M2.
#' data_m2 <- simdata(graph = 'm2_ge',
#'                    N = 200,
#'                    b0 = 0,
#'                    ss = 1,
#'                    s = 1)
#'
#' # Adjacency matrix for topology M2
#' am_m2 <- matrix(c(0, 1, 1,
#'                   0, 0, 1,
#'                   0, 0, 0),
#'                 byrow = TRUE,
#'                 nrow = 3)
#'
#' # Run baycn on the data from topology M2.
#' baycn_m2 <- mhEdge(data = data_m2,
#'                    adjMatrix = am_m2,
#'                    prior = c(0.05,
#'                              0.05,
#'                              0.9),
#'                    nCPh = 0,
#'                    nGV = 0,
#'                    pmr = FALSE,
#'                    iterations = 1000,
#'                    burnIn = 0.2,
#'                    thinTo = 500,
#'                    progress = FALSE)
#'
#' # Expected probabilities for topology M2.
#' ep_m2 <- matrix(c(1, 0, 0,
#'                   0, 0, 1,
#'                   0, 1, 0),
#'                 byrow = TRUE,
#'                 ncol = 3)
#'
#' # Calculate the edgewise MSE.
#' emse_m2 <- mse(posterior = list(baycn_m2),
#'                expected = ep_m2,
#'                type = 'emse')
#'
#' \donttest{
#' # When a list with two levels is passed posterior the mean and standard
#' # deviation of the MSE is returned. Below is an example demonstrating this
#' # feature.
#'
#' # Adjacency matrix for topology M2
#' am_m2 <- matrix(c(0, 1, 1,
#'                   0, 0, 1,
#'                   0, 0, 0),
#'                 byrow = TRUE,
#'                 nrow = 3)
#'
#' # Create a list to hold multiple M2 data sets.
#' data_m2 <- vector(mode = 'list',
#'                   length = 5)
#'
#' # Create a list to hold the output from baycn.
#' baycn_m2 <- vector(mode = 'list',
#'                    length = 5)
#'
#' for (e in 1:5) {
#'
#'   # Generate data for topology M2.
#'   data_m2[[e]] <- simdata(graph = 'm2_ge',
#'                           N = 200,
#'                           b0 = 0,
#'                           ss = 1,
#'                           s = 1)
#'
#'   # Run baycn on the data simulated for topology M2.
#'   baycn_m2[[e]] <- mhEdge(data = data_m2[[e]],
#'                           adjMatrix = am_m2,
#'                           prior = c(0.05,
#'                                     0.05,
#'                                     0.9),
#'                           nCPh = 0,
#'                           nGV = 0,
#'                           pmr = FALSE,
#'                           iterations = 1000,
#'                           burnIn = 0.2,
#'                           thinTo = 500,
#'                           progress = FALSE)
#' }
#'
#' # Expected probabilities for topology M2.
#' ep_m2 <- matrix(c(1, 0, 0,
#'                   0, 0, 1,
#'                   0, 1, 0),
#'                 byrow = TRUE,
#'                 ncol = 3)
#'
#' # Calculate the edgewise MSE
#' emse_m2 <- mse(posterior = list(baycn_m2),
#'                expected = ep_m2,
#'                type = 'emse')
#'
#' }
#'
#' @export
#'
mse <- function (posterior,
                 expected,
                 adjMatrix,
                 type = 'emse') {

  # Get the number of posterior probability lists passed to mse.
  N <- length(posterior)

  # Create a matrix to hold the output from innerMSE.
  mse_list <- vector(mode = 'list',
                     length = N)

  # Loop through the elements of the posterior object.
  for (e in 1:N) {

    mse_list[[e]] <- innerMSE(posterior = posterior[[e]],
                              expected = expected,
                              adjMatrix = adjMatrix,
                              type = type)

  }

  # Combine the matrices in mse_list columnwise.
  return (do.call(cbind, mse_list))

}
