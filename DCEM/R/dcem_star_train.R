#' dcem_star_train: Part of DCEM package.
#'
#' Implements the improved EM* ([1], [2]) algorithm. EM* avoids revisiting all but high
#' expressive data via structure based data segregation thus resulting in significant speed gain.
#' It calls the \code{\link{dcem_star_cluster_uv}} routine internally (univariate data) and
#' \code{\link{dcem_star_cluster_mv}} for (multivariate data).
#'
#' @param data (dataframe): The dataframe containing the data. See \code{\link{trim_data}} for
#' cleaning the data.
#'
#' @param iteration_count (numeric): The number of iterations for which the algorithm should run, if the
#' convergence is not achieved then the algorithm stops and exit. \strong{Default: 200}.
#'
#' @param num_clusters (numeric): The number of clusters. Default: \strong{2}
#'
#' @param seed_meu (matrix): The user specified set of meu to use as initial centroids. Default: \strong{None}
#'
#' @param seeding (string): The initialization scheme ('rand', 'improved'). Default: \strong{rand}
#'
#' @return
#'         A list of objects. This list contains parameters associated with the Gaussian(s)
#'         (posterior probabilities, meu, sigma and priors). The
#'         parameters can be accessed as follows where sample_out is the list containing
#'         the output:
#'
#'\enumerate{
#'         \item (1) Posterior Probabilities: \strong{sample_out$prob}
#'         A matrix of posterior-probabilities.
#'
#'         \item (2) Meu(s): \strong{sample_out$meu}
#'
#'         For multivariate data: It is a matrix of meu(s). Each row in
#'         the  matrix corresponds to one mean.
#'
#'         For univariate data: It is a vector of meu(s). Each element of the vector
#'         corresponds to one meu.
#'
#'         \item (3) Co-variance matrices: \strong{sample_out$sigma}
#'
#'         For multivariate data: List of co-variance matrices.
#'
#'         Standard-deviation: \strong{sample_out$sigma}
#'
#'         For univariate data: Vector of standard deviation.
#'
#'         \item (4) Priors: \strong{sample_out$prior}
#'         A vector of priors.
#'
#'        \item (5) Membership: \strong{sample_out$membership}: A dataframe of
#'         cluster membership for data. Columns numbers are data indices and values
#'         are the assigned clusters.
#'         }
#'
#' @usage
#' dcem_star_train(data, iteration_count,  num_clusters, seed_meu, seeding)
#'
#' @references
#' Parichit Sharma, Hasan Kurban, Mehmet Dalkilic DCEM: An R package for clustering big data via
#' data-centric modification of Expectation Maximization, SoftwareX, 17, 100944 URL
#' https://doi.org/10.1016/j.softx.2021.100944
#'
#' @examples
#'# Simulating a mixture of univariate samples from three distributions
#'# with mean as 20, 70 and 100 and standard deviation as 10, 100 and 40 respectively.
#' sample_uv_data = as.data.frame(c(rnorm(100, 20, 5), rnorm(70, 70, 1), rnorm(50, 100, 2)))
#'
#'# Randomly shuffle the samples.
#' sample_uv_data = as.data.frame(sample_uv_data[sample(nrow(sample_uv_data)),])
#'
#'# Calling the dcem_star_train() function on the simulated data with iteration count of 1000
#'# and random seeding respectively.
#' sample_uv_out = dcem_star_train(sample_uv_data, num_clusters = 3, iteration_count = 100)
#'
#'# Simulating a mixture of multivariate samples from 2 gaussian distributions.
#' sample_mv_data = as.data.frame(rbind(MASS::mvrnorm(n=2, rep(2,5), Sigma = diag(5)),
#' MASS::mvrnorm(n=5, rep(14,5), Sigma = diag(5))))
#'
#'# Calling the dcem_star_train() function on the simulated data with iteration count of 100 and
#'# random seeding method respectively.
#' sample_mv_out = dcem_star_train(sample_mv_data, iteration_count = 100, num_clusters=2)
#'
#'# Access the output
#' sample_mv_out$meu
#' sample_mv_out$sigma
#' sample_mv_out$prior
#' sample_mv_out$prob
#' print(sample_mv_out$membership)
#'
#' @export

dcem_star_train <-
  function(data,
           iteration_count,
           num_clusters, seed_meu, seeding) {

    if (missing(iteration_count)) {
      iteration_count = 200

      print("Using default value for iteration count = 200.")
    }
    else{
      print(paste("Specified iterations = ", iteration_count))
    }

    if (missing(num_clusters)) {
      num_clusters = 2
      print("Using default value for number of clusters = 2.")
    }
    else{
      print(paste("Specified number of  clusters = ", num_clusters))
    }

    if (missing(seeding) || seeding == "rand") {
      seeding = "rand"
      print("Using the random initialisation scheme.")
    }
    else{
      seeding = seeding
      print("Using the improved Kmeans++ initialisation scheme.")
    }

    # Remove any missing data
    data <- apply(data, 2, as.numeric)
    data[is.na(data)] <- NULL

    # Safe copy the data for operations
    test_data <- as.matrix(data)
    num_data <- nrow(test_data)
    valid_columns <- ncol(test_data)

    # Variable to store the output
    emstar_out = list()

    # Call clustering routine for multivariate data
    # Get the initial values for meu, sigma and priors
    if (valid_columns >= 2) {

      if (missing(seed_meu)){
      if (seeding == "rand"){
        meu = meu_mv(test_data, num_clusters)
      }
      else if(seeding == "improved"){
        meu = meu_mv_impr(test_data, num_clusters)
      }
      }
      else{
        meu <- seed_meu
      }
      sigma = sigma_mv(num_clusters, valid_columns)
      priors = get_priors(num_clusters)
      emstar_out = dcem_star_cluster_mv(
        test_data,
        meu,
        sigma,
        priors,
        num_clusters,
        iteration_count,
        num_data)
    }

    # Call clustering routine for univariate data
    # Get the initial values for meu, sigma and priors
    if (valid_columns < 2) {
      if(seeding=="rand"){
        meu = meu_uv(test_data, num_clusters)
      }
      else if(seeding == "improved"){
        meu = meu_uv_impr(test_data, num_clusters)
      }
      sigma = sigma_uv(test_data, num_clusters)
      priors = get_priors(num_clusters)
      emstar_out = dcem_star_cluster_uv(
        test_data,
        meu,
        sigma,
        priors,
        num_clusters,
        num_data,
        iteration_count)
    }

    return(emstar_out)
  }
