#' dcem_predict: Part of DCEM package.
#'
#' Predict the cluster membership of test data based on the learned parameters i.e, output from
#' \code{\link{dcem_train}} or \code{\link{dcem_star_train}}.
#'
#' @param param_list (list): List of distribution parameters. The list contains the learned
#' parameteres of the distribution.
#'
#' @param data (vector or dataframe): A vector of data for univariate data. A dataframe (rows represent
#' the data and columns represent the features) for multivariate data.
#'
#' @return
#'         A list containing the cluster membership for the test data.
#'
#' @usage
#' dcem_predict(param_list, data)
#'
#' @examples
#'# Simulating a mixture of univariate samples from three distributions
#'# with meu as 20, 70 and 100 and standard deviation as 10, 100 and 40 respectively.
#'sample_uv_data = as.data.frame(c(rnorm(100, 20, 5), rnorm(70, 70, 1), rnorm(50, 100, 2)))
#'
#'# Select first few points from each distribution as test data
#'test_data = as.vector(sample_uv_data[c(1:5, 101:105, 171:175),])
#'
#'# Remove the test data from the training set
#'sample_uv_data = as.data.frame(sample_uv_data[-c(1:5, 101:105, 171:175), ])
#'
#'# Randomly shuffle the samples.
#'sample_uv_data = as.data.frame(sample_uv_data[sample(nrow(sample_uv_data)),])
#'
#'# Calling the dcem_train() function on the simulated data with threshold of
#'# 0.000001, iteration count of 1000 and random seeding respectively.
#'sample_uv_out = dcem_train(sample_uv_data, num_clusters = 3, iteration_count = 100,
#'threshold = 0.001)
#'
#'# Predict the membership for test data
#'test_data_membership <- dcem_predict(sample_uv_out, test_data)
#'
#'# Access the output
#' print(test_data_membership)
#'
#' @references
#' Parichit Sharma, Hasan Kurban, Mehmet Dalkilic DCEM: An R package for clustering big data via
#' data-centric modification of Expectation Maximization, SoftwareX, 17, 100944 URL
#' https://doi.org/10.1016/j.softx.2021.100944
#'
#' @export

dcem_predict <- function(param_list, data){

  if (missing(param_list)){
    print("List of cluster parameters not supplied.")
    stop("Exiting: use ?dcem_predict to see how to predict cluster membership for new data.")
  }

  meu = param_list$meu
  sigma = param_list$sigma
  prior = param_list$prior
  num_clusters = nrow(param_list$prob)

  num_rows = 0

  if (typeof(data) == "double"){
    num_rows = length(data)
  }
  else if (typeof(data) == "list"){
    num_rows = nrow(data)
  }

  predict_liklihood = matrix(0, nrow = num_clusters, ncol = num_rows)

  # Check if the data and parametrs correspond to the same number of dimensions
  if ( (is.vector(data)  == TRUE) && (!is.null(ncol(meu)))){
  print("Mismatching number of dimensions between data and distribution parameters.")
  print("The data is univariate but the parameters correspond to a multivariate distribution.")
  stop("Exiting: use ?dcem_predict to see how to predict cluster membership for new data.")
  }

  else if(is.data.frame(data)){
    if (ncol(data) != ncol(meu)){
      print("Mismatching number of dimensions between data and distribution parameters.")
      stop("Exiting: use ?dcem_predict to see how to predict cluster membership for new data.")
    }
  }

  tolerance <- .Machine$double.eps

  # Get liklihood for univariate data
  if (is.null(ncol(data))){
    predict_liklihood = expectation_uv(data, predict_liklihood, meu, sigma, prior, num_clusters, tolerance)
  }

  # Get liklihood for multivariate data
  else if (ncol(data) > 1){
    predict_liklihood = expectation_mv(data, predict_liklihood, meu, sigma, prior, num_clusters, tolerance)
  }

  # Declare a datadrame to store the data membership values.
  membership = data.frame()

  # Predict the membership by maximum liklihood
  membership = rbind(membership, apply(predict_liklihood, 2, which.max))
  colnames(membership) <- seq(1:num_rows)

  return(membership)
}
