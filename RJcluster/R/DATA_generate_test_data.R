#' simulate_HD_data
#' 
#' This is simulaiton data to check performance of RJcluster. Data can be simulated for any n, P, and size of clusters. The data has two types of
#' data: noisy data and signal data. The percent of the data that is noisy is controlled by the sparsity paramater. The noisy data has two parts:
#' half of it is \eqn{N(0,1)} and half is \eqn{N(0, noise_variance)}. The signal data is divided in two as well, half of it is 
#' \eqn{N(\mu[,1], signal_variance)} and half \eqn{N(\mu[,2], signal_variance)}. 
#' 
#' The data in the paper is generated with number of clusters = 4, a balanced case of c(20, 20, 20, 20) and an unbalanced case of c(20, 20, 200, 200),
#' with p = 220 in both cases. The default is a balanced, high signal case with \eqn{\mu} as the matrix in the RJcluster paper. 
#'
#' @param size_vector A list of the size of the different clusters. (default = a balanced case of 4 clusters of size 20, c(20, 20, 20, 20))
#' @param p The number of columns in the simulated matrix (default = 220)
#' @param mu The matrix of means, of dimension length(size_vector)x2. The first column of means is for the first half informative features, 
#' the second columns of mean is for the second half of the informative features (default is described in RJcluster paper)
#' @param signal_variance Variance of the signal part of the generated data. A value of 1 indicates a high SNR, a value of 2 indicates a low SNR 
#' (default = 1)
#' @param noise_variance Variance of the noisy part of the generated data (Default = 1)
#' @param sparsity What percent of the data should be informative? A value between 0 and 1, a higher value means more data is informative
#' (default = 0.09)
#' @param seed Random seed. Change if generating multiple simulation datasets (default = 1234)
#'
#'@return Returns simulation data for X and Y values\tabular{ll}{
#'    \code{X} \tab Matrix of dimension sum(size_vector)xp \cr
#'    \tab \cr
#'    \code{Y} \tab Vector of class labels of length \eqn{\sum(size_vector)}, with unique values of 1:length(size_vector) \cr
#' }
#' @export
#'
#' @examples
#' data = simulate_HD_data()
#' X = data$X
#' Y = data$X
#' print(head(X))
simulate_HD_data = function(size_vector = c(20, 20, 20, 20), p = 220, 
                            mu = matrix(c(1.5, 2.5, 0, 1.5, 0, -1.5, -2.5, -1.5), ncol = 2, byrow = TRUE),
                            signal_variance = 1, noise_variance = 1, sparsity = 0.09, seed = 1234)
{
  ######### Bayesian Clustering Algorithm ############ 
  K = length(size_vector)
  if (K != nrow(mu))
  {
    stop("The number of rows of mu must equal clusters (the length of size_vector)")
  }
  n = size_vector        # Equal cluster size settings

  # generate sparsity and matrix
  set.seed(seed)
  X = matrix(rnorm(sum(n)*p, 0, 1), nrow = sum(n), ncol = p, byrow = TRUE)
  
  #Cluster 1: N(2.5, sigma)(1-10), N(1.5, sigma)(11-20) 
  
  # loop over the number of provided clusters
  start_value = floor(floor(sparsity*p)/2)
  
  for (i in 1:K)
  {
    # loop over the informative features 
    # divide sparisty into two groups 
    row_start = ifelse(i == 1, 0, sum(n[1:(i - 1)]))
    X[(row_start + 1):(row_start + n[i]), 1:start_value] = rnorm(n[i]*start_value, mu[i, 1], signal_variance)
    X[(row_start + 1):(row_start + n[i]), (1 + start_value):(2*start_value)] = rnorm(n[i]*start_value, mu[i, 2], signal_variance)
  }
  
  # now divide the noise into two points 
  num_new_noise = floor((p - (2*start_value))/2)
  n_temp = length(c(2*start_value + 1):num_new_noise)
  X[, c(2*start_value + 1):num_new_noise] = matrix(rnorm(n_temp*sum(n), 0, noise_variance), nrow = nrow(X))
  
  # X[1:n[1],1:10] = rnorm(n[1]*10, 2.5, sigma)
  # X[1:n[1],(1 + 10):(10 + 10)] = rnorm(n[1]*10, 1.5, sigma)
  # 
  # #Cluster 2: N(0, sigma)(1-10), N(1.5, sigma) (11-20)
  # X[(n[1] + 1):(n[1] + n[2]),1:10]  = rnorm(n[2]*10, 0, sigma)
  # X[(n[1] + 1):(n[1] + n[2]),(1 + 10):(10 + 10)] = rnorm(n[2]*10, 1.5, sigma)
  # 
  # #Cluster 3: N(0, sigma)(1-10), N(-1.5,sigma)(11-20)
  # X[(n[1] + n[2] + 1):(n[1] + n[2] + n[3]),1:10] = rnorm(n[3]*10, 0, sigma)
  # X[(n[1] + n[2] + 1):(n[1] + n[2] + n[3]),(1 + 10):(10 + 10)] = rnorm(n[3]*10, -1.5, sigma)
  # 
  # #Cluster 4: N(-2.5,sigma)(1-10), N(-1.5, sigma)(11-20)
  # X[(n[1] + n[2] + n[3] + 1):(n[1] + n[2] + n[3] + n[4]),1:10] = rnorm(n[4]*10, -2.5, sigma)
  # X[(n[1] + n[2] + n[3] + 1):(n[1] + n[2] + n[3] + n[4]),(1 + 10):(10 + 10)] = rnorm(n[4]*10, -1.5, sigma)
  
  Y = c()
  for (i in 1:K)
  {
    Y = c(Y, rep(i, n[i]))
  }

  return(list(X = X, Y = Y))
}

