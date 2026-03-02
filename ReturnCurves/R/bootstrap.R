block_bootstrap_function <- function(data, k, n){ 
  if(k < 1 | k %% 1 != 0){
    stop("The size of the blocks for the block bootstrap procedure needs to be a positive integer.")
  }
  if(k > n){
    stop("The size of blocks for the block bootstrap procedure needs to be no greater than the sample size.")
  }
  data <- as.matrix(data)
  no_blocks <- ceiling(n/k)
  n_new <- no_blocks*k
  new_data <- matrix(NA, nrow = n_new, ncol = dim(data)[2])
  indices <- 1:(n-k+1)
  start_points <- sample(x = indices, size = no_blocks, replace = T)
  for(i in 1:no_blocks){
    new_data[((i-1)*k+1):(i*k), ] = data[(start_points[i]:(start_points[i] + k - 1)), ]
  }
  return(new_data[1:n, ])
} 