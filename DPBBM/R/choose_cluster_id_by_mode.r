# function used to get the mode for each site
#
.getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

.choose_cluster_id_by_mode <- function(param){
  
  num_iter <- length(param)
  num_site <- length(param[[num_iter]]$c)
  c_mat <- matrix(NA, nrow= num_iter-1000, ncol = num_site)
  for (i in 1:(num_iter-1000)){
    c_mat[i,] <- param[[i+1000]]$c
  }
  
  c_final <- apply(c_mat, 2, .getmode)
  return(c_final)
}