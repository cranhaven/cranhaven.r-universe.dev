#' The distributed stochastic approximation principal component for handling online data sets with highly correlated data across multiple nodes.
#'
#' @param data is a highly correlated online data set
#' @param m is the number of principal component 
#' @param eta is the proportion of online data to total data
#' @param n1 is the length of each data subset
#' @param K is the number of nodes
#'
#' @return Asa, Dsa (lists containing results from each node)
#' @export
#'
#' @examples
#' library(LFM)
#' data_from_package <- Wine
#' data_a <- Wine
#' DSAPC(data=data_a, m=3, eta=0.8, n1=128, K=2)
DSAPC <- function(data, m, eta, n1, K) {
  n <- nrow(data)
  p <- ncol(data)
  
  Asa_list <- list()
  Dsa_list <- list()
  
  for (i in 1:K) {
    L <- matrix(rep(0, K * n1), ncol = n1)
    R <- matrix(0, n1, n)
    L[i, ] <- sample(1:n, n1, replace = FALSE)
    r <- matrix(c(1:n1, L[i, ]), ncol = n1, byrow = TRUE)
    R[t(r)] <- 1
    
    X_subset <- R %*% as.matrix(data)
    
    sapc_result <- SAPC(data = X_subset, m = m, eta = eta)
    
    Asa_list[[i]] <- sapc_result$Asa
    Dsa_list[[i]] <- sapc_result$Dsa
  }
  
  return(list(Asa = Asa_list, Dsa = Dsa_list))
}