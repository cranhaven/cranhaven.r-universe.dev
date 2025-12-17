#' @name DIPC
#' @title Distributed Incremental Principal Component Analysis (DIPC)
#' @description Apply IPC in a distributed manner across K nodes.
#' @param data Matrix of input data (n × p).
#' @param m Number of principal components.
#' @param eta Proportion of  initial batch to  total data  within each node.
#' @param K Number of nodes (distributed splits).
#' @return List with per-node results and aggregated averages.
#' @examples
#' library(LFM)
#' data_from_package <- Wine
#' data_a <- Wine
#' m=5
#' eta=0.8
#' K=2
#' results <- DIPC(data_a, m, eta, K)
#' @export
DIPC <- function(data, m, eta, K) {
  Ai_list <- list()
  Di_list <- list()
  n <- nrow(data)
  p <- ncol(data)
  chunk <- floor(n / K)
  if (chunk * K < n) warning("Rows not evenly divisible; trailing data ignored.")
  for (k in 1:K) {
    ## 1. 节点数据
    idx  <- ((k - 1) * chunk + 1):(k * chunk)
    Xk   <- scale(data[idx, ])             
    nk   <- nrow(Xk)
    n0   <- max(round(eta * nk), m)          
    Xbar <- colMeans(Xk[1:n0, ])
    S0   <- cov(Xk[1:n0, ])             
    eig  <- eigen(S0)
    lam  <- eig$values[1:m]
    V    <- eig$vectors[, 1:m, drop = FALSE] 
    
    ## 2. 在线更新
    for (i in (n0 + 1):nk) {
      xi <- Xk[i, ] - Xbar
      g  <- crossprod(V, xi)                 # V^T xi
      T  <- matrix(0, m + 1, m + 1)
      T[1:m, 1:m]       <- ((i - 1)/i) * diag(lam) + ((i - 1)^2/i^3) * tcrossprod(g)
      hmao <- norm(xi - V %*% g, "2")
      gamma <- as.numeric(crossprod(xi - V %*% g, xi)) / hmao
      T[1:m, m + 1] <- ((i - 1)^2/i^3) * gamma * g
      T[m + 1, ]    <- c(((i - 1)^2/i^3) * gamma * g,
                         ((i - 1)^2/i^3) * gamma^2)
      
      eigT <- eigen(T)
      lam  <- eigT$values[1:m]
      V    <- cbind(V, (xi - V %*% g)/hmao) %*% eigT$vectors[, 1:m, drop = FALSE]
      Xbar <- ((i - 1)/i) * Xbar + (1/i) * Xk[i, ]
    }
    
    # Store results for the i-th node
    Ai <- sqrt(lam) * V
    Di <- diag(cov(Xk) - tcrossprod(Ai))
    Ai_list[[k]] <- Ai
    Di_list[[k]]  <- Di
  }
  #Aggregate results across nodes (element-wise average)
  DAi <- Reduce(`+`, Ai_list) / K
  DDi <- Reduce(`+`, Di_list)  / K
  # Return per-node results and aggregated results
  return(list(node_results = list(Ai = Ai_list, Di = Di_list),
              aggregated   = list(DAi = DAi, DDi = DDi)))
}