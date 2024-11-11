asym_obj <- function(S) {
  S <- as.matrix(S)
  return(mdm(X = S, type = 'asym_dcov')$stat)
}

sym_obj <- function(S) {
  S <- as.matrix(S)
  return(mdm(X = S, type = 'sym_dcov')$stat)
}

comp_obj <- function(S) {
  S <- as.matrix(S)
  return(mdm(X = S, type = 'comp_simp')$stat)
}

dhsic_obj <- function(S) {
  S <- as.matrix(S)
  return(dhsic(X = S, matrix.input = TRUE)$dHSIC)
}

sqrt_inv <- function(A) {
  evd <- eigen(A, symmetric = TRUE)
  return(diag(1 / sqrt(evd$values)) %*% t(evd$vectors))
}

latin_hc_samp <- function(d, n) { 
  len_theta <- d * (d - 1) / 2
  theta_mat <- matrix(0, len_theta, n)

  # sample values from [0, 2 * pi] for the first d - 1 angles 
  bin_list1 <- seq(0, 2 * pi, length.out = n + 1)
  # sample values from [0, pi] for the other angles
  bin_list2 <- seq(0, pi, length.out = n + 1)

  if (d >= 2) {
    for (i in 1:(d - 1)) { 
      theta_mat[i, ] <- runif(n, min = bin_list1[1:n], max = bin_list1[2:(n + 1)])[sample(1:n)]
    } 
  }
  
  if (len_theta >= d) {
    for (i in d:len_theta) {
      theta_mat[i, ] <- runif(n, min = bin_list2[1:n], max = bin_list2[2:(n + 1)])[sample(1:n)]
    }
  }

  theta_list <- list()
  for (i in 1:n) {
    theta_list[[i]] <- theta_mat[, i]
  }

  return(list(l = theta_list, m = t(theta_mat)))
}

# Given a theta, return a d x d Givens rotation matrix
# When d = 2, i < j, G = (a -b)
#                        (b  a)
givens_rot_mat <- function(theta, d, index) {
  a <- cos(theta)
  b <- sin(theta)

  i <- index[1]
  j <- index[2]

  G <- diag(d)
  G[i, i] <-  a
  G[j, j] <-  a
  G[i, j] <- -b
  G[j, i] <-  b

  return(G)
}

# Given a theta, return a d x d Givens rotation matrix
# W = Q_d-1,d %*% ... %*% Q_2,d %*% ... %*% Q_2,3 %*% Q_1,d %*% ... %*% Q_1,2 
theta_to_W <- function(theta) {
  d <- (sqrt(8 * length(theta) + 1) + 1) / 2
  if (d != floor(d)) {
    stop("theta must have length d * (d - 1) / 2.")
  }

  W <- diag(d)
  index <- 1
  for (i in 1:(d - 1)) {
    for (j in (i + 1):d) {
      Q_ij <- givens_rot_mat(theta[index], d, c(i, j))
      W <- Q_ij %*% W
      index <- index + 1
    }
  }
  # for (j in 1:(d - 1)) {
  #   for (i in (j + 1):d) {
  #     Q_ij <- givens_rot_mat(theta[index], d, c(i, j))
  #     W <- Q_ij %*% W 
  #     index <- index + 1
  #   }
  # }
  
  return(W)
}
  
