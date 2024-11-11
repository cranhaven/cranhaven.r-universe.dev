# U-centered matrix
u.center <- function(X) {
  if (is.matrix(X)) {
    n <- dim(X)[1]
    if (isSymmetric(X)) {
      A <- X 
    } else { 
      A <- as.matrix(dist(X))
    }
  } else {
    n <- length(X)
    A <- as.matrix(dist(X))
  }
  
  R <- rowSums(A)
  C <- colSums(A)
  S <- sum(A)
  r <- matrix(rep(R, n), n, n) / (n - 2)
  c <- t(matrix(rep(C, n), n, n)) / (n - 2)
  t <- matrix(S / (n - 1) / (n - 2), n, n)
  UA <- A - r - c + t
  diag(UA) <- 0

  return(UA)
}

# inner product of U-centered matrix
u.inner <- function(X, Y){
  n <- dim(X)[1]
  ip <- sum(X * Y) / n / (n - 3) 

  return(ip)
}

# double-centered matrix
d.center <- function(X) {
  if (is.matrix(X)) {
    n <- dim(X)[1]
    if (isSymmetric(X)) {
      A <- X 
    } else {
      A <- as.matrix(dist(X))      
    }
  } else {
    n <- length(X)
    A <- as.matrix(dist(X))
  }

  R <- rowSums(A)
  C <- colSums(A)
  S <- sum(A)
  r <- matrix(rep(R, n), n, n) / n
  c <- t(matrix(rep(C, n), n, n)) / n
  t <- matrix(S / n^2, n, n)
  DA <- A - r - c + t

  return(DA)
}

# inner product of double-centered matrix
d.inner <- function(X, Y) {
  n <- dim(X)[1]
  ip <- sum(X * Y) / n / n 
  
  return(ip)
}

# R* function
r.star <- function(X, Y) {
  A <- u.center(X)
  B <- u.center(Y)
  n <- dim(A)[1]
  inner <- sum(A * B) / n / (n - 3) 
  norm1 <- sqrt(sum(A * A) / n / (n - 3))
  norm2 <- sqrt(sum(B * B) / n / (n - 3))  
  r <- inner / norm1 / norm2
  
  return(r)
}
