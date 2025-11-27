utils::globalVariables("i")

# data: Each row corresponds to a data point
# distance: distance(x, Y) gives the distance of each row of x to all rows of Y
# ...: passed to distance
PairwiseDistance <- function(data, distance, PARALLEL=FALSE, ...) {
  n <- nrow(data)
  p <- ncol(data)
  mat <- matrix(0, n, n)
  ff <- function(i) {
    # browser()
    # X <- matrix(data[i, ], n - i, p, byrow=TRUE)
    X <- data[i, ]
    Y <- data[seq(i + 1, n), , drop=FALSE]
    distance(X, Y, ...)
  }
  if (!PARALLEL) {
    res <- lapply(seq_len(n - 1), ff)
  } else if (PARALLEL) {
    `%dopar%` <- foreach::`%dopar%`
    res <- foreach::foreach(i = seq_len(n - 1)) %dopar% {
      ff(i)
    }
  }
  res <- unlist(res)
  mat[lower.tri(mat)] <- res
  as.dist(mat)
}


# Pairwise distances of all rows in data1 and all rows in data2.
# Returns a matrix. The rows corresponds to data1 and columns data2.
PairwiseDistance2 <- function(data1, data2, distance, PARALLEL=FALSE, ...) {

  if (!is.matrix(data1) || !is.matrix(data2)) {
    stop('`data1` and `data2` must be matrices')
  }

  n1 <- nrow(data1)
  n2 <- nrow(data2)
  p <- ncol(data1)
  ff <- function(i) {
    # browser()
    X <- data1[i, ]
    Y <- data2
    distance(X, Y, ...)
  }
  if (!PARALLEL) {
    res <- vapply(seq_len(n1), ff, rep(1, n2))
  } else {
    `%dopar%` <- foreach::`%dopar%`
    res <- foreach::foreach(i = seq_len(n1)) %dopar% {
      ff(i)    
    } 
    res <- simplify2array(res)
  }

  if (!is.matrix(res)) {
    matrix(res)
  } else {
    t(res)
  }
}


# Calculate Lp distance
# X and Y are either vectors or matrices. If X is a vector and Y a matrix, then X will be replicated to match the dimension of Y.
Lp <- function(X, Y, p=2) {
  if (!is.matrix(X)) {
    X <- matrix(X, nrow=1)
  }
  if (!is.matrix(Y)) {
    Y <- matrix(Y, nrow=1)
  }
  if (nrow(X) == 1) {
    X <- matrix(X, nrow(Y), ncol(Y), byrow=TRUE)
  }

  if (1 <= p && p < Inf) {
    res <- rowSums(abs(X - Y)^p) ^ (1 / p)
  } else if (p == Inf) {
    res <- apply(abs(X - Y), 1, max)
  } else {
    stop('p not supported')
  }

  res
}

