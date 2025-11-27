# Jiggle a point in the Euclidean space of dimension m: Create n uniformly jiggled points on the sphere with m-1 dimension

Jiggle <- function(n, m, method=c('random', 'coordUnif')) {
  
  if (m <= 1) {
    stop('The Euclidean space must have at least dimension 2')
  }
  method <- match.arg(method)

  if (method == 'random') {
    X <- matrix(rnorm(n * m), n, m)
    X <- t(apply(X, 1, Normalize))
    return(X)
  } else if (method == 'coordUnif') {
    # S^d = exp_p{S^{d-1} \times [0, \pi]}
    # Apply the algorithm recursively

    each <- n ^ (1 / (m - 1)) / 2
    tryEach <- unique(c(floor(each) - 1, floor(each), ceiling(each)))
    tryEach <- tryEach[tryEach > 0]
    for (nEachDim in tryEach) { # number of points on each dimension

    # pts <- seq(-pi, pi, length.out=nEachDim + 1)
    pts <- matrix(c(-1, 1))
    for (i in seq_len(m-1)) { # i-dimensional sphere
      rad <- seq(0, pi, length.out=nEachDim + 2)
      rad <- rad[-c(1, length(rad))]
      pts <- cbind(do.call(rbind, lapply(rad, function(r) r * pts)), 
                   0)
      pts <- rbind(pts, 
                   matrix(c(rep(0, i + 1),
                            c(pi, rep(0, i - 1), 0)),
                          nrow=2, byrow=TRUE))
      northPole <- c(rep(0, i), 1)
      pts <- t(manifold::rieExp(manifold::createM('Sphere'), northPole, t(pts))) # on i-dimensional sphere
    }
      # browser()
    if (nrow(pts) >= n) {
      return(pts[seq_len(n), , drop=FALSE])
    }
    }

  }
}
