# Check function for designs based on the beta-binomial distribution
validate_betabin <- function(x) {
  if ((x$k %% 1) != 0) {
    stop("k must be an integer")
  }
  if (x$k <= 1) {
    stop("k must be greater than or equal to 2")
  }
  if (x$p0 <= 0 | x$p0 >= 1) {
    stop("p0 must be between 0 and 1")
  }
  if (x$shape1 <= 0 | x$shape2 <= 0) {
    stop("shape1 and shape2 must be greater than 0")
  }
  x
}

# Calculate Posterior Probabilites of a Beta Distribution
post_beta <- function(shape, p0) {
  stats::pbeta(p0, shape1 = shape[1, ], shape2 = shape[2, ],
    lower.tail = FALSE)
}

cfun1 <- function(x, y) {
  list(
    rbind(x[[1]], y[[1]]),
    rbind(x[[2]], y[[2]]),
    rbind(x[[3]], y[[3]]),
    rbind(x[[4]], y[[4]])
  )
}

cfun2 <- function(x, y) {
  list(
    rbind(x[[1]], y[[1]]),
    rbind(x[[2]], y[[2]])
  )
}

check_scenarios <- function(scenarios, design) {
  if (!(is.matrix(scenarios) | is.data.frame(scenarios))) {
    stop("scenarios is not a matrix or a data.frame")
  }
  if (sum(duplicated(t(scenarios))) > 0) {
    stop("not all scenarios are distinct")
  }
  if (sum(apply(scenarios, 2, function(x) all(x == design$p0))) == 0) {
    stop("no null scenario")
  }
  if (nrow(scenarios) != design$k) {
    stop("scenarios doesn't have k rows")
  }
  if (!all(scenarios > 0) | !all(scenarios < 1)) {
    stop("probabilities have to be in (0, 1)")
  }
}

check_p1 <- function(design, p1, data) {
  if (is.null(p1) & is.null(data)) p1 <- rep(design$p0, design$k)
  if (any(p1 < design$p0)) {
    stop("all p1 must be greater than or equal to p0")
  }
  if ((length(p1) != design$k) & (length(p1) != 1) & (!is.null(p1))) {
    stop("p1 must either have length k or 1")
  }
  p1
}

check_params <- function(n, lambda, iter) {
  if (length(n) != 1) stop("n must have length 1")
  if ((n <= 0) | (n %% 1 != 0)) stop("n must be a positive integer")
  if (lambda <= 0 | lambda >= 1) stop("lambda must be between 0 and 1")
  if ((iter <= 0) | (iter %% 1 != 0)) stop("iter must be a positive integer")
}


