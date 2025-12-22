# Beta Borrowing for Global JSD Design
beta_borrow_jsdglobal <- function(design, n, r, weights_pair, eps_all) {
  shape_noprior <- matrix(c(r, n - r), nrow = 2, byrow = TRUE)
  shape <- matrix(c(design$shape1 + r, design$shape1 + n - r), nrow = 2,
    byrow = TRUE)
  # Compute global weight
  weight_all <- jsd_global(shape = shape, epsilon = eps_all)

  # Compute pairwise weights and multiply by global weight
  all_combs <- arrangements::combinations(r, 2) + 1
  weights_vec <- weights_pair[all_combs] * weight_all

  # Create weight k x k weight matrix
  weight_mat <- matrix(0, nrow = design$k, ncol = design$k)
  weight_mat[lower.tri(weight_mat)] <- weights_vec
  weight_mat <- weight_mat + t(weight_mat)
  diag(weight_mat) <- 1

  # Compute posterior shapes
  shape1post <- apply(weight_mat, 1, function(x) sum(shape_noprior[1, ] * x)) +
    design$shape1
  shape2post <- apply(weight_mat, 1, function(x) sum(shape_noprior[2, ] * x)) +
    design$shape2
  rbind(shape1post, shape2post)
}

# Compute global Jensen-Shannon divergence
jsd_global <- function(shape, epsilon) {
  k <- ncol(shape)
  bf <- function(x, i) stats::dbeta(x, shape[1, i], shape[2, i])
  bff <- Vectorize(function(x) sum(mapply(bf, i = 1:k,
    MoreArgs = list(x = x))) / k)
  klf <- function(x, i) bf(x, i) * log(bf(x, i) / bff(x), base = k)

  kl <- numeric(k)
  for (i in 1:k) {
    kl[i] <- stats::integrate(function(x) klf(x, i), 0, 1)$value
  }
  (1 - mean(kl))^epsilon
}

# Analyzing Results for Global JSD Design
ana_jsdglobal <- function(design, n, r, eps_all, lambda, weights_pair) {
  shape_post <- beta_borrow_jsdglobal(design = design, n = n, r = r,
    weights_pair = weights_pair, eps_all = eps_all)
  post_prob <- post_beta(shape = shape_post, p0 = design$p0)
  ifelse(post_prob >= lambda, 1, 0)
}
