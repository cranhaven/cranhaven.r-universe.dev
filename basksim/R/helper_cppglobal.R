# Beta Borrowing for Global CPP Design
beta_borrow_cppglobal <- function(design, n, r, weights_pair, epsilon) {
  shape_noprior <- matrix(c(r, n - r), nrow = 2, byrow = TRUE)
  weight_all <- diff_all(n = n, r = r, epsilon = epsilon)

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

# Global Weights for Global CPP Design
diff_all <- function(n, r, epsilon) {
  rr <- r / n
  rs <- sort(rr)
  d <- diff(rs)
  (1 - sum(d) * 10^(-sum((d - 1 / length(d))^2)))^epsilon
}

# Analyzing Results for CPP Design
ana_cppglobal <- function(design, n, r, lambda, weights_pair, epsilon) {
  shape_post <- beta_borrow_cppglobal(design = design, n = n, r = r,
    weights_pair = weights_pair, epsilon = epsilon)
  post_prob <- post_beta(shape = shape_post, p0 = design$p0)
  ifelse(post_prob >= lambda, 1, 0)
}
