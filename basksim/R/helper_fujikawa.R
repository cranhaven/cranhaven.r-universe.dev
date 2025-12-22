# Beta Borrowing for Fujikawa's Design
beta_borrow_fujikawa <- function(design, n, r, weights) {
  shape <- matrix(c(design$shape1 + r, design$shape2 + n - r), nrow = 2,
    byrow = TRUE)
  all_combs <- arrangements::combinations(r, 2) + 1
  weights_vec <- weights[all_combs]
  weight_mat <- matrix(0, nrow = design$k, ncol = design$k)
  weight_mat[lower.tri(weight_mat)] <- weights_vec
  weight_mat <- weight_mat + t(weight_mat)
  diag(weight_mat) <- 1

  shape1post <- apply(weight_mat, 1, function(x) sum(shape[1, ] * x))
  shape2post <- apply(weight_mat, 1, function(x) sum(shape[2, ] * x))
  rbind(shape1post, shape2post)
}

# Analyzing Results for Fujikawa's Design
ana_fujikawa <- function(design, n, r, lambda, weights) {
  shape_post <- beta_borrow_fujikawa(design = design, n = n, r = r,
    weights = weights)
  post_prob <- post_beta(shape = shape_post, p0 = design$p0)
  ifelse(post_prob >= lambda, 1, 0)
}
