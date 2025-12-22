ana_mmlglobal <- function(design, n, r, lambda) {
  shape_post <- weight_mmlglobal(design = design, n = n, r = r)
  post_prob <- post_beta(shape = shape_post, p0 = design$p0)
  ifelse(post_prob >= lambda, 1, 0)
}

weight_mmlglobal <- function(design, n, r) {
  shape1_post <- shape2_post <- numeric(design$k)

  for (i in 1:design$k) {
    f <- function(delta) -extraDistr::dbbinom(
      x = r[i],
      size = n,
      alpha = design$shape1 + sum(delta * r[-i]),
      beta = design$shape2 + sum(delta * (n - r[-i]))
    )

    l <- stats::optim(rep(0.5, design$k - 1), fn = f,
      lower = rep(0, design$k - 1), upper = rep(1, design$k - 1),
      method = "L-BFGS-B")$par

    shape1_post[i] <- design$shape1 + r[i] + sum(l * r[-i])
    shape2_post[i] <- design$shape2 + (n - r[i]) + sum(l * (n - r[-i]))
  }

  matrix(c(shape1_post, shape2_post), nrow = 2, byrow = TRUE)
}
