args_env <- new.env()
local(envir = args_env, {
  d_m <- 3L
  d_g <- 4L
  d_b <- 2L
  n_y <- 2L
  d_z <- 0L
  d_x <- 4L
  K <- n_y * d_m

  b_ks <- seq(log(1e-1), log(10), length.out = d_b)
  m_ks <- seq(0        , 10     , length.out = d_m)
  g_ks <- seq(0        , 10     , length.out = d_g)

  omega <- c(-4.11, -1.28)
  B <- structure(c(0.95, -0.02, -0.22, -0.27, -0.45, 0.39, -0.05, -0.39),
                 .Dim = c(4L, 2L))
  Psi <- structure(c(2.64, 0.29, -0.34, 1.12, 0.14, 0.74, 0.29, 1.97,
                     -0.23, -0.1, 0.07, -0.79, -0.34, -0.23, 2.87, -0.45, 0.22, 0.12,
                     1.12, -0.1, -0.45, 1.23, 0.45, -0.32, 0.14, 0.07, 0.22, 0.45,
                     2.49, 0.09, 0.74, -0.79, 0.12, -0.32, 0.09, 2.03),
                   .Dim = c(6L, 6L))
  sig <- structure(c(0.08, 0, 0, 0.02), .Dim = c(2L, 2L))
  delta <- NULL
  alpha <- c(0.24, 0.49)
  gamma <- structure(c(0.15, -0.21, 0.24, -0.21, 0.16, -0.23, 0.24, -0.18),
                     .Dim = c(4L, 2L))

  n_obs <- 2000L
})
